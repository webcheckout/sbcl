/*
 * Extension to GENCGC which provides for pages of objects
 * that are static in placement but subject to reclamation.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

/*
 * TODO:
 *  1. Space accounting (GET-BYTES-CONSED etc)
 *  2. Heuristic for auto-trigger. (Can't yet because no space accounting)
 *     Currently happens with regular GC trigger mechanism.
 *  3. Specify space size on startup
 */

// Work around a bug in some llvm/clang versions affecting the memcpy
// call in defrag_immobile_space:
//
// When compiled with _FORTIFY_SOURCE nonzero, as seems to be the
// default, memcpy is a macro that expands to
// __builtin_memcpy_chk(dst, source, size, __builtin_object_size(...)).
//
// Now usually if the compiler knows that it does not know
// __builtin_object_size for the source of the copy, the
// __builtin_memcpy_chk call becomes plain old memcpy. But in the
// buggy case, the compiler is convinced that it does know the
// size. This shows up clearly in the disassembly, where the library
// routine receives a source size that was erroneously determined to
// be a compile-time constant 0. Thus the assertion failure is that
// you are reading from a range with 0 bytes in it.
//
// Defining _FORTIFY_LEVEL 0 disables the above macro and thus works
// around the problem. Since it is unclear which clang versions are
// affected, only apply the workaround for the known-bad version.
#if (defined(__clang__) && (__clang_major__ == 6) && (__clang_minor__ == 0))
#define _FORTIFY_SOURCE 0
#endif

#include "gc.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "genesis/gc-tables.h"
#include "genesis/cons.h"
#include "genesis/vector.h"
#include "genesis/layout.h"
#include "forwarding-ptr.h"
#include "pseudo-atomic.h"
#include "var-io.h"
#include "immobile-space.h"
#include "unaligned.h"
#include "code.h"

#include <stdlib.h>
#include <stdio.h>

#define WORDS_PER_PAGE ((int)IMMOBILE_CARD_BYTES/N_WORD_BYTES)
#define DOUBLEWORDS_PER_PAGE (WORDS_PER_PAGE/2)

// In case of problems while debugging, this is selectable.
#define DEFRAGMENT_FIXEDOBJ_SUBSPACE 1

#undef DEBUG
#undef VERIFY_PAGE_GENS

#ifdef DEBUG
#  define dprintf(arg) fprintf arg
FILE * logfile;
#else
#  define dprintf(arg)
#endif

static void defrag_immobile_space(boolean verbose);

uword_t FIXEDOBJ_SPACE_START, VARYOBJ_SPACE_START;
uword_t immobile_space_lower_bound, immobile_space_max_offset;
uword_t immobile_range_1_max_offset, immobile_range_2_min_offset;
unsigned int varyobj_space_size = VARYOBJ_SPACE_SIZE;

// This table is for objects fixed in size, as opposed to variable-sized.
// (Immobile objects are naturally fixed in placement)
struct fixedobj_page *fixedobj_pages;
lispobj* immobile_scav_queue;
int immobile_scav_queue_head;
// Number of items enqueued; can exceed QCAPACITY on overflow.
// If overflowed, the queue is unusable until reset.
unsigned int immobile_scav_queue_count;
#define QCAPACITY 1024

#define gens attr.parts.gens_

// These are the high 2 bits of 'flags'
#define WRITE_PROTECT         0x80
#define WRITE_PROTECT_CLEARED 0x40

// Packing and unpacking attributes
// the low two flag bits are for write-protect status
#define MAKE_ATTR(spacing,size,flags) (((spacing)<<8)|((size)<<16)|flags)
#define OBJ_SPACING(attr) ((attr>>8) & 0xFF)

// Ignore the write-protect bits and the generations when comparing attributes
#define ATTRIBUTES_MATCH_P(page_attr,specified_attr) \
  ((page_attr & 0xFFFF3F) == specified_attr)
#define SET_WP_FLAG(index,flag) \
  fixedobj_pages[index].attr.parts.flags = (fixedobj_pages[index].attr.parts.flags & 0x3F) | flag

#define set_page_full(i) fixedobj_pages[i].free_index = IMMOBILE_CARD_BYTES
#define page_full_p(i) (fixedobj_pages[i].free_index >= (int)IMMOBILE_CARD_BYTES)
#define fixedobj_page_wp(i) (fixedobj_pages[i].attr.parts.flags & WRITE_PROTECT)

/* Compute the last (potential) object on a fixed-size object page
 * starting at 'base' and stepping by 'spacing_bytes' bytes */
static inline lispobj* compute_fixedobj_limit(void* base, int spacing_bytes) {
    return (lispobj*)((char*)base + IMMOBILE_CARD_BYTES - spacing_bytes);
}
#define NEXT_FIXEDOBJ(where, spacing_bytes) \
    (where = (lispobj*)((char*)where + spacing_bytes))

/// Variable-length pages:

// Array of inverted write-protect flags, 1 bit per page.
unsigned int* varyobj_page_touched_bits;
static int n_bitmap_elts; // length of array measured in 'int's

boolean immobile_card_protected_p(void* addr)
{
    low_page_index_t page;
    page = find_varyobj_page_index(addr);
    if (page >= 0) return !((varyobj_page_touched_bits[page/32] >> (page&31)) & 1);
    page = find_fixedobj_page_index(addr);
    if (page >= 0) return fixedobj_page_wp(page);
    lose("immobile_card_protected_p(%p)", addr);
}

struct varyobj_page *varyobj_pages;
// Holes to be stuffed back into the managed free list.
lispobj varyobj_holes;

#define varyobj_page_touched(x) ((varyobj_page_touched_bits[x/32] >> (x&31)) & 1)

#ifdef VERIFY_PAGE_GENS
void check_fixedobj_page(low_page_index_t, generation_index_t, generation_index_t);
void check_varyobj_pages();
#endif

//// Variable-length utilities

/* Return the generation mask for objects headers on 'page_index'
   including at most one object that starts before the page but ends on
   or after it.
   If the scan start is within the page, i.e. less than DOUBLEWORDS_PER_PAGE
   (note that the scan start is measured relative to the page end) then
   we don't need to OR in the generation byte from an extra object,
   as all headers on the page are accounted for in the page generation mask.
   Also an empty page (where scan start is zero) avoids looking
   at the next page's first object by accident via the same test. */
unsigned char varyobj_page_gens_augmented(low_page_index_t page_index)
{
  return (varyobj_pages[page_index].scan_start_offset <= DOUBLEWORDS_PER_PAGE
          ? 0 : (1<<__immobile_obj_generation(varyobj_scan_start(page_index))))
         | varyobj_pages[page_index].generations;
}

//// Fixed-length object allocator

/* Return the index of an immobile page that is probably not totally full,
   starting with 'hint_page' and wrapping around.
   'attributes' determine an eligible page.
   *FIXEDOBJ-SPACE-FREE-POINTER* is updated to point beyond the found page
   if it previously did not. */

static int get_freeish_page(int hint_page, int attributes)
{
  int page = hint_page;
  lispobj *new_free_pointer, *old_free_pointer, *actual_old;
  int page_attr_packed;
  unsigned char best_genmask = 0xff;
  int best_page = -1;
  const int npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;

  // Speed of this could be improved by keeping a linked list of pages
  // with any space available, headed by a field in the page struct.
  // This is totally lock-free / wait-free though, so it's really not
  // too shabby, because it never has to deal with a page-table mutex.
  do {
      page_attr_packed = fixedobj_pages[page].attr.packed;
      if (page_attr_packed == 0)
          if ((page_attr_packed =
               __sync_val_compare_and_swap(&fixedobj_pages[page].attr.packed,
                                           0, attributes)) == 0) {
              // Atomically assign MAX(old_free_pointer, new_free_pointer)
              // into the free pointer.
              new_free_pointer = fixedobj_page_address(page+1);
              old_free_pointer = fixedobj_free_pointer;
              while (new_free_pointer > old_free_pointer) {
                  actual_old =
                    __sync_val_compare_and_swap(&fixedobj_free_pointer,
                                                old_free_pointer,
                                                new_free_pointer);
                  if (actual_old == old_free_pointer)
                      break;
                  old_free_pointer = actual_old;
              }
              return page;
          }
      if (ATTRIBUTES_MATCH_P(page_attr_packed, attributes)
          && !page_full_p(page)) {
          // Try to avoid new objects on pages with any pseudo-static objects,
          // because then touching the young object forces scanning the page,
          // which is unfortunate if most things on it were untouched.
          if (fixedobj_pages[page].gens < (1<<PSEUDO_STATIC_GENERATION)) {
            // instant win
            return page;
          } else if (fixedobj_pages[page].gens < best_genmask) {
            best_genmask = fixedobj_pages[page].gens;
            best_page = page;
          }
      }
      if (++page >= npages) page = 0;
  } while (page != hint_page);
  if (best_page >= 0)
      return best_page;
  lose("No more immobile pages available");
}

/// The size classes are: 2w, 4w, 6w, 8w, ..., up to 20w.
/// Each size class can store at most 2 different alignments. So for example in
/// the size class for 14 words there will be a page hint for 14-word-aligned
/// objects and at most one other alignment.
#define MAX_SIZE_CLASSES 10
#define MAX_HINTS_PER_CLASS 2
long page_hints[MAX_SIZE_CLASSES*MAX_HINTS_PER_CLASS];
static inline int hint_attributes(long hint) { return hint & 0xFFFFFFFF; }
static int hint_page(long hint) { return hint>>32; }
static long make_hint(int page, int attributes) {
    return ((long)page << 32) | attributes;
}

static int get_hint(int attributes, int *page)
{
    long hint;
    unsigned int size = attributes >> 16;
    int hint_index = size - 2, limit = hint_index + 1, free_slot = -1;
    if (hint_index > (int)(sizeof page_hints / sizeof (long)))
        lose("Unexpectedly large fixedobj allocation request");
    for ( ; hint_index <= limit; ++hint_index ) {
#ifdef __ATOMIC_SEQ_CST
        __atomic_load(&page_hints[hint_index], &hint, __ATOMIC_SEQ_CST);
#else
        hint = __sync_fetch_and_add(&page_hints[hint_index], 0);
#endif
        if (hint_attributes(hint) == attributes) {
            *page = hint_page(hint);
            return hint_index;
        } else if (hint == 0 && free_slot < 0)
            free_slot = hint_index;
    }
    if (free_slot<0)
        lose("Should not happen\n"); // TODO: evict a hint
    // Linearly search for a free page from the beginning
    int free_page = get_freeish_page(0, attributes);
    *page = free_page;
    int existing = __sync_val_compare_and_swap(&page_hints[free_slot], 0,
                                               make_hint(free_page, attributes));
    if (existing) // collided. don't worry about it
        return -1;
    return free_slot;
}

static void unset_hint(int page)
{
    int attributes = fixedobj_pages[page].attr.packed;
    unsigned int size = (attributes >> 16) & 0xff; // mask off the generation bits
    int hint_index = size - 2;
    if (hint_page(page_hints[hint_index]) == page)
        page_hints[hint_index] = 0;
    if (hint_page(page_hints[hint_index+1]) == page)
        page_hints[hint_index+1] = 0;
}

// Unused, but possibly will be for some kind of collision-avoidance scheme
// on claiming of new free pages.
long immobile_alloc_collisions;

/* Beginning at page index *hint, attempt to find space
   for an object on a page with page_attributes. Write its header word
   and return a C (native) pointer. The start page MUST have the proper
   characteristisc, but might be totally full.

   Precondition: Lisp has established a pseudo-atomic section. */

/* There is a slightly different algorithm that would probably be faster
   than what is currently implemented:
   - hint should be the address of a word that you try to claim
     as an object header; it moves from high-to-low instead of low-to-high.
     It's easier to compute the page base than the last valid object start
     if there are some wasted words at the end due to page size not being
     a perfect multiple of object size.
   - you do a CAS into that word, and either suceed or fail
   - if you succeed, subtract the object spacing and compare
     to the page's base address, which can be computed by
     masking. if the next address is above or equal to the page start,
     store it in the hint, otherwise mark the page full */

static lispobj* alloc_immobile_obj(int page_attributes, lispobj header)
{
  int hint_index, page;
  lispobj word;
  char * page_data, * obj_ptr, * next_obj_ptr, * limit, * next_free;
  int spacing_in_bytes = OBJ_SPACING(page_attributes) << WORD_SHIFT;
  const int npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;

  hint_index = get_hint(page_attributes, &page);
  gc_dcheck(fixedobj_page_address(page) < (void*)fixedobj_free_pointer);
  do {
      page_data = fixedobj_page_address(page);
      obj_ptr = page_data + fixedobj_pages[page].free_index;
      limit = page_data + IMMOBILE_CARD_BYTES - spacing_in_bytes;
      while (obj_ptr <= limit) {
          word = *(lispobj*)obj_ptr;
          next_obj_ptr = obj_ptr + spacing_in_bytes;
          if (fixnump(word) // a fixnum marks free space
              && __sync_bool_compare_and_swap((lispobj*)obj_ptr,
                                              word, header)) {
              // The value formerly in the header word was the offset to
              // the next hole. Use it to update the freelist pointer.
              // Just slam it in.
              fixedobj_pages[page].free_index = next_obj_ptr + word - page_data;
              return (lispobj*)obj_ptr;
          }
          // If some other thread updated the free_index
          // to a larger value, use that. (See example below)
          next_free = page_data + fixedobj_pages[page].free_index;
          obj_ptr = next_free > next_obj_ptr ? next_free : next_obj_ptr;
      }
      set_page_full(page);
      int old_page = page;
      page = get_freeish_page(page+1 >= npages ? 0 : page+1,
                              page_attributes);
      if (hint_index >= 0) { // try to update the hint
          __sync_val_compare_and_swap(page_hints + hint_index,
                                      make_hint(old_page, page_attributes),
                                      make_hint(page, page_attributes));
      }
  } while (1);
}

/*
Example: Conside the freelist initially pointing to word index 6
Threads A, and B, and C each want to claim index 6.
- Thread A wins and then is switched out immediately after the CAS.
- Thread B fails to claim cell 6, claims cell 12 instead.
- Thread C fails to claim a cell and is switched out immediately
  after the CAS.
- Thread B writes the index of the next hole, cell 18 into the
  page's freelist cell.
- Thread A wakes up and writes 12 into the freelist cell.
- Thread C wakes up sees 12 for next_offset. 12 is greater than 6,
  so it sets its next probe location to 12.
  It fails the fixnump(header) test.
- Thread C sees that next_offset is still 12,
  so it skips by the page's object spacing instead, and will continue
  to do so until hitting the end of the page.
*/

//// The collector

// Find the high water marks for this GC scavenge phase
// (avoid passing exactly IMMOBILE_SPACE_END, which has no page index)
#define calc_max_used_fixedobj_page() find_fixedobj_page_index(fixedobj_free_pointer-1)
#define calc_max_used_varyobj_page() find_varyobj_page_index(varyobj_free_pointer-1)

void update_immobile_nursery_bits()
{
  low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
  low_page_index_t page;

  if (ENABLE_PAGE_PROTECTION) {
      // Unprotect the in-use ranges. Any page could be written during scavenge
      os_protect((os_vm_address_t)FIXEDOBJ_SPACE_START,
                 (lispobj)fixedobj_free_pointer - FIXEDOBJ_SPACE_START,
                 OS_VM_PROT_ALL);
  }

  for (page=0; page <= max_used_fixedobj_page ; ++page) {
      // any page whose free index changed contains nursery objects
      if (fixedobj_pages[page].free_index >> WORD_SHIFT !=
          fixedobj_pages[page].prior_gc_free_word_index)
          fixedobj_pages[page].gens |= 1;
#ifdef VERIFY_PAGE_GENS
      check_fixedobj_page(page, 0xff, 0xff);
#endif
  }
#ifdef VERIFY_PAGE_GENS
  check_varyobj_pages();
#endif
}

/* Turn a white object grey. Also enqueue the object for re-scan if required */
void
enliven_immobile_obj(lispobj *ptr, int rescan) // a native pointer
{
    if (widetag_of(ptr) == SIMPLE_FUN_WIDETAG)
        ptr = fun_code_header(ptr);
    gc_assert(__immobile_obj_gen_bits(ptr) == from_space);
    int pointerish = !leaf_obj_widetag_p(widetag_of(ptr));
    int bits = (pointerish ? 0 : IMMOBILE_OBJ_VISITED_FLAG);
    // enlivening makes the object appear as if written, so that
    // scav_code_header won't skip it, thus ensuring we transitively
    // scavenge + enliven newspace objects.
    if (widetag_of(ptr) == CODE_HEADER_WIDETAG)
        bits |= OBJ_WRITTEN_FLAG;
    assign_generation(ptr, bits | new_space);
    low_page_index_t page_index = find_fixedobj_page_index(ptr);
    boolean varyobj = 0;

    if (page_index < 0) {
        page_index = find_varyobj_page_index(ptr);
        gc_assert(page_index >= 0);
        varyobj_pages[page_index].generations |= 1<<new_space;
        varyobj = 1;
    } else {
        fixedobj_pages[page_index].gens |= 1<<new_space;
    }
    // If called from preserve_pointer(), then we haven't scanned immobile
    // roots yet, so we only need ensure that this object's page's WP bit
    // is cleared so that the page is not skipped during root scan.
    if (!rescan) {
        if (pointerish) {
            if (varyobj)
                varyobj_page_touched_bits[page_index/32] |= 1U << (page_index & 31);
            else
                SET_WP_FLAG(page_index, WRITE_PROTECT_CLEARED);
        }
        return; // No need to enqueue.
    }

    // TODO: check that objects on protected root pages are not enqueued

    // Do nothing if either we don't need to look for pointers in this object,
    // or the work queue has already overflowed, causing a full scan.
    if (!pointerish || immobile_scav_queue_count > QCAPACITY) return;

    // count is either less than or equal to QCAPACITY.
    // If equal, just bump the count to signify overflow.
    if (immobile_scav_queue_count < QCAPACITY) {
        immobile_scav_queue[immobile_scav_queue_head] = (lispobj)ptr;
        immobile_scav_queue_head = (immobile_scav_queue_head + 1) & (QCAPACITY - 1);
    }
    ++immobile_scav_queue_count;
}

/* If 'addr' points to an immobile object, then make the object
   live by promotion. But if the object is not in the generation
   being collected, do nothing */
void immobile_space_preserve_pointer(void* addr)
{
    unsigned char genmask = compacting_p() ? 1<<from_space : 0xff;
    lispobj* object_start;
    int valid = 0;
    low_page_index_t page_index = find_varyobj_page_index(addr);

    if (page_index >= 0) {
        // Restrict addr to lie below 'varyobj_free_pointer'.
        // This way, if the gens byte is nonzero but there is
        // a final array acting as filler on the remainder of the
        // final page, we won't accidentally find that.
        lispobj* scan_start;
        valid = addr < (void*)varyobj_free_pointer
            && (varyobj_page_gens_augmented(page_index) & genmask)
            && (scan_start = varyobj_scan_start(page_index)) <= (lispobj*)addr
            && (object_start = gc_search_space(scan_start, addr)) != 0
            /* gc_search_space can return filler objects, unlike
             * search_immobile_space which can not */
            && !filler_obj_p(object_start)
            && (instruction_ptr_p(addr, object_start)
                || properly_tagged_descriptor_p(addr, object_start));
    } else if ((page_index = find_fixedobj_page_index(addr)) >= FIXEDOBJ_RESERVED_PAGES
               && ((fixedobj_pages[page_index].gens & genmask) != 0)) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        int obj_index = ((uword_t)addr & (IMMOBILE_CARD_BYTES-1)) / obj_spacing;
        dprintf((logfile,"Pointer %p is to immobile page %d, object %d\n",
                 addr, page_index, obj_index));
        char* page_start_addr = PTR_ALIGN_DOWN(addr, IMMOBILE_CARD_BYTES);
        object_start = (lispobj*)(page_start_addr + obj_index * obj_spacing);
        valid = !fixnump(*object_start)
            && (lispobj*)addr < object_start + fixedobj_page_obj_size(page_index)
            && (properly_tagged_descriptor_p(addr, object_start)
                || widetag_of(object_start) == FUNCALLABLE_INSTANCE_WIDETAG);
    } else return;
    if (valid && (!compacting_p() ||
                  __immobile_obj_gen_bits(object_start) == from_space)) {
        dprintf((logfile,"immobile obj @ %p (<- %p) is conservatively live\n",
                 object_start, addr));
        if (compacting_p())
            enliven_immobile_obj(object_start, 0);
        else
            gc_mark_obj(compute_lispobj(object_start));
    }
}

// Loop over the newly-live objects, scavenging them for pointers.
// As with the ordinary gencgc algorithm, this uses almost no stack.
static void full_scavenge_immobile_newspace()
{
    page_index_t page;
    unsigned char bit = 1<<new_space;

    // Fixed-size object pages.

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page; ++page) {
        if (!(fixedobj_pages[page].gens & bit)) continue;
        // Skip amount within the loop is in bytes.
        int obj_spacing = fixedobj_page_obj_align(page);
        lispobj* obj    = fixedobj_page_address(page);
        lispobj* limit  = compute_fixedobj_limit(obj, obj_spacing);
        do {
            if (!fixnump(*obj) && __immobile_obj_gen_bits(obj) == new_space) {
                set_visited(obj);
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
            }
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }

    // Variable-size object pages

    low_page_index_t max_used_varyobj_page = calc_max_used_varyobj_page();
    page = -1; // -1 because of pre-increment
    while (1) {
        // Find the next page with anything in newspace.
        do {
            if (++page > max_used_varyobj_page) return;
        } while ((varyobj_pages[page].generations & bit) == 0);
        lispobj* obj = varyobj_scan_start(page);
        do {
            lispobj* limit = (lispobj*)varyobj_page_address(page) + WORDS_PER_PAGE;
            int n_words;
            for ( ; obj < limit ; obj += n_words ) {
                lispobj header = *obj;
                if (__immobile_obj_gen_bits(obj) == new_space) {
                    set_visited(obj);
                    n_words = scavtab[header_widetag(header)](obj, header);
                } else {
                    n_words = sizetab[header_widetag(header)](obj);
                }
            }
            page = find_varyobj_page_index(obj);
            // Bail out if exact absolute end of immobile space was reached.
            if (page < 0) return;
            // If 'page' should be scanned, then pick up where we left off,
            // without recomputing 'obj' but setting a higher 'limit'.
        } while (varyobj_pages[page].generations & bit);
    }
}

/// Repeatedly scavenge immobile newspace work queue until we find no more
/// reachable objects within. (They might be in dynamic space though).
/// If queue overflow already happened, then a worst-case full scan is needed.
/// If it didn't, we try to drain the queue, hoping that overflow does
/// not happen while doing so.
/// The approach taken is more subtle than just dequeuing each item,
/// scavenging, and letting the outer 'while' loop take over.
/// That would be ok, but could cause more full scans than necessary.
/// Instead, since each entry in the queue is useful information
/// in the non-overflow condition, perform all the work indicated thereby,
/// rather than considering the queue discardable as soon as overflow happens.
/// Essentially we just have to capture the valid span of enqueued items,
/// because the queue state is inconsistent when 'count' exceeds 'capacity'.
void scavenge_immobile_newspace()
{
  while (immobile_scav_queue_count) {
      if (immobile_scav_queue_count > QCAPACITY) {
          immobile_scav_queue_count = 0;
          full_scavenge_immobile_newspace();
      } else {
          int queue_index_from = (immobile_scav_queue_head - immobile_scav_queue_count)
                               & (QCAPACITY - 1);
          int queue_index_to   = immobile_scav_queue_head;
          int i = queue_index_from;
          // The termination condition can't be expressed as an inequality,
          // since the indices might be reversed due to wraparound.
          // To express as equality entails forcing at least one iteration
          // since the ending index might be the starting index.
          do {
              lispobj* obj = (lispobj*)(uword_t)immobile_scav_queue[i];
              i = (1 + i) & (QCAPACITY-1);
              // Only decrement the count if overflow did not happen.
              // The first iteration of this loop will decrement for sure,
              // but subsequent iterations might not.
              if (immobile_scav_queue_count <= QCAPACITY)
                  --immobile_scav_queue_count;
              // FIXME: should not enqueue already-visited objects,
              // but a gc_assert() that it wasn't visited fails.
              if (!(__immobile_obj_gen_bits(obj) & IMMOBILE_OBJ_VISITED_FLAG)) {
                set_visited(obj);
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
              }
          } while (i != queue_index_to);
      }
  }
}

// Return a page >= page_index having potential old->young pointers,
// or -1 if there isn't one.
static int next_varyobj_root_page(unsigned int page_index,
                                  unsigned int end_bitmap_index,
                                  unsigned char genmask)
{
    unsigned int map_index = page_index / 32;
    if (map_index >= end_bitmap_index) return -1;
    int bit_index = page_index & 31;
    // Look only at bits of equal or greater weight than bit_index.
    unsigned int word = (0xFFFFFFFFU << bit_index) & varyobj_page_touched_bits[map_index];
    while (1) {
        if (word) {
            bit_index = ffs(word) - 1;
            page_index = map_index * 32 + bit_index;
            if (varyobj_page_gens_augmented(page_index) & genmask)
                return page_index;
            else {
                word ^= (1U<<bit_index);
                continue;
            }
        }
        if (++map_index >= end_bitmap_index) return -1;
        word = varyobj_page_touched_bits[map_index];
    }
}

void
scavenge_immobile_roots(generation_index_t min_gen, generation_index_t max_gen)
{
    // example: scavenging gens 2..6, the mask of root gens is #b1111100
    int genmask = ((1 << (max_gen - min_gen + 1)) - 1) << min_gen;

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    low_page_index_t page;
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page ; ++page) {
        if (fixedobj_page_wp(page) || !(fixedobj_pages[page].gens & genmask))
            continue;
        int obj_spacing = fixedobj_page_obj_align(page);
        lispobj* obj = fixedobj_page_address(page);
        lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
        int gen;
        // Immobile space can only contain objects with a header word,
        // no conses, so any fixnum where a header could be is not a live
        // object.
        do {
            if (!fixnump(*obj) && (genmask >> (gen=__immobile_obj_gen_bits(obj)) & 1)) {
                if (gen == new_space) { set_visited(obj); }
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
            }
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }

    // Variable-length object pages
    low_page_index_t max_used_varyobj_page = calc_max_used_varyobj_page();
    unsigned n_varyobj_pages = 1+max_used_varyobj_page;
    unsigned end_bitmap_index = (n_varyobj_pages+31)/32;
    page = next_varyobj_root_page(0, end_bitmap_index, genmask);
    while (page >= 0) {
        lispobj* obj = varyobj_scan_start(page);
        do {
            lispobj* limit = (lispobj*)varyobj_page_address(page) + WORDS_PER_PAGE;
            int n_words, gen;
            for ( ; obj < limit ; obj += n_words ) {
                lispobj header = *obj;
                // scav_code_header will do nothing if the object isn't
                // marked as written.
                if (genmask >> (gen=__immobile_obj_gen_bits(obj)) & 1) {
                    if (gen == new_space) { set_visited(obj); }
                    n_words = scavtab[header_widetag(header)](obj, header);
                } else {
                    n_words = sizetab[header_widetag(header)](obj);
                }
            }
            page = find_varyobj_page_index(obj);
        } while (page > 0
                 && (varyobj_pages[page].generations & genmask)
                 && varyobj_page_touched(page));
        if (page < 0) break;
        page = next_varyobj_root_page(1+page, end_bitmap_index, genmask);
    }
    scavenge_immobile_newspace();
}

void write_protect_immobile_space()
{
    immobile_scav_queue_head = 0;

    if (!ENABLE_PAGE_PROTECTION)
        return;

    // Now find contiguous ranges of pages that are protectable,
    // minimizing the number of system calls as much as possible.
    int i, start = -1, end = -1; // inclusive bounds on page indices
    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for (i = max_used_fixedobj_page ; i >= 0 ; --i) {
        if (fixedobj_page_wp(i)) {
            if (end < 0) end = i;
            start = i;
        }
        if (end >= 0 && (!fixedobj_page_wp(i) || i == 0)) {
            os_protect(fixedobj_page_address(start),
                       IMMOBILE_CARD_BYTES * (1 + end - start),
                       OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);
            start = end = -1;
        }
    }
}

static inline generation_index_t
pointee_gen(lispobj thing, int keep_gen, int new_gen)
{
    int to_page = find_page_index((void*)thing);
    // the default return value should be larger than any real generation number.
    int gen = 127; // generation_index_t is a signed char
    if (to_page >= 0) { // points to ordinary dynamic space
        gen = page_table[to_page].gen;
        if (gen == PSEUDO_STATIC_GENERATION+1) // scratch gen
            gen = new_gen; // is actually this
    } else if (immobile_space_p(thing)) {
        // Use the general variant of immobile_obj_gen_bits
        // because the pointed-to object could be anything.
        gen = immobile_obj_gen_bits(native_pointer(thing));
        if (gen == keep_gen) // keep gen
            gen = new_gen; // is actually this
    }
    return gen;
}

// 'keep_gen' is the value of the generation byte of objects that were
// candidates to become garbage, but remain live after this gc.
// It will necessarily have the VISITED flag on.
// 'new_gen' is the generation number that those objects will have
// after collection, which is either the same generation or one higher,
// depending on the 'raise' flag for this GC cycle.
static int
younger_p(lispobj thing, int gen, int keep_gen, int new_gen)
{
    return is_lisp_pointer(thing) && pointee_gen(thing, keep_gen, new_gen) < gen;
}

// Scan range between start and end (exclusive) for old-to-young pointers.
static int
range_points_to_younger_p(lispobj* obj, lispobj* end,
                          int gen, int keep_gen, int new_gen)
{
#ifdef DEBUG
  lispobj* __attribute__((unused)) saved_obj = obj, __attribute__((unused)) header = *obj;
#endif
    do {
        lispobj thing = *obj;
        if (is_lisp_pointer(thing) && pointee_gen(thing, keep_gen, new_gen) < gen)
            return 1; // yes, points to younger
    } while (++obj < end);
    return 0; // no, does not point to younger
}

// Scan a fixed-size object for old-to-young pointers.
// Since fixed-size objects are boxed and on known boundaries,
// we never start in the middle of random bytes, so the answer is exact.
static inline boolean
fixedobj_points_to_younger_p(lispobj* obj, int n_words,
                             int gen, int keep_gen, int new_gen)
{
  lispobj layout, lbitmap;

  switch (widetag_of(obj)) {
  case FDEFN_WIDETAG:
    return younger_p(fdefn_callee_lispobj((struct fdefn*)obj),
                     gen, keep_gen, new_gen)
        || range_points_to_younger_p(obj+1, obj+3, gen, keep_gen, new_gen);
  case CODE_HEADER_WIDETAG:
    // This is a simplifying trampoline around a closure or FIN.
    // The only pointerish slot is debug_info (the called function).
    // The size slot is a descriptor, though a non-pointer.
    return younger_p(((struct code*)obj)->debug_info, gen, keep_gen, new_gen);
  case INSTANCE_WIDETAG:
  case FUNCALLABLE_INSTANCE_WIDETAG:
    layout = instance_layout(obj);
    if (younger_p(layout, gen, keep_gen, new_gen))
        return 1;
    if ((lbitmap = LAYOUT(layout)->bitmap) != make_fixnum(-1)) {
        gc_assert(fixnump(lbitmap));  // No bignums (yet)
        sword_t bitmap = fixnum_value(lbitmap);
        lispobj* where = obj + 1;
        for ( ; --n_words ; ++where, bitmap >>= 1 )
            if ((bitmap & 1) != 0 && younger_p(*where, gen, keep_gen, new_gen))
                return 1;
        return 0;
    }
    // FALLTHROUGH_INTENDED
  }
  return range_points_to_younger_p(obj+1, obj+n_words, gen, keep_gen, new_gen);
}

static boolean
varyobj_points_to_younger_p(lispobj* obj, int gen, int keep_gen, int new_gen,
                            os_vm_address_t page_begin,
                            os_vm_address_t page_end) // upper (exclusive) bound
{
    lispobj *begin, *end, word = *obj;
    unsigned char widetag = header_widetag(word);
    if (widetag == CODE_HEADER_WIDETAG) { // usual case. Like scav_code_header()
        return header_rememberedp(word);
    } else if (widetag == FDEFN_WIDETAG ||
               widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
        // both of these have non-descriptor bits in at least one word,
        // thus precluding a simple range scan.
        // Due to ignored address bounds, in the rare case of a FIN or fdefn in varyobj
        // subspace and spanning cards, we might say that neither card can be protected,
        // when one or the other could be. Not a big deal.
        return fixedobj_points_to_younger_p(obj, sizetab[widetag](obj),
                                            gen, keep_gen, new_gen);
    } else if (widetag == SIMPLE_VECTOR_WIDETAG) {
        sword_t length = fixnum_value(((struct vector *)obj)->length);
        begin = obj + 2; // skip the header and length
        end = obj + ALIGN_UP(length + 2, 2);
    } else if (leaf_obj_widetag_p(widetag)) {
        return 0;
    } else {
        lose("Unexpected widetag %x @ %p", widetag, obj);
    }
    // Fallthrough: scan words from begin to end
    if (page_begin > (os_vm_address_t)begin) begin = (lispobj*)page_begin;
    if (page_end   < (os_vm_address_t)end)   end   = (lispobj*)page_end;
    if (end > begin && range_points_to_younger_p(begin, end, gen, keep_gen, new_gen))
        return 1;
    return 0;
}

/// The next two functions are analogous to 'update_page_write_prot()'
/// but they differ in that they are "precise" - random code bytes that look
/// like pointers are not accidentally treated as pointers.

// If 'page' does not contain any objects that points to an object
// younger than themselves, then return true.
// This is called on pages that do not themselves contain objects of
// the generation being collected, but might contain pointers
// to younger generations, which we detect by a cleared WP status bit.
// The bit is cleared on any write, though, even of a non-pointer,
// so this unfortunately has to be tested much more often than we'd like.
static inline boolean can_wp_fixedobj_page(page_index_t page, int keep_gen, int new_gen)
{
    int obj_spacing = fixedobj_page_obj_align(page);
    int obj_size_words = fixedobj_page_obj_size(page);
    lispobj* obj = fixedobj_page_address(page);
    lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
    do {
        if (!fixnump(*obj) && // an object header
            fixedobj_points_to_younger_p(obj, obj_size_words,
                                         __immobile_obj_generation(obj),
                                         keep_gen, new_gen))
            return 0;
    } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    return 1;
}

// To scan _only_ 'page' is impossible in general, but we can act like only
// one page was scanned by backing up to the first object whose end is on
// or after it, and then restricting points_to_younger within the boundaries.
// Doing it this way is probably much better than conservatively assuming
// that any word satisfying is_lisp_pointer() is a pointer.
static inline boolean can_wp_varyobj_page(page_index_t page, int keep_gen, int new_gen)
{
    lispobj *begin = varyobj_page_address(page);
    lispobj *end   = begin + WORDS_PER_PAGE;
    lispobj *obj   = varyobj_scan_start(page);
    for ( ; obj < end ; obj += sizetab[widetag_of(obj)](obj) ) {
        gc_assert(other_immediate_lowtag_p(*obj));
        if (!filler_obj_p(obj) &&
            varyobj_points_to_younger_p(obj,
                                        __immobile_obj_generation(obj),
                                        keep_gen, new_gen,
                                        (os_vm_address_t)begin,
                                        (os_vm_address_t)end))
            return 0;
    }
    return 1;
}

/*
  Sweep immobile space by zeroing the memory of trashed objects
  and linking them into the freelist.

  Possible improvements:
  - If an entire page becomes nothing but holes, we could bzero it
    instead of object-at-a-time clearing. But it's not known to be
    so until after the sweep, so it would entail two passes per page,
    one to mark holes and one to zero them.
  - And perhaps bzero could be used on ranges of holes, because
    in that case each hole's pointer to the next hole is zero as well.
*/

#define SETUP_GENS()                                                   \
  /* Only care about pages with something in old or new space. */      \
  int relevant_genmask = (1 << from_space) | (1 << new_space);         \
  /* Objects whose gen byte is 'keep_gen' are alive. */                \
  int keep_gen = IMMOBILE_OBJ_VISITED_FLAG | new_space;                \
  /* Objects whose gen byte is 'from_space' are trash. */              \
  int discard_gen = from_space;                                        \
  /* Moving non-garbage into either 'from_space' or 'from_space+1' */  \
  generation_index_t new_gen = from_space + (raise!=0)

// The new value of the page generation mask is computed as follows:
// If 'raise' = 1 then:
//     Nothing resides in 'from_space', and 'from_space+1' gains new objects
//     if and only if any objects on the page were retained.
// If 'raise' = 0 then:
//     Nothing resides in the scratch generation, and 'from_space'
//     has objects if and only if any objects were retained.
#define COMPUTE_NEW_MASK(var, old) \
  int var = old & ~(1<<from_space); \
  if ( raise ) \
    var |= 1<<(from_space+1) & any_kept; \
  else \
    var = (var & ~(1<<new_space)) | (1<<from_space & any_kept)

static void
sweep_fixedobj_pages(int raise)
{
    char *page_base;
    lispobj *obj, *limit, *hole;
    // This will be needed for space accounting.
    // threads might fail to consume all the space on a page.
    // By storing in the page table the count of holes that really existed
    // at the start of the prior GC, and subtracting from that the number
    // that exist now, we know how much usable space was obtained (per page).
    int n_holes = 0;
    int word_idx;

    SETUP_GENS();

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    low_page_index_t page;
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page; ++page) {
        // On pages that won't need manipulation of the freelist,
        // we try to do less work than for pages that need it.
        if (!(fixedobj_pages[page].gens & relevant_genmask)) {
            // Scan for old->young pointers, and WP if there are none.
            if (ENABLE_PAGE_PROTECTION && !fixedobj_page_wp(page)
                && fixedobj_pages[page].gens > 1
                && can_wp_fixedobj_page(page, keep_gen, new_gen)) {
                SET_WP_FLAG(page, WRITE_PROTECT);
                dprintf((logfile, "set WP(1) on fixedobj page %d (mask=%#x)\n",
                         page, fixedobj_pages[page].gens));
            }
            continue;
        }
        int obj_spacing = fixedobj_page_obj_align(page);
        int obj_size_words = fixedobj_page_obj_size(page);
        page_base = fixedobj_page_address(page);
        limit = compute_fixedobj_limit(page_base, obj_spacing);
        obj = (lispobj*)page_base;
        hole = NULL;
        int any_kept = 0; // was anything moved to the kept generation
        n_holes = 0;

        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = ENABLE_PAGE_PROTECTION && !fixedobj_page_wp(page);
        int gen;
        do {
            if (fixnump(*obj)) { // was already a hole
            trash_it:
                // re-link it into the new freelist
                if (hole)
                    // store the displacement from the end of the object
                    // at prev_hole to the start of this object.
                    *hole = (lispobj)((char*)obj - ((char*)hole + obj_spacing));
                else // this is the first seen hole on the page
                    // record the byte offset to that hole
                  fixedobj_pages[page].free_index = (char*)obj - page_base;
                hole = obj;
                n_holes ++;
            } else if ((gen = __immobile_obj_gen_bits(obj) & ~OBJ_WRITTEN_FLAG)
                       == discard_gen) { // trash
                for (word_idx=obj_size_words-1 ; word_idx > 0 ; --word_idx)
                    obj[word_idx] = 0;
                goto trash_it;
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!fixedobj_points_to_younger_p(obj, obj_size_words,
                                                        gen, keep_gen, new_gen));
#endif
                any_kept = -1;
            } else if (wp_it && fixedobj_points_to_younger_p(obj, obj_size_words,
                                                             gen, keep_gen, new_gen))
              wp_it = 0;
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
        if ( hole ) // terminate the chain of holes
            *hole = (lispobj)((char*)obj - ((char*)hole + obj_spacing));
        fixedobj_pages[page].prior_gc_free_word_index =
          fixedobj_pages[page].free_index >> WORD_SHIFT;

        COMPUTE_NEW_MASK(mask, fixedobj_pages[page].gens);
        if ( mask ) {
            fixedobj_pages[page].gens = mask;
            if (wp_it) {
                SET_WP_FLAG(page, WRITE_PROTECT);
                dprintf((logfile, "set WP(2) on fixedobj page %d\n", page));
            }
        } else {
            dprintf((logfile,"page %d is all garbage\n", page));
            unset_hint(page);
            fixedobj_pages[page].attr.packed = 0;
        }
#ifdef DEBUG
        check_fixedobj_page(page, keep_gen, new_gen);
#endif
        dprintf((logfile,"page %d: %d holes\n", page, n_holes));
    }
}

static void make_filler(void* where, int nbytes)
{
    if (nbytes < 4*N_WORD_BYTES)
        lose("can't place filler @ %p - too small", where);
    else { // Create a filler object.
        struct code* code  = (struct code*)where;
        code->header       = ((uword_t)nbytes << (CODE_HEADER_SIZE_SHIFT-WORD_SHIFT))
                             | CODE_HEADER_WIDETAG;
        code->boxed_size   = 0;
        code->debug_info   = varyobj_holes;
        varyobj_holes      = (lispobj)code;
    }
}

// Scan for freshly trashed objects and turn them into filler.
// Lisp is responsible for consuming the free space
// when it next allocates a variable-size object.
static void
sweep_varyobj_pages(int raise)
{
    SETUP_GENS();

    low_page_index_t max_used_varyobj_page = calc_max_used_varyobj_page();
    lispobj* free_pointer = varyobj_free_pointer;
    low_page_index_t page;
    for (page = 0; page <= max_used_varyobj_page; ++page) {
        int genmask = varyobj_pages[page].generations;
        if (!(genmask & relevant_genmask)) { // Has nothing in oldspace or newspace.
            // Scan for old->young pointers, and WP if there are none.
            if (ENABLE_PAGE_PROTECTION && varyobj_page_touched(page)
                && varyobj_page_gens_augmented(page) > 1
                && can_wp_varyobj_page(page, keep_gen, new_gen)) {
                varyobj_page_touched_bits[page/32] &= ~(1U<<(page & 31));
            }
            continue;
        }
        lispobj* page_base = varyobj_page_address(page);
        lispobj* limit = page_base + WORDS_PER_PAGE;
        if (limit > free_pointer) limit = free_pointer;
        int any_kept = 0; // was anything moved to the kept generation
        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = ENABLE_PAGE_PROTECTION && varyobj_page_touched(page);
        lispobj* obj = varyobj_scan_start(page);
        int size, gen;

        if (obj < page_base) {
            // An object whose tail is on this page, or which spans this page,
            // would have been promoted/kept while dealing with the page with
            // the object header. Therefore we don't need to consider that object,
            // * except * that we do need to consider whether it is an old object
            // pointing to a young object.
            if (wp_it // If we wanted to try write-protecting this page,
                // and the object starting before this page is strictly older
                // than the generation that we're moving retained objects into
                && (gen = __immobile_obj_gen_bits(obj)) > new_gen
                // and it contains an old->young pointer
                && varyobj_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                               (os_vm_address_t)page_base,
                                               (os_vm_address_t)limit)) {
                wp_it = 0;
            }
            // We MUST skip this object in the sweep, because in the case of
            // non-promotion (raise=0), we could see an object in from_space
            // and believe it to be dead.
            obj += sizetab[widetag_of(obj)](obj);
            // obj can't hop over this page. If it did, there would be no
            // headers on the page, and genmask would have been zero.
            gc_assert(obj < limit);
        }
        for ( ; obj < limit ; obj += size ) {
            lispobj word = *obj;
            size = sizetab[header_widetag(word)](obj);
            if (filler_obj_p(obj)) { // do nothing
            } else if ((gen = __immobile_obj_gen_bits(obj) & ~OBJ_WRITTEN_FLAG)
                       == discard_gen) {
                make_filler(obj, size * N_WORD_BYTES);
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!varyobj_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                                       (os_vm_address_t)page_base,
                                                       (os_vm_address_t)limit));
#endif
                any_kept = -1;
            } else if (wp_it &&
                       varyobj_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                                   (os_vm_address_t)page_base,
                                                   (os_vm_address_t)limit))
                wp_it = 0;
        }
        COMPUTE_NEW_MASK(mask, varyobj_pages[page].generations);
        varyobj_pages[page].generations = mask;
        if ( mask && wp_it )
            varyobj_page_touched_bits[page/32] &= ~(1U << (page & 31));
    }
}

// TODO: (Maybe this won't work. Not sure yet.) rather than use the
// same 'raise' concept as in gencgc, each immobile object can store bits
// indicating whether it has survived any GC at its current generation.
// If it has, then it gets promoted next time, rather than all or nothing
// being promoted from the generation getting collected.
void
sweep_immobile_space(int raise)
{
  gc_assert(immobile_scav_queue_count == 0);
  sweep_fixedobj_pages(raise);
  sweep_varyobj_pages(raise);
}

static void gc_init_immobile()
{
#ifdef DEBUG
    logfile = stderr;
#endif
    int n_fixedobj_pages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;
    int n_varyobj_pages = VARYOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;
    fixedobj_pages = calloc(n_fixedobj_pages, sizeof(struct fixedobj_page));
    gc_assert(fixedobj_pages);

    n_bitmap_elts = ALIGN_UP(n_varyobj_pages, 32) / 32;
    int request = n_bitmap_elts * sizeof (int) + n_varyobj_pages * sizeof (int);
    varyobj_page_touched_bits = (unsigned int*)calloc(1, request);
    gc_assert(varyobj_page_touched_bits);
    // The conservative value for 'touched' is 1.
    memset(varyobj_page_touched_bits, 0xff, n_bitmap_elts * sizeof (int));
    varyobj_pages = (struct varyobj_page*)(varyobj_page_touched_bits + n_bitmap_elts);
    // Scav queue is arbitrarily located.
    immobile_scav_queue = malloc(QCAPACITY * sizeof(lispobj));
}

/* Because the immobile page table is not dumped into a core image,
   we have to reverse-engineer the characteristics of each page,
   which means figuring out what the object spacing should be.
   This is not difficult, but is a bit of a kludge */

static inline int immobile_obj_spacing(lispobj header_word, lispobj *obj,
                                       int actual_size)
{
    if (header_widetag(header_word) == INSTANCE_WIDETAG &&
        instance_layout(obj) == LAYOUT_OF_LAYOUT)
        return LAYOUT_ALIGN / N_WORD_BYTES;
    else
        return actual_size; // in words
}

// Signify that scan_start is initially not reliable
static int page_attributes_valid;

// Set the characteristics of each used page at image startup time.
void immobile_space_coreparse(uword_t fixedobj_len, uword_t varyobj_len)
{
    int n_pages, word_idx, page;
    generation_index_t gen = CORE_PAGE_GENERATION;

    gc_init_immobile();

    if (gen != PSEUDO_STATIC_GENERATION) {
        // Change every object's generation. This is only for debugging.
        lispobj *where, *end;
        where = fixedobj_page_address(0);
        end = (lispobj*)((char*)where + fixedobj_len);
        while (where < end) {
            if (!fixnump(*where)) assign_generation(where, gen);
            where += OBJECT_SIZE(*where, where);
        }
        where = (lispobj*)VARYOBJ_SPACE_START;
        end = (lispobj*)((char*)where + varyobj_len);
        while (where < end) {
            if (!filler_obj_p(where)) assign_generation(where, gen);
            where += OBJECT_SIZE(*where, where);
        }
        fprintf(stderr, "WARNING: demoted immobile objects to gen%d\n", gen);
    }

    n_pages = fixedobj_len / IMMOBILE_CARD_BYTES;
    for (page = 0; page <= FIXEDOBJ_RESERVED_PAGES; ++page) {
        // set page attributes that can't match anything in get_freeish_page()
        fixedobj_pages[page].attr.parts.obj_align = 1;
        fixedobj_pages[page].attr.parts.obj_size = 1;
        if (gen != 0 && ENABLE_PAGE_PROTECTION)
            fixedobj_pages[page].attr.parts.flags = WRITE_PROTECT;
        fixedobj_pages[page].gens |= 1 << gen;
    }
    for (page = FIXEDOBJ_RESERVED_PAGES ; page < n_pages ; ++page) {
        lispobj* page_data = fixedobj_page_address(page);
        for (word_idx = 0 ; word_idx < WORDS_PER_PAGE ; ++word_idx) {
            lispobj* obj = page_data + word_idx;
            lispobj header = *obj;
            if (!fixnump(header)) {
                gc_assert(other_immediate_lowtag_p(*obj));
                int size = sizetab[header_widetag(header)](obj);
                fixedobj_pages[page].attr.parts.obj_size = size;
                fixedobj_pages[page].attr.parts.obj_align
                  = immobile_obj_spacing(header, obj, size);
                fixedobj_pages[page].gens |= 1 << __immobile_obj_gen_bits(obj);
                if (gen != 0 && ENABLE_PAGE_PROTECTION)
                    fixedobj_pages[page].attr.parts.flags = WRITE_PROTECT;
                break;
            }
        }
    }
    uword_t address = VARYOBJ_SPACE_START;
    n_pages = varyobj_len / IMMOBILE_CARD_BYTES;
    lispobj* obj = (lispobj*)address;
    int n_words;
    low_page_index_t last_page = 0;
    // coreparse() already set varyobj_free_pointer
    lispobj* limit = varyobj_free_pointer;
    gc_assert(limit != 0 /* would be zero if not mmapped yet */
              && limit <= (lispobj*)(address + varyobj_len));
    for ( ; obj < limit ; obj += n_words ) {
        gc_assert(other_immediate_lowtag_p(obj[0]));
        n_words = sizetab[widetag_of(obj)](obj);
        if (filler_obj_p(obj)) {
            // Holes were chained through the debug_info slot at save.
            // Just update the head of the chain.
            varyobj_holes = (lispobj)obj;
            continue;
        }
        low_page_index_t first_page = find_varyobj_page_index(obj);
        last_page = find_varyobj_page_index(obj+n_words-1);
        // Only the page with this object header gets a bit in its gen mask.
        varyobj_pages[first_page].generations |= 1<<__immobile_obj_gen_bits(obj);
        // For each page touched by this object, set the page's
        // scan_start_offset, unless it was already set.
        int page;
        for (page = first_page ; page <= last_page ; ++page) {
            if (!varyobj_pages[page].scan_start_offset) {
                long offset = (char*)varyobj_page_address(page+1) - (char*)obj;
                varyobj_pages[page].scan_start_offset = offset >> (WORD_SHIFT + 1);
            }
        }
    }
    // Write a padding object if necessary
    if ((uword_t)limit & (IMMOBILE_CARD_BYTES-1)) {
        int remainder = IMMOBILE_CARD_BYTES -
          ((uword_t)limit & (IMMOBILE_CARD_BYTES-1));
        lispobj array_length = make_fixnum((remainder >> WORD_SHIFT) - 2);
        if (limit[0] == SIMPLE_ARRAY_FIXNUM_WIDETAG) {
            gc_assert(limit[1] == array_length);
        } else {
            limit[0] = SIMPLE_ARRAY_FIXNUM_WIDETAG;
            limit[1] = array_length;
        }
        int size = sizetab[SIMPLE_ARRAY_FIXNUM_WIDETAG](limit);
        lispobj* __attribute__((unused)) padded_end = limit + size;
        gc_assert(!((uword_t)padded_end & (IMMOBILE_CARD_BYTES-1)));
    }
    // Set the WP bits for pages occupied by the core file.
    // (There can be no inter-generation pointers.)
    if (gen != 0 && ENABLE_PAGE_PROTECTION) {
        low_page_index_t page;
        for (page = 0 ; page <= last_page ; ++page)
            varyobj_page_touched_bits[page/32] &= ~(1U<<(page & 31));
    }
    lispobj* code = (lispobj*)VARYOBJ_SPACE_START;
    asm_routines_start = VARYOBJ_SPACE_START;
    asm_routines_end   = asm_routines_start +
                         (sizetab[CODE_HEADER_WIDETAG](code) << WORD_SHIFT);
    page_attributes_valid = 1;
}

// Demote pseudo-static to highest normal generation
// so that all objects become eligible for collection.
void prepare_immobile_space_for_final_gc()
{
    int page;
    char* page_base;
    char* page_end = (char*)fixedobj_free_pointer;

    // The list of holes need not be saved.
    SYMBOL(IMMOBILE_FREELIST)->value = NIL;

    // This scan allow dissimilar object sizes. In the grand scheme of things
    // it's not terribly important to optimize out the calls to the sizing function
    // for the pages which have a unique size of object.
    for (page = 0, page_base = fixedobj_page_address(page) ;
         page_base < page_end ;
         page_base += IMMOBILE_CARD_BYTES, ++page) {
        unsigned char mask = fixedobj_pages[page].gens;
        if (mask & 1<<PSEUDO_STATIC_GENERATION) {
            lispobj* obj = (lispobj*)page_base;
            lispobj* limit = (lispobj*)((uword_t)obj + IMMOBILE_CARD_BYTES);
            for ( ; obj < limit ; obj += sizetab[widetag_of(obj)](obj) )
                if (other_immediate_lowtag_p(*obj) &&
                    __immobile_obj_gen_bits(obj) == PSEUDO_STATIC_GENERATION)
                    assign_generation(obj, HIGHEST_NORMAL_GENERATION);
            fixedobj_pages[page].gens = (mask & ~(1<<PSEUDO_STATIC_GENERATION))
                                        | 1<<HIGHEST_NORMAL_GENERATION;
        }
    }

    lispobj* obj = (lispobj*)VARYOBJ_SPACE_START;
    lispobj* limit = varyobj_free_pointer;
    for ( ; obj < limit ; obj += sizetab[widetag_of(obj)](obj) ) {
        if (__immobile_obj_gen_bits(obj) == PSEUDO_STATIC_GENERATION)
            assign_generation(obj, HIGHEST_NORMAL_GENERATION);
    }
    int max_page = find_varyobj_page_index(limit-1);
    for ( page = 0 ; page <= max_page ; ++page ) {
        int mask = varyobj_pages[page].generations;
        if (mask & (1<<PSEUDO_STATIC_GENERATION)) {
            varyobj_pages[page].generations
                = (mask & ~(1<<PSEUDO_STATIC_GENERATION))
                                      | 1<<HIGHEST_NORMAL_GENERATION;
        }
    }
}

int* code_component_order;

/* Defragment the immobile space, and then promote all objects to gen6.
 * 'coreparse' causes all pages in dynamic space to be pseudo-static, but
 * each immobile object stores its own generation, so this must be done at
 * save time, or else it would require touching every object on startup */
void prepare_immobile_space_for_save(lispobj init_function, boolean verbose)
{
    if (verbose) {
        printf("[defragmenting immobile space... ");
        fflush(stdout);
    }
    // Assert that defrag will not move the init_function
    gc_assert(!immobile_space_p(init_function));
    defrag_immobile_space(verbose);

    lispobj* obj = (lispobj*)FIXEDOBJ_SPACE_START;
    lispobj* limit = fixedobj_free_pointer;
    while (obj < limit) {
        if (other_immediate_lowtag_p(*obj))
            assign_generation(obj, PSEUDO_STATIC_GENERATION);
        obj += sizetab[widetag_of(obj)](obj);
    }

    obj = (lispobj*)VARYOBJ_SPACE_START;
    limit = varyobj_free_pointer;
    for ( varyobj_holes = 0 ;  obj < limit ; obj += sizetab[widetag_of(obj)](obj) ) {
        if (filler_obj_p(obj)) {
            struct code* code  = (struct code*)obj;
            code->debug_info = varyobj_holes;
            code->fixups     = 0;
            varyobj_holes    = (lispobj)code;
            // 0-fill the unused space.
            int nwords = sizetab[widetag_of(obj)](obj);
            memset(code->constants, 0,
                   (nwords * N_WORD_BYTES) - offsetof(struct code, constants));
        } else
            assign_generation(obj, PSEUDO_STATIC_GENERATION);
    }
    if (verbose) printf("done]\n");
}

//// Interface

int immobile_space_handle_wp_violation(void* fault_addr)
{
    low_page_index_t fixedobj_page_index = find_fixedobj_page_index(fault_addr);
    if (fixedobj_page_index < 0)
      return 0; // unhandled

    os_protect(PTR_ALIGN_DOWN(fault_addr, IMMOBILE_CARD_BYTES),
               IMMOBILE_CARD_BYTES, OS_VM_PROT_ALL);

    // FIXME: the _CLEARED flag doesn't achieve much if anything.
    if (!(fixedobj_pages[fixedobj_page_index].attr.parts.flags
          & (WRITE_PROTECT|WRITE_PROTECT_CLEARED)))
        return 0;
    SET_WP_FLAG(fixedobj_page_index, WRITE_PROTECT_CLEARED);

    return 1;
}

// Find the object that encloses pointer.
lispobj *
search_immobile_space(void *pointer)
{
    lispobj *start;

    if ((void*)VARYOBJ_SPACE_START <= pointer
        && pointer < (void*)varyobj_free_pointer) {
        low_page_index_t page_index = find_varyobj_page_index(pointer);
        if (page_attributes_valid) {
            start = varyobj_scan_start(page_index);
            if (start > (lispobj*)pointer) return NULL;
        } else {
            start = (lispobj*)VARYOBJ_SPACE_START;
        }
        lispobj* found = gc_search_space(start, pointer);
        return (found && filler_obj_p(found)) ? 0 : found;
    } else if ((void*)FIXEDOBJ_SPACE_START <= pointer
               && pointer < (void*)fixedobj_free_pointer) {
        low_page_index_t page_index = find_fixedobj_page_index(pointer);
        char *page_base = PTR_ALIGN_DOWN(pointer, IMMOBILE_CARD_BYTES);
        // Page attributes are inadequate to represent the size of objects
        // on the reserved page (of which there is only 1 at present).
        // In addition, there are actually differently sized objects on the page.
        // We don't really care about dissimilar sizes, since it is not used as
        // a root for scavenging. It resembles READ-ONLY space in that regard.
        if (page_attributes_valid && page_index >= FIXEDOBJ_RESERVED_PAGES) {
            int spacing = fixedobj_page_obj_align(page_index);
            int index = ((char*)pointer - page_base) / spacing;
            char *begin = page_base + spacing * index;
            char *end = begin + (fixedobj_page_obj_size(page_index) << WORD_SHIFT);
            if ((char*)pointer < end) return (lispobj*)begin;
        } else {
            return gc_search_space((lispobj*)page_base, pointer);
        }
    }
    return NULL;
}

// For coalescing holes, we need to scan backwards, which is done by
// looking backwards for a page that contains the start of a
// block of objects one of which must abut 'obj'.
lispobj* find_preceding_object(lispobj* obj)
{
  int page = find_varyobj_page_index(obj);
  gc_assert(page >= 0);
  while (1) {
      int offset = varyobj_pages[page].scan_start_offset;
      if (offset) { // 0 means the page is empty.
          lispobj* start = varyobj_scan_start(page);
          if (start < obj) { // Scan from here forward
              while (1) {
                  lispobj* end = start + sizetab[widetag_of(start)](start);
                  if (end == obj) return start;
                  gc_assert(end < obj);
                  start = end;
              }
          }
      }
      if (page == 0) {
          gc_assert(obj == varyobj_page_address(0));
          return 0; // Predecessor does not exist
      }
      --page;
  }
}

lispobj* AMD64_SYSV_ABI alloc_fixedobj(int nwords, uword_t header)
{
    return alloc_immobile_obj(MAKE_ATTR(ALIGN_UP(nwords,2), // spacing
                                        ALIGN_UP(nwords,2), // size
                                        0),
                              header);
}

lispobj AMD64_SYSV_ABI alloc_layout()
{
    const int LAYOUT_SIZE = sizeof (struct layout)/N_WORD_BYTES;
    lispobj* l =
        alloc_immobile_obj(MAKE_ATTR(LAYOUT_ALIGN / N_WORD_BYTES,
                                     ALIGN_UP(LAYOUT_SIZE,2),
                                     0),
                           (LAYOUT_SIZE-1)<<N_WIDETAG_BITS | INSTANCE_WIDETAG);
    return make_lispobj(l, INSTANCE_POINTER_LOWTAG);
}

// FIXME: Figure out not to hardcode
#define FUN_TRAMP_SIZE 6
#define GF_SIZE 6

//// Defragmentation

static struct {
  char* start;
  int n_bytes;
} fixedobj_tempspace, varyobj_tempspace;

// Given an address in the target core, return the equivalent
// physical address to read or write during defragmentation
static lispobj* tempspace_addr(void* address)
{
    gc_assert(immobile_space_p((lispobj)address));
    if (find_fixedobj_page_index(address) >= 0) {
        if (fixedobj_tempspace.n_bytes == 0) return address;
        int byte_index = (char*)address - (char*)FIXEDOBJ_SPACE_START;
        gc_assert(byte_index < fixedobj_tempspace.n_bytes);
        return (void*)(fixedobj_tempspace.start + byte_index);
    } else { // varyobj subspace
        int byte_index = (char*)address - (char*)VARYOBJ_SPACE_START;
        gc_assert(byte_index < varyobj_tempspace.n_bytes);
        return (void*)(varyobj_tempspace.start + byte_index);
    }
}

static inline boolean tempspace_p(char* addr)
{
    return (addr >= fixedobj_tempspace.start &&
            addr < fixedobj_tempspace.start + fixedobj_tempspace.n_bytes)
        || (addr >= varyobj_tempspace.start &&
            addr < varyobj_tempspace.start + varyobj_tempspace.n_bytes);
}
static inline boolean known_space_p(lispobj ptr)
{
    return find_page_index((char*)ptr) >= 0
        || tempspace_p((char*)ptr)
        || immobile_space_p(ptr)
        || (STATIC_SPACE_START <= ptr && ptr < STATIC_SPACE_END);
}
static boolean forwardable_ptr_p(lispobj ptr)
{
    return is_lisp_pointer(ptr) &&
           known_space_p(ptr) &&
           forwarding_pointer_p(native_pointer(ptr));
}

static void adjust_words(lispobj *where, sword_t n_words,
                         uword_t __attribute__((unused)) arg)
{
    int i;
    for (i=0;i<n_words;++i) {
        lispobj ptr = where[i];
        if (forwardable_ptr_p(ptr))
            where[i] = forwarding_pointer_value(native_pointer(ptr));
    }
}

static lispobj adjust_fun_entrypoint(lispobj raw_addr)
{
    // closure tramp and fin tramp don't have a simple-fun header.
    // Do not examine the word where the header would be,
    // since it could confuse adjust_words() by having a bit pattern
    // resembling a FP. (It doesn't, but better safe than sorry)
    if (asm_routines_start <= raw_addr && raw_addr < asm_routines_end)
        return raw_addr;
    lispobj simple_fun = raw_addr - FUN_RAW_ADDR_OFFSET;
    adjust_words(&simple_fun, 1, 0);
    return simple_fun + FUN_RAW_ADDR_OFFSET;
}

static void adjust_fdefn_raw_addr(struct fdefn* fdefn)
{
  if (!fdefn->raw_addr || points_to_asm_code_p((lispobj)fdefn->raw_addr))
      return;
  lispobj* raw_addr = (lispobj*)fdefn->raw_addr;
  lispobj* obj_base = 0;
  lispobj header;
  int i;
  for (i=1; i<=4; ++i)
      if ((header = raw_addr[-i]) == 1 || other_immediate_lowtag_p(header)) {
          obj_base = raw_addr-i;
          break;
      }
  gc_assert(obj_base);
  int offset = (char*)raw_addr - (char*)obj_base;
  if (header == 1) {
      char* new = (char*)native_pointer(forwarding_pointer_value(obj_base));
      fdefn->raw_addr = new + offset;
  }
}

/* Fix the layout of OBJ, and return the layout's address in tempspace.
 * If compact headers, store the layout back into the object.
 * If non-compact headers, DO NOT store the layout back into the object,
 * because that will be done when instance_scan() touches all slots.
 * If it were wrongly done now, then the following (real example) happens:
 *   instance @ 0x1000000000 has layout pointer 0x203cb483.
 *   layout @ 0x203cb483 forwards to 0x2030c483.
 *   object _currently_ at 0x2030c480 (NOT a layout) forwards to 0x203c39cf.
 * so the instance winds up with a non-layout in its layout after
 * instance_scan() forwards that slot "again". */
static struct layout* fix_object_layout(lispobj* obj)
{
    // This works on instances, funcallable instances (and/or closures)
    // but the latter only if the layout is in the header word.
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    gc_assert(widetag_of(obj) == INSTANCE_WIDETAG
              || widetag_of(obj) == FUNCALLABLE_INSTANCE_WIDETAG
              || widetag_of(obj) == CLOSURE_WIDETAG);
#else
    gc_assert(widetag_of(obj) == INSTANCE_WIDETAG);
#endif
    lispobj layout = instance_layout(obj);
    if (layout == 0) return 0;
    if (forwarding_pointer_p(native_pointer(layout))) { // usually
        layout = forwarding_pointer_value(native_pointer(layout));
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        instance_layout(obj) = layout;
#endif
    }
    struct layout* native_layout = (struct layout*)tempspace_addr(LAYOUT(layout));
    gc_assert(widetag_of(&native_layout->header) == INSTANCE_WIDETAG);
    gc_assert(instance_layout((lispobj*)native_layout) == LAYOUT_OF_LAYOUT);
    return native_layout;
}

static lispobj follow_fp(lispobj ptr)
{
  if (forwarding_pointer_p(native_pointer(ptr)))
      return forwarding_pointer_value(native_pointer(ptr));
  else
      return ptr;
}
static void apply_absolute_fixups(lispobj, struct code*);

/// It's tricky to try to use the scavtab[] functions for fixing up moved
/// objects, because scavenger functions might invoke transport functions.
/// The best approach is to do an explicit switch over all object types.
#include "genesis/hash-table.h"
static void fixup_space(lispobj* where, size_t n_words)
{
    lispobj* end = where + n_words;
    lispobj header_word;
    int widetag;
    long size;
    int __attribute__((unused)) static_space_p = ((lispobj)where == STATIC_SPACE_START);
    struct code* code;

    while (where < end) {
        gc_assert(!forwarding_pointer_p(where));
        header_word = *where;
        if (is_cons_half(header_word)) {
            adjust_words(where, 2, 0); // A cons.
            where += 2;
            continue;
        }
        widetag = header_widetag(header_word);
        size = sizetab[widetag](where);
        switch (widetag) {
        default:
          if (!leaf_obj_widetag_p(widetag))
            lose("Unhandled widetag in fixup_space: %p\n", (void*)header_word);
          break;
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        case FUNCALLABLE_INSTANCE_WIDETAG:
#endif
        case INSTANCE_WIDETAG:
          instance_scan(adjust_words, where+1,
                        instance_length(header_word) | 1,
                        fix_object_layout(where)->bitmap,
                        0);
          break;
        case CODE_HEADER_WIDETAG:
          // Fixup the constant pool.
          code = (struct code*)where;
          adjust_words(where+2, code_header_words(code)-2, 0);
          // Fixup all embedded simple-funs
          for_each_simple_fun(i, f, code, 1, {
              f->self = adjust_fun_entrypoint(f->self);
          });
          apply_absolute_fixups(code->fixups, code);
          break;
        case CLOSURE_WIDETAG:
          where[1] = adjust_fun_entrypoint(where[1]);
          // FALLTHROUGH_INTENDED
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        case FUNCALLABLE_INSTANCE_WIDETAG:
#endif
          // skip the trampoline word at where[1]
          adjust_words(where+2, size-2, 0);
          break;
        case FDEFN_WIDETAG:
          adjust_words(where+1, 2, 0);
          adjust_fdefn_raw_addr((struct fdefn*)where);
          break;

        // Special case because we might need to mark hashtables
        // as needing rehash.
        case SIMPLE_VECTOR_WIDETAG:
          if (is_vector_subtype(header_word, VectorAddrHashing)) {
              struct vector* kv_vector = (struct vector*)where;
              lispobj* data = kv_vector->data;
              gc_assert(kv_vector->length >= 5);
              boolean needs_rehash = 0;
              unsigned int hwm = KV_PAIRS_HIGH_WATER_MARK(data);
              unsigned int i;
              // FIXME: we're disregarding the hash vector.
              for (i = 1; i <= hwm; ++i) {
                  lispobj ptr = data[2*i];
                  if (forwardable_ptr_p(ptr)) {
                      data[2*i] = forwarding_pointer_value(native_pointer(ptr));
                      needs_rehash = 1;
                  }
                  ptr = data[2*i+1];
                  if (forwardable_ptr_p(ptr))
                      data[2*i+1] = forwarding_pointer_value(native_pointer(ptr));
              }
              if (needs_rehash)
                  KV_PAIRS_REHASH(data) |= make_fixnum(1);
              break;
          } else {
            // FALLTHROUGH_INTENDED
          }
        // All the other array header widetags.
        case SIMPLE_ARRAY_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
        case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
        case COMPLEX_BASE_STRING_WIDETAG:
        case COMPLEX_VECTOR_NIL_WIDETAG:
        case COMPLEX_BIT_VECTOR_WIDETAG:
        case COMPLEX_VECTOR_WIDETAG:
        case COMPLEX_ARRAY_WIDETAG:
        // And the other entirely boxed objects.
        case SYMBOL_WIDETAG:
        case VALUE_CELL_WIDETAG:
        case WEAK_POINTER_WIDETAG:
        case RATIO_WIDETAG:
        case COMPLEX_WIDETAG:
          // Use the sizing functions for generality.
          // Symbols can contain strange header bytes,
          // and vectors might have a padding word, etc.
          adjust_words(where+1, size-1, 0);
          break;
        }
        where += size;
    }
}

int* immobile_space_reloc_index;
int* immobile_space_relocs;

// Take and return an untagged pointer, or 0 if the object did not survive GC.
static lispobj* get_load_address(lispobj* old)
{
    if (forwarding_pointer_p(old))
        return native_pointer(forwarding_pointer_value(old));
    gc_assert(filler_obj_p(old));
    return 0;
}

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
// This does not accept (SIMPLE-ARRAY NIL (*))
// (You'd have a pretty bad time trying making a symbol like that)
static int schar(struct vector* string, int index)
{
#ifdef LISP_FEATURE_SB_UNICODE
    if (widetag_of(&string->header) == SIMPLE_CHARACTER_STRING_WIDETAG)
        return ((int*)string->data)[index];
#endif
    return ((char*)string->data)[index];
}

#include "genesis/package.h"
#define N_SYMBOL_KINDS 5

// Return an integer 0..4 telling which block of symbols to relocate 'sym' into.
//  0=uninterned, 1=keyword, 2=special operator with extra slot,
//  3=special var, 4=other
static int classify_symbol(lispobj* obj)
{
  struct symbol* symbol = (struct symbol*)obj;
  if (symbol->package == NIL) return 0;
  struct vector* package_name = (struct vector*)
    native_pointer(((struct package*)native_pointer(symbol->package))->_name);
  if (widetag_of(&package_name->header) == SIMPLE_BASE_STRING_WIDETAG
      && !strcmp((char*)package_name->data, "KEYWORD"))
      return 1;
  // Same criterion as SYMBOL-EXTRA-SLOT-P in src/code/room.
  if ((HeaderValue(*obj) & 0xFF) > (SYMBOL_SIZE-1))
      return 2;
  struct vector* symbol_name = VECTOR(symbol->name);
  if (symbol_name->length >= make_fixnum(2) &&
      schar(symbol_name, 0) == '*' &&
      schar(symbol_name, fixnum_value(symbol_name->length)-1) == '*')
      return 3;
  return 4;
}

static inline char* compute_defrag_start_address()
{
    // The first fixedobj page contains some essential layouts,
    // the addresses of which might be wired in by code generation.
    // As such they must never move.
    return (char*)FIXEDOBJ_SPACE_START + 2*IMMOBILE_CARD_BYTES;

}

static int calc_n_fixedobj_pages(int n_objects, int words_per_object)
{
  words_per_object = ALIGN_UP(words_per_object, 2);
  int objects_per_page = WORDS_PER_PAGE / words_per_object;
  return (n_objects + objects_per_page - 1) / objects_per_page;
}
static int calc_n_varyobj_pages(int n_objects, int words_per_object)
{
  words_per_object = ALIGN_UP(words_per_object, 2);
  int objects_per_page = WORDS_PER_PAGE / words_per_object;
  // varyobj space can't have unused bytes, but filler objects
  // require at least 4 words. So if there are only 2 words
  // of trailing fluff, cut out one object.
  // This could be improved by using 'filler' type object (not code)
  // which stores the length in the header word, leaving one word
  // available in which to store a pointer to the next filler object.
  if (WORDS_PER_PAGE - (words_per_object * objects_per_page) == 2)
      --objects_per_page;
  return (n_objects + objects_per_page - 1) / objects_per_page;
}

/// Copy a fixed-size object to a variable-sized object page,
/// such that the copy never crosses page boundaries.
/// The original object may reside in either varyobj or fixedobj space.
static void place_fixedobj(lispobj* obj, int size_in_bytes,
                           char *alloc_ptrs[64], char *symbol_alloc_ptr[])
{
      lispobj word = *obj;
      int widetag = header_widetag(word);
      char** alloc_ptr;
      if (widetag == SYMBOL_WIDETAG)
          alloc_ptr = &symbol_alloc_ptr[classify_symbol(obj)];
      else if (!(alloc_ptr = &alloc_ptrs[widetag>>2]))
          lose("Unexpected widetag %x", widetag);
      char* new = *alloc_ptr;
      char* page_end =
          PTR_ALIGN_DOWN(new, IMMOBILE_CARD_BYTES) + IMMOBILE_CARD_BYTES;
      // compute space remaining, possibly negative,
      // if this object were placed on the page.
      int space_avail = (page_end - new) - size_in_bytes;
      if (widetag == FDEFN_WIDETAG || widetag == CODE_HEADER_WIDETAG
          || widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
          // transporting into varyobj space.
          // since filler objects occupy >= 4 words, consider the page
          // to be overfull if exactly 2 words would remain.
          if (space_avail < 0 || space_avail == 2*N_WORD_BYTES) {
              make_filler(tempspace_addr(new), page_end - new);
              new = page_end;
          }
      } else {
          if (space_avail < 0)
            new = page_end;
      }
      gc_assert(!*tempspace_addr(new)); // better not clobber some other object
      memcpy(tempspace_addr(new), obj, size_in_bytes);
      set_forwarding_pointer(obj, make_lispobj(new, lowtag_for_widetag[widetag>>2]));
      if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) // fix the self-pointer
          tempspace_addr(new)[1] = (lispobj)(new + 4*N_WORD_BYTES);
      *alloc_ptr = new + size_in_bytes;
}

static void add_filler_if_needed(char* from, char* to)
{
    if (to>from)
        make_filler(tempspace_addr(from), to-from);
}

static boolean executable_object_p(lispobj* obj)
{
    int widetag = widetag_of(obj);
    return widetag == FDEFN_WIDETAG ||
        (widetag == CODE_HEADER_WIDETAG && code_header_words((struct code*)obj) >= 3
         && lowtag_of(((struct code*)obj)->debug_info) == FUN_POINTER_LOWTAG)
        || widetag == FUNCALLABLE_INSTANCE_WIDETAG;
}

static void defrag_immobile_space(boolean verbose)
{
    lispobj* addr;
    int i;

    int *components = code_component_order;

    // Count the number of symbols, fdefns, and layouts that will be relocated
    int obj_type_histo[64];
    struct { int size, count; } sym_kind_histo[N_SYMBOL_KINDS];
    bzero(obj_type_histo, sizeof obj_type_histo);
    bzero(sym_kind_histo, sizeof sym_kind_histo);

    // Count the fdefns, trampolines, and GFs already in varyobj sapace.
    // There are 3 kinds of code objects we'll see -
    // (1) ordinary code, (2) trampolines, (3) filler
    // Make sure only to count the trampolines on this pass.
    lispobj* obj = (lispobj*)VARYOBJ_SPACE_START;
    while (obj < varyobj_free_pointer) {
        int widetag = widetag_of(obj);
        if (executable_object_p(obj)) ++obj_type_histo[widetag/4];
        obj += sizetab[widetag](obj);
    }

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    // Find the starting address of fixed-size objects that will undergo defrag.
    // Never move the first pages of LAYOUTs created by genesis.
    // so that codegen can wire in layout of function with less pain.
    char* defrag_base = compute_defrag_start_address();
    low_page_index_t page_index = find_fixedobj_page_index(defrag_base);
    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for ( ; page_index <= max_used_fixedobj_page ; ++page_index) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        if (obj_spacing) {
            lispobj* obj = fixedobj_page_address(page_index);
            lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
            do {
                lispobj word = *obj;
                if (!fixnump(word)) {
                    int widetag = header_widetag(word);
                    ++obj_type_histo[widetag/4];
                    if (widetag == SYMBOL_WIDETAG) {
                        int kind = classify_symbol(obj);
                        int size = sizetab[widetag](obj);
                        ++sym_kind_histo[kind].count;
                        if (!sym_kind_histo[kind].size)
                            sym_kind_histo[kind].size = size;
                        gc_assert(sym_kind_histo[kind].size == size);
                    }
                }
            } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
        }
    }
    gc_assert(obj_type_histo[INSTANCE_WIDETAG/4]);

    // Calculate space needed for fixedobj pages after defrag.
    // page order is: layouts, symbols.
    // FDEFNs, trampolines, and GFs are moved into varyobj space.
    int n_layout_pages = calc_n_fixedobj_pages(
        obj_type_histo[INSTANCE_WIDETAG/4],
        LAYOUT_ALIGN / N_WORD_BYTES);
    char* layout_alloc_ptr = defrag_base;
    char* symbol_alloc_ptrs[N_SYMBOL_KINDS+1];
    symbol_alloc_ptrs[0]    = layout_alloc_ptr + n_layout_pages * IMMOBILE_CARD_BYTES;
    for (i=0; i<N_SYMBOL_KINDS ; ++i)
      symbol_alloc_ptrs[i+1] =
          symbol_alloc_ptrs[i] + calc_n_fixedobj_pages(
              sym_kind_histo[i].count, sym_kind_histo[i].size) * IMMOBILE_CARD_BYTES;
    char* ending_alloc_ptr = symbol_alloc_ptrs[N_SYMBOL_KINDS];

    fixedobj_tempspace.n_bytes = ending_alloc_ptr - (char*)FIXEDOBJ_SPACE_START;
    fixedobj_tempspace.start = calloc(fixedobj_tempspace.n_bytes, 1);

    // Copy pages below the defrag base into the temporary copy.
    memcpy(fixedobj_tempspace.start, (char*)FIXEDOBJ_SPACE_START,
           (lispobj)defrag_base - FIXEDOBJ_SPACE_START);
#endif

    // Compute where each code component will be moved to.
    int n_code_components = 0;
    int n_code_bytes = 0;

    if (components) {
        for (i=0 ; components[i*2] ; ++i) {
            addr = (lispobj*)(long)components[i*2];
            gc_assert(lowtag_of((lispobj)addr) == OTHER_POINTER_LOWTAG);
            addr = native_pointer((lispobj)addr);
            int widetag = widetag_of(addr);
            gc_assert(widetag == CODE_HEADER_WIDETAG);
            lispobj new_vaddr = 0;
            // A code component can become garbage in the final GC
            // (defrag happens after the last GC) leaving a filler object
            // which was in components[] because it was live before GC.
            if (!filler_obj_p(addr)) {
                // must not be a trampoline object
                if ((lispobj)addr > VARYOBJ_SPACE_START)
                    gc_assert(code_n_funs((struct code*)addr));
                ++n_code_components;
                new_vaddr = VARYOBJ_SPACE_START + n_code_bytes;
                n_code_bytes += sizetab[widetag](addr) << WORD_SHIFT;
            }
            components[i*2+1] = new_vaddr;
        }
    }
    int aligned_nbytes = ALIGN_UP(n_code_bytes, IMMOBILE_CARD_BYTES);
    if (aligned_nbytes - n_code_bytes == 2 * N_WORD_BYTES)
        // waste another page because it can't be a 2-word filler
        aligned_nbytes += IMMOBILE_CARD_BYTES;
    char* fdefn_alloc_ptr = (char*)VARYOBJ_SPACE_START + aligned_nbytes;

    int n_fdefn_pages =
        calc_n_varyobj_pages(obj_type_histo[FDEFN_WIDETAG/4],
                             FDEFN_SIZE);
    int n_gf_pages    =
        calc_n_varyobj_pages(obj_type_histo[FUNCALLABLE_INSTANCE_WIDETAG/4],
                             GF_SIZE);
    int n_tramp_pages =
        calc_n_varyobj_pages(obj_type_histo[CODE_HEADER_WIDETAG/4],
                             FUN_TRAMP_SIZE);

    char* tramp_alloc_ptr  = fdefn_alloc_ptr + n_fdefn_pages * IMMOBILE_CARD_BYTES;
    char* gf_alloc_ptr     = tramp_alloc_ptr + n_tramp_pages * IMMOBILE_CARD_BYTES;
    ending_alloc_ptr       = gf_alloc_ptr + n_gf_pages * IMMOBILE_CARD_BYTES;
    varyobj_tempspace.n_bytes = ending_alloc_ptr - (char*)VARYOBJ_SPACE_START;
    varyobj_tempspace.start = calloc(varyobj_tempspace.n_bytes, 1);

    if (verbose)
        printf("(fin,inst,fdefn,code,sym)=%d+%d+%d+%d+%d... ",
               obj_type_histo[FUNCALLABLE_INSTANCE_WIDETAG/4],
               obj_type_histo[INSTANCE_WIDETAG/4],
               obj_type_histo[FDEFN_WIDETAG/4],
               obj_type_histo[CODE_HEADER_WIDETAG/4] +  n_code_components,
               obj_type_histo[SYMBOL_WIDETAG/4]);

    if (components) {
        // Permute varyobj space into tempspace and deposit forwarding pointers.
        lispobj new_vaddr;
        for (i=0 ; components[i*2] ; ++i) {
            if ((new_vaddr = components[i*2+1]) != 0) {
                addr = native_pointer(components[i*2]);
                memcpy(tempspace_addr((void*)new_vaddr), addr,
                       sizetab[widetag_of(addr)](addr) << WORD_SHIFT);
                int displacement = new_vaddr - (lispobj)addr;
                switch (widetag_of(addr)) {
                case CODE_HEADER_WIDETAG:
                    for_each_simple_fun(index, fun, (struct code*)addr, 1, {
                        set_forwarding_pointer((lispobj*)fun,
                                               make_lispobj((char*)fun + displacement,
                                                            FUN_POINTER_LOWTAG));
                    });
                    break;
                }
                set_forwarding_pointer(addr,
                                       make_lispobj((void*)new_vaddr,
                                                    OTHER_POINTER_LOWTAG));
            }
        }
        if (aligned_nbytes > n_code_bytes)
            make_filler(tempspace_addr((char*)VARYOBJ_SPACE_START + n_code_bytes),
                        aligned_nbytes - n_code_bytes);
    }

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    char* alloc_ptrs[64];
    bzero(alloc_ptrs, sizeof alloc_ptrs);
    alloc_ptrs[INSTANCE_WIDETAG/4] = layout_alloc_ptr;
    alloc_ptrs[FDEFN_WIDETAG/4] = fdefn_alloc_ptr;
    alloc_ptrs[CODE_HEADER_WIDETAG/4] = tramp_alloc_ptr;
    alloc_ptrs[FUNCALLABLE_INSTANCE_WIDETAG/4] = gf_alloc_ptr;

    // Copy fixed-sized objects that were already in code space
    // to a possibly new address in code space.
    obj = (lispobj*)VARYOBJ_SPACE_START;
    while (obj < varyobj_free_pointer) {
        // Use the forwarded object if forwarded, because the header was stomped on.
        // This handles the code objects that were already forwarded.
        lispobj* fwdobj =
            forwarding_pointer_p(obj) ?
            tempspace_addr(native_pointer(forwarding_pointer_value(obj))) : obj;
        int size = sizetab[widetag_of(fwdobj)](fwdobj);
        // Supplying symbol_kind as 0 is fine - symbols can't exist in code space.
        if (executable_object_p(fwdobj))
            place_fixedobj(obj, size << WORD_SHIFT, alloc_ptrs, 0);
        obj += size;
    }
    // Permute fixed-sized object pages and deposit forwarding pointers.
    for ( page_index = find_fixedobj_page_index(defrag_base) ;
          page_index <= max_used_fixedobj_page ; ++page_index) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        if (!obj_spacing) continue;
        lispobj* obj = fixedobj_page_address(page_index);
        lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
        do {
            if (fixnump(*obj)) continue;
            place_fixedobj(obj, obj_spacing, alloc_ptrs, symbol_alloc_ptrs);
         } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }
    // Might require filler between inter-object-type gaps
    add_filler_if_needed(alloc_ptrs[FDEFN_WIDETAG/4], tramp_alloc_ptr);
    add_filler_if_needed(alloc_ptrs[CODE_HEADER_WIDETAG/4], gf_alloc_ptr);
#endif  /* DEFRAGMENT_FIXEDOBJ_SUBSPACE */

    if (immobile_space_reloc_index) {
#ifdef LISP_FEATURE_X86_64
        // Fix displacements in JMP, CALL, and LEA instructions in code objects.
        // There are 2 arrays in use:
        //  - the relocs[] array contains the offset of any machine instruction
        //    that needs to be altered on account of space relocation. Offsets are
        //    relative to base address of code, not the code-instructions.
        //    After the offset is the lisp object referred to by the instruction.
        //  - the reloc_index[] array identifes the code component each reloc belongs to.
        //    It is an array of pairs:
        //      comp_ptr1, index1, comp_ptr2, index2 ... comp_ptrN, indexN, 0, index_max
        //    The index following a component specifies the starting index within the
        //    relocs[] array of the first reloc belonging to the respective component.
        //    The ending reloc can be deduced from the next component's first reloc.
        for (i = 0 ; immobile_space_reloc_index[i*2] ; ++i) {
            lispobj code = immobile_space_reloc_index[i*2] - OTHER_POINTER_LOWTAG;
            lispobj load_addr;
            if (code >= READ_ONLY_SPACE_START && code < READ_ONLY_SPACE_END)
                load_addr = code; // This code can not be moved or GCed.
            else
                load_addr = (lispobj)get_load_address((lispobj*)code);
            if (!load_addr) continue;  // Skip code that was dropped by GC
            int reloc_index     = immobile_space_reloc_index[i*2+1];
            int end_reloc_index = immobile_space_reloc_index[i*2+3];
            for ( ; reloc_index < end_reloc_index ; reloc_index += 2) {
                int offset = immobile_space_relocs[reloc_index];
                char* inst_addr = (char*)code + offset;
                // Both this code and the referenced code can move.
                // For this component, adjust by the displacement by (old - new).
                // If the jump target moved, also adjust by its (new - old).
                // The target object can be a simple-fun, funcallable instance,
                // fdefn, or 0 if it's an assembly routine call.
                int target_adjust = 0;
                lispobj obj = immobile_space_relocs[reloc_index+1];
                if (obj) {
                    lispobj *npobj = native_pointer(obj);
                    if (forwarding_pointer_p(npobj))
                        target_adjust = forwarding_pointer_value(npobj) - obj;
                }
                // If the instruction to fix has moved, then adjust for
                // its new address, and perform the fixup in tempspace.
                // Otherwise perform the fixup where the instruction is now.
                // (It only wouldn't move if it's asm code in readonly space)
                char* fixup_loc =
                    immobile_space_p((lispobj)inst_addr) ?
                    (char*)tempspace_addr(inst_addr - code + load_addr) : inst_addr;
                UNALIGNED_STORE32(fixup_loc,
                                  UNALIGNED_LOAD32(fixup_loc)
                                    + target_adjust + (code - load_addr));
            }
        }
#endif
        free(immobile_space_relocs);
        free(immobile_space_reloc_index);
    }

    // Fix Lisp pointers in static, immobile, and dynamic spaces
    fixup_space((lispobj*)STATIC_SPACE_START,
                static_space_free_pointer - (lispobj*)STATIC_SPACE_START);

    // Objects in immobile space are physically at 'tempspace',
    // but logically at their natural address. Perform fixups
    // at their current physical address.
#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    fixup_space((lispobj*)fixedobj_tempspace.start,
                fixedobj_tempspace.n_bytes >> WORD_SHIFT);
#else
    fixup_space((lispobj*)FIXEDOBJ_SPACE_START,
                FIXEDOBJ_SPACE_SIZE >> WORD_SHIFT);
#endif
    fixup_space((lispobj*)varyobj_tempspace.start,
                varyobj_tempspace.n_bytes >> WORD_SHIFT);

    // Dynamic space
    // We can safely ignore allocation region boundaries.
    fixup_space(current_dynamic_space,
                (lispobj*)get_alloc_pointer() - current_dynamic_space);

    // Copy the spaces back where they belong.

    lispobj old_free_ptr;
    lispobj free_ptr;
#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    memcpy((void*)FIXEDOBJ_SPACE_START, fixedobj_tempspace.start,
           fixedobj_tempspace.n_bytes);
    // Zero-fill the unused remainder
    old_free_ptr = (lispobj)fixedobj_free_pointer;
    free_ptr = FIXEDOBJ_SPACE_START + fixedobj_tempspace.n_bytes;
    bzero((char*)free_ptr, old_free_ptr - free_ptr);
    fixedobj_free_pointer = (lispobj*)free_ptr;
#endif

#ifdef LISP_FEATURE_IMMOBILE_CODE
    // Variable-size object pages.
    free_ptr = (lispobj)alloc_ptrs[FUNCALLABLE_INSTANCE_WIDETAG/4];
    memcpy((char*)VARYOBJ_SPACE_START, varyobj_tempspace.start,
           free_ptr-VARYOBJ_SPACE_START);
    // Zero-fill the unused remainder
    old_free_ptr = (lispobj)varyobj_free_pointer;
    // varyobj space can have *more* bytes in it after defrag than before,
    // as we might have transported fdefns (etc) into it.
    if (free_ptr < old_free_ptr)
        bzero((char*)free_ptr, old_free_ptr - free_ptr);
    varyobj_free_pointer = (lispobj*)free_ptr;
    free(components);
#endif

    page_attributes_valid = 0;
#if 0
    // It's easy to mess things up, so assert correctness before saving a core.
    printf("verifying defrag\n");
    verify_heap(1);
#endif
    free(fixedobj_tempspace.start);
    free(varyobj_tempspace.start);
}
#endif

// Fixup immediate values that encode Lisp object addresses
// in immobile space. Process only the absolute fixups.
#include "forwarding-ptr.h"
#ifdef LISP_FEATURE_X86_64
static void apply_absolute_fixups(lispobj fixups, struct code* code)
{
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, fixups);
    char* instructions = code_text_start(code);
    int prev_loc = 0, loc;
    // The unpacker will produce successive values followed by a zero. There may
    // be a second data stream for the relative fixups which we ignore.
    while (varint_unpack(&unpacker, &loc) && loc != 0) {
        // For extra compactness, each loc is relative to the prior,
        // so that the magnitudes are smaller.
        loc += prev_loc;
        prev_loc = loc;
        void* fixup_where = instructions + loc;
        lispobj ptr = (lispobj)UNALIGNED_LOAD32(fixup_where);
        lispobj* header_addr;
        long fpval;

        if (is_lisp_pointer(ptr)) {
            lispobj fixed = follow_fp(ptr);
            if (fixed != ptr)
                UNALIGNED_STORE32(fixup_where, fixed);
            continue;
        }
        if (asm_routines_start <= ptr && ptr < asm_routines_end) {
            // Call to asm routine using "CALL [#xNNNN]" form.
            // This fixup is only for whole-heap relocation on startup.
            continue;
        }
        if (find_fixedobj_page_index((void*)ptr) >= 0) {
            header_addr = search_immobile_space((void*)ptr);
            gc_assert(header_addr);
            if (!forwarding_pointer_p(header_addr))
                continue;
            fpval = forwarding_pointer_value(header_addr);
            int widetag = widetag_of(tempspace_addr(native_pointer(fpval)));
            // Must be an interior pointer to a symbol value slot
            // or fdefn raw addr slot
            if (!(widetag == SYMBOL_WIDETAG || widetag == FDEFN_WIDETAG))
                lose("Expected symbol or fdefn @ %p", header_addr);
        } else {
            /* Dynamic space functions can call immobile space functions
             * and fdefns using the two-instruction sequence:
             *   MOV RAX, #x{addr} ; CALL RAX
             * where the addr is either word index 0 of an fdefn
             * (the jump instruction), or word index 2 of a simple-fun.
             * We have to heuristically figure out which it is.
             * If we started by assuming that it's a simple-fun then
             * we might go astray if it's an fdefn because we can't
             * look at negative word indices. */
            header_addr = (lispobj*)(ptr - 2);
            if (forwarding_pointer_p(header_addr)) {
                fpval = forwarding_pointer_value(header_addr);
                if (widetag_of(tempspace_addr(native_pointer(fpval))) == FDEFN_WIDETAG)
                    goto fix;
                lose("Expected fdefn @ %p", header_addr);
            }
            header_addr = (lispobj*)(ptr - offsetof(struct simple_fun, insts));
            if (forwarding_pointer_p(header_addr)) {
                fpval = forwarding_pointer_value(header_addr);
                if (widetag_of(tempspace_addr(native_pointer(fpval))) == SIMPLE_FUN_WIDETAG)
                    goto fix;
                lose("Expected simple-fun @ %p", header_addr);
            }
            lose("Can't determine referent of absolute fixup");
        }
  fix:  UNALIGNED_STORE32(fixup_where,
                          ptr - (lispobj)header_addr + (lispobj)native_pointer(fpval));
    }
}
#endif

#ifdef VERIFY_PAGE_GENS
void check_fixedobj_page(int page,
                         generation_index_t keep_gen,
                         generation_index_t new_gen)
{
  // Every page should have a 'gens' mask which exactly reflects
  // the aggregate over all objects on that page. Verify that invariant,
  // checking all pages, not just the ones below the free pointer.
  int genmask, obj_size, obj_spacing, i, all_ok = 1;
  lispobj *obj, header;
  int sees_younger = 0;

  obj_size = fixedobj_page_obj_size(page);
  obj_spacing = fixedobj_page_obj_align(page);
  obj = fixedobj_page_address(page);
  lispobj *limit = compute_fixedobj_limit(obj, obj_spacing);
  genmask = 0;
  if (obj_size == 0) {
      gc_assert(!fixedobj_pages[page].gens);
      for (i=0; i<WORDS_PER_PAGE && obj[i]==0; ++i)
          ;
      if(i<WORDS_PER_PAGE)
          lose("page %d @ %p nonempty", page, fixedobj_page_address(page));
      return;
  }
  do {
      header = *obj;
      if (!fixnump(header)) {
          int gen = __immobile_obj_gen_bits(obj);
          gc_assert(0 <= gen && gen <= PSEUDO_STATIC_GENERATION);
          genmask |= 1<<gen;
          if (fixedobj_points_to_younger_p(obj, obj_size, gen, keep_gen, new_gen)) {
            if (fixedobj_page_wp(page))
              lose("sees_younger @ %p + %d\n", obj, obj_size);
            sees_younger = 1;
          }
      }
  } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
  // It's not wrong if the gen0 bit is set spuriously, but it should only
  // happen at most once, on the first GC after image startup.
  // At all other times, the invariant should hold that if the freelist
  // indicated that space was available, and the new pointer differs,
  // then some gen0 object exists on the page.
  // The converse is true because of pseudo-atomicity of the allocator:
  // if some thread claimed a hole, then it also updated the freelist.
  // If it died before doing the latter, then the object allegedly created
  // was never really live, so won't contain any pointers.
  if (fixedobj_pages[page].gens != genmask
      && fixedobj_pages[page].gens != (genmask|1)) {
    fprintf(stderr, "Page %d @ %p: stored mask=%x actual=%x\n",
            page, fixedobj_page_address(page),
            fixedobj_pages[page].gens, genmask);
    all_ok = 0;
  }
  if (fixedobj_page_wp(page) && sees_younger) {
    fprintf(stderr, "Page %d @ %p: WP is wrong\n",
            page, fixedobj_page_address(page));
    all_ok = 0;
  }
  gc_assert(all_ok);
}

int n_immobile_objects;
int *immobile_objects, *immobile_objects_limit;

int comparator_eq(const void* a, const void* b) {
  return *(int*)a - *(int*)b;
}

// Find the largest item less than or equal.
// (useful for finding the object that contains a given pointer)
int comparator_le(const void* a, const void* b) {
  int diff = *(int*)a - *(int*)b;
  if (diff <= 0) return diff;
  // If looking to the right would see an item strictly greater
  // than the sought key, or there is nothing to the right,
  // then deem this an exact match.
  if (b == (void*)immobile_objects_limit || ((int*)b)[1] > *(int*)a) return 0;
  return 1;
}

// Find the smallest item greater than or equal.
// useful for finding the lowest item at or after a page base address.
int comparator_ge(const void* a, const void* b) {
  int diff = *(int*)a - *(int*)b;
  if (diff >= 0) return diff;
  // If looking to the left would see an item strictly less
  // than the sought key, or there is nothing to the left
  // then deem this an exact match.
  if (b == (void*)immobile_objects || ((int*)b)[-1] < *(int*)a) return 0;
  return -1;
}

void check_varyobj_pages()
{
  // 1. Check that a linear scan sees only valid object headers,
  //    and that it terminates exactly at IMMOBILE_CODE_FREE_POINTER.
  lispobj* obj = (lispobj*)VARYOBJ_SPACE_START;
  lispobj* end = varyobj_free_pointer;
  low_page_index_t end_page = find_varyobj_page_index((char*)end-1);

  n_immobile_objects = 0;
  while (obj < end) {
    lispobj word = *obj;
    gc_assert(other_immediate_lowtag_p(word));
    int n_words = sizetab[header_widetag(word)](obj);
    obj += n_words;
    ++n_immobile_objects;
  }
  gc_assert(obj == end);

  // 2. Check that all scan_start_offsets are plausible.
  // Begin by collecting all object header locations into an array;
  immobile_objects = calloc(n_immobile_objects, sizeof (lispobj));
  immobile_objects_limit = immobile_objects + n_immobile_objects - 1;
  obj = (lispobj*)VARYOBJ_SPACE_START;
  int i = 0;
  while (obj < end) {
    immobile_objects[i++] = (lispobj)obj;
    lispobj word = *obj;
    int n_words = sizetab[header_widetag(word)](obj);
    obj += n_words;
  }
  // Check that each page's scan start is a known immobile object
  // and that it is the right object.
  low_page_index_t page;
  for (page = 0; page <= end_page; ++page) {
    lispobj page_addr = (lispobj)varyobj_page_address(page);
    int* found_below = bsearch(&page_addr, immobile_objects, n_immobile_objects,
                                sizeof (int), comparator_le);
    int* found_above = bsearch(&page_addr, immobile_objects, n_immobile_objects,
                                sizeof (int), comparator_ge);
    int stored_scan_start = (int)(long)varyobj_scan_start(page);
    lispobj* scan_start_obj = (lispobj*)(long)*found_below;
    if (scan_start_obj != (lispobj*)(long)stored_scan_start) {
      //printf("page %d: found-below=%p stored=%p\n", page, scan_start_obj, stored_scan_start);
      while (filler_obj_p(scan_start_obj)) {
        int nwords = sizetab[widetag_of(scan_start_obj)](scan_start_obj);
        //        printf("skipping %d words to %p\n", nwords, scan_start_obj + nwords);
        scan_start_obj += nwords;
        // the stored scan start does not guarantee that it points
        // to a non-hole; we only assert that it *probably* does not.
        // As such, when computing the "correct" value, we allow
        // any value in between the legal bounding values for it.
        if ((int)(long)scan_start_obj == stored_scan_start)
          break;
        // If you hit the free pointer, or run off the page,
        // then the page is completely empty.
        if (scan_start_obj == varyobj_free_pointer
            || scan_start_obj >= (lispobj*)varyobj_page_address(page+1)) {
          scan_start_obj = varyobj_page_address(page+1);
          break;
        }
      }
    }
    if (scan_start_obj != (lispobj*)(long)stored_scan_start)
      lose("page %d: stored_scan_start=%p does not match found %p\n",
           page, stored_scan_start, *found_below);
    if (found_below != found_above) {
      // the object below must touch this page.
      // if it didn't, there should be a higher object below.
      lispobj* below = (lispobj*)(long)*found_below;
      int n_words = sizetab[header_widetag(*below)](below);
      lispobj* end = below + n_words;
      gc_assert(end > (lispobj*)page_addr);
    }
  }
  free(immobile_objects);

  // 3. The generation mask for each page is exactly the union
  //    of generation numbers of object headers on the page.
  for (page = 0; page <= end_page; ++page) {
      if (!varyobj_page_scan_start_offset[page])
        continue; // page is all holes or never used
      obj = varyobj_scan_start(page);
      lispobj word = *obj;
      int n_words = sizetab[header_widetag(word)](obj);
      // Skip the first object if it doesn't start on this page.
      if (obj < (lispobj*)varyobj_page_address(page)) obj += n_words;
      lispobj* limit = (lispobj*)varyobj_page_address(page) + WORDS_PER_PAGE;
      lispobj* freeptr = varyobj_free_pointer;
      if (limit > freeptr) limit = freeptr;
      int mask = 0;
      for ( ; obj < limit ; obj += sizetab[widetag_of(obj)](obj) ) {
          int gen = __immobile_obj_gen_bits(obj);
          if (filler_obj_p(obj)) {
              gc_assert(gen == 0);
          } else {
              gc_assert(0 <= gen && gen <= PSEUDO_STATIC_GENERATION);
              mask |= 1 << gen;
          }
      }
      /* This assertion fails after a fullcgc which doesn't update
       * the genmasks, so that they remain as overestimates */
      // gc_assert(mask == varyobj_page_gens[page]);

      int actual = varyobj_page_gens[page];
      /* Fail if any bit in (LOGANDC1 ACTUAL EXPECTED) is true:
       *    actual=0, expected=0 -> // ok
       *    actual=1, expected=1 -> // ok
       *    actual=1, expected=0 -> // ok
       *    actual=0, expected=1 -> // NOT ok
       */
      if (~actual & mask)
          lose("genmask wrong: actual=%x expect=%x\n", actual, mask);
  }
}
#endif
