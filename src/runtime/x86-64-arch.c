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

#include <stdio.h>

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "sbcl.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "unaligned.h"
#include "search.h"
#include "globals.h" // for asm_routines_start,end

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "forwarding-ptr.h"
#include "core.h"

#ifdef LISP_FEATURE_UD2_BREAKPOINTS
#define UD2_INST 0x0b0f         /* UD2 */
#define BREAKPOINT_WIDTH 2
#else
#ifdef LISP_FEATURE_INT4_BREAKPOINTS
# define BREAKPOINT_INST 0xce    /* INTO */
#else
# define BREAKPOINT_INST 0xcc    /* INT3 */
#endif
#define BREAKPOINT_WIDTH 1
#endif
unsigned int cpuid_fn1_ecx;
int avx_supported = 0, avx2_supported = 0;

static void cpuid(unsigned info, unsigned subinfo,
                  unsigned *eax, unsigned *ebx, unsigned *ecx, unsigned *edx)
{
#ifdef _MSC_VER
  int regs[4];
  __cpuid(regs, info);
  *eax = regs[0];
  *ebx = regs[1];
  *ecx = regs[2];
  *edx = regs[3];
#else
  __asm__("cpuid;"                                            /* assembly code */
          :"=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx) /* outputs */
          :"a" (info), "c" (subinfo)                          /* input: info into eax,
                                                                 subinfo to ecx */
           /* clobbers: none */
          );
#endif
}

static void xgetbv(unsigned *eax, unsigned *edx)
{
    __asm__("xgetbv;"
            :"=a" (*eax), "=d" (*edx)
            : "c" (0));
}

#define VECTOR_FILL_T "VECTOR-FILL/T"

// Poke in a byte that changes an opcode to enable faster vector fill.
// Using fixed offsets and bytes is no worse than what we do elsewhere.
void tune_asm_routines_for_microarch(void)
{
    unsigned int eax, ebx, ecx, edx;

    cpuid(0, 0, &eax, &ebx, &ecx, &edx);
    if (eax >= 1) { // see if we can execute basic id function 1
        unsigned avx_mask = 0x18000000; // OXSAVE and AVX
        cpuid(1, 0, &eax, &ebx, &ecx, &edx);
        cpuid_fn1_ecx = ecx;
        if ((ecx & avx_mask) == avx_mask) {
            xgetbv(&eax, &edx);
            if ((eax & 0x06) == 0x06) { // YMM and XMM
                avx_supported = 1;
                cpuid(7, 0, &eax, &ebx, &ecx, &edx);
                if  (ebx & 0x20)  {
                    avx2_supported = 1;
                }
            }
        }
    }
    SetSymbolValue(CPUID_FN1_ECX, (lispobj)make_fixnum(cpuid_fn1_ecx), 0);
    // I don't know if this works on Windows
#ifndef _MSC_VER
    cpuid(0, 0, &eax, &ebx, &ecx, &edx);
    if (eax >= 7) {
        cpuid(7, 0, &eax, &ebx, &ecx, &edx);
        if (ebx & (1<<9)) // Enhanced Repeat Movs/Stos
          asm_routine_poke(VECTOR_FILL_T, 0x12, 0x7C); // Change JMP to JL
    }
#endif
}

/* Undo code patches so that the core file applies to the most generic
   microarchitecture on startup. As it happens, FILL-VECTOR/T is fine
   either way, but in general this might not be true for code using
   instructions that don't exist on some cpu family members */
void untune_asm_routines_for_microarch(void)
{
    asm_routine_poke(VECTOR_FILL_T, 0x12, 0xEB); // Change JL to JMP
}

#ifndef _WIN64
os_vm_address_t
arch_get_bad_addr(int __attribute__((unused)) sig,
                  siginfo_t *code,
                  os_context_t __attribute__((unused)) *context)
{
    return (os_vm_address_t)code->si_addr;
}
#endif


/*
 * hacking signal contexts
 *
 * (This depends both on architecture, which determines what we might
 * want to get to, and on OS, which determines how we get to it.)
 */

os_context_register_t *
context_eflags_addr(os_context_t *context)
{
#if defined __linux__
    /* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
     * <sys/ucontext.h> file to define symbolic names for offsets into
     * gregs[], but it's conditional on __USE_GNU and not defined, so
     * we need to do this nasty absolute index magic number thing
     * instead. */
    return (os_context_register_t*)&context->uc_mcontext.gregs[17];
#elif defined LISP_FEATURE_SUNOS
    return &context->uc_mcontext.gregs[REG_RFL];
#elif defined LISP_FEATURE_FREEBSD || defined(__DragonFly__)
    return &context->uc_mcontext.mc_rflags;
#elif defined LISP_FEATURE_DARWIN
    return CONTEXT_ADDR_FROM_STEM(rflags);
#elif defined __OpenBSD__
    return &context->sc_rflags;
#elif defined __NetBSD__
    return CONTEXT_ADDR_FROM_STEM(RFLAGS);
#elif defined _WIN64
    return (os_context_register_t*)&context->win32_context->EFlags;
#else
#error unsupported OS
#endif
}

void arch_skip_instruction(os_context_t *context)
{
    /* Assuming we get here via an INT3 xxx instruction, the PC now
     * points to the interrupt code (a Lisp value) so we just move
     * past it. Skip the code; after that, if the code is an
     * error-trap or cerror-trap then skip the data bytes that follow. */

    long code;

    /* Get and skip the Lisp interrupt code. */
    code = *(char*)(*os_context_pc_addr(context))++;
    switch (code)
        {
        case trap_Error:
        case trap_Cerror:
            skip_internal_error(context);

            break;

        case trap_Breakpoint:           /* not tested */
        case trap_FunEndBreakpoint: /* not tested */
            break;

#ifdef LISP_FEATURE_SB_SAFEPOINT
        case trap_GlobalSafepoint:
        case trap_CspSafepoint:
#endif
        case trap_PendingInterrupt:
        case trap_Halt:
        case trap_SingleStepAround:
        case trap_SingleStepBefore:
        case trap_InvalidArgCount:
            /* only needed to skip the Code */
            break;

        default:
            fprintf(stderr,"[arch_skip_inst invalid code %ld\n]\n",code);
            break;
        }

    FSHOW((stderr,
           "/[arch_skip_inst resuming at %x]\n",
           *os_context_pc_addr(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    return 1 + (unsigned char *)(*os_context_pc_addr(context));
}

boolean
arch_pseudo_atomic_atomic(os_context_t __attribute__((unused)) *context)
{
    return get_pseudo_atomic_atomic(arch_os_get_current_thread());
}

void
arch_set_pseudo_atomic_interrupted(os_context_t __attribute__((unused)) *context)
{
    struct thread *thread = arch_os_get_current_thread();
    set_pseudo_atomic_interrupted(thread);
}

void
arch_clear_pseudo_atomic_interrupted(os_context_t __attribute__((unused)) *context)
{
    struct thread *thread = arch_os_get_current_thread();
    clear_pseudo_atomic_interrupted(thread);
}

/*
 * This stuff seems to get called for TRACE and debug activity.
 */

unsigned int
arch_install_breakpoint(void *pc)
{
    unsigned int result = *(unsigned int*)pc;

#ifndef LISP_FEATURE_UD2_BREAKPOINTS
    *(char*)pc = BREAKPOINT_INST;               /* x86 INT3       */
    *((char*)pc+1) = trap_Breakpoint;           /* Lisp trap code */
#else
    *(char*)pc = UD2_INST & 0xff;
    *((char*)pc+1) = UD2_INST >> 8;
    *((char*)pc+2) = trap_Breakpoint;
#endif

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *((char *)pc) = orig_inst & 0xff;
    *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;
#if BREAKPOINT_WIDTH > 1
    *((char *)pc + 2) = (orig_inst & 0xff0000) >> 16;
#endif
}

/* When single stepping, single_stepping holds the original instruction
 * PC location. */
unsigned int *single_stepping = NULL;
#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
unsigned int  single_step_save1;
unsigned int  single_step_save2;
unsigned int  single_step_save3;
#endif

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int*)(*os_context_pc_addr(context));

    /* Put the original instruction back. */
    arch_remove_breakpoint(pc, orig_inst);

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    /* Install helper instructions for the single step:
     * pushf; or [esp],0x100; popf. */
    single_step_save1 = *(pc-3);
    single_step_save2 = *(pc-2);
    single_step_save3 = *(pc-1);
    *(pc-3) = 0x9c909090;
    *(pc-2) = 0x00240c81;
    *(pc-1) = 0x9d000001;
#else
    *context_eflags_addr(context) |= 0x100;
#endif

    single_stepping = pc;

#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    *os_context_pc_addr(context) = (os_context_register_t)((char *)pc - 9);
#endif
}

void
arch_handle_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) -= BREAKPOINT_WIDTH;
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) -= BREAKPOINT_WIDTH;
    *os_context_pc_addr(context) =
        (uword_t)handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    arch_skip_instruction(context);
    /* On x86-64 the fdefn / function is always in RAX, so we pass
     * 0 as the register_offset. */
    handle_single_step_trap(context, trap, 0);
}


void
restore_breakpoint_from_single_step(os_context_t * context)
{
#ifdef CANNOT_GET_TO_SINGLE_STEP_FLAG
    /* Un-install single step helper instructions. */
    *(single_stepping-3) = single_step_save1;
    *(single_stepping-2) = single_step_save2;
    *(single_stepping-1) = single_step_save3;
#else
    *context_eflags_addr(context) &= ~0x100;
#endif
    /* Re-install the breakpoint if possible. */
    if (((char *)*os_context_pc_addr(context) >
         (char *)single_stepping) &&
        ((char *)*os_context_pc_addr(context) <=
         (char *)single_stepping + BREAKPOINT_WIDTH)) {
        fprintf(stderr, "warning: couldn't reinstall breakpoint\n");
    } else {
        arch_install_breakpoint(single_stepping);
    }

    single_stepping = NULL;
    return;
}

void
sigtrap_handler(int __attribute__((unused)) signal,
                siginfo_t __attribute__((unused)) *info,
                os_context_t *context)
{
    unsigned int trap;

    if (single_stepping) {
        restore_breakpoint_from_single_step(context);
        return;
    }

    /* This is just for info in case the monitor wants to print an
     * approximation. */
    access_control_stack_pointer(arch_os_get_current_thread()) =
        (lispobj *)*os_context_sp_addr(context);

    /* On entry %rip points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
     * number of bytes will follow, the first is the length of the byte
     * arguments to follow. */
    trap = *(unsigned char *)(*os_context_pc_addr(context));
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (trap == trap_UndefinedFunction) {
        // The interrupted PC pins this fdefn. Sigtrap is delivered on the ordinary stack,
        // not the alternate stack.
        lispobj* fdefn = search_immobile_space((void*)*os_context_pc_addr(context));
        if (fdefn && widetag_of(fdefn) == FDEFN_WIDETAG) {
            // Return to undefined-tramp
            *os_context_pc_addr(context) = (uword_t)((struct fdefn*)fdefn)->raw_addr;
            // with RAX containing the FDEFN
            *os_context_register_addr(context,reg_RAX) =
                make_lispobj(fdefn, OTHER_POINTER_LOWTAG);
            return;
        }
    }
#endif
    handle_trap(context, trap);
}

void
sigill_handler(int __attribute__((unused)) signal,
               siginfo_t __attribute__((unused)) *siginfo,
               os_context_t *context) {
    /* Triggering SIGTRAP using int3 is unreliable on OS X/x86, so
     * we need to use illegal instructions for traps.
     */
#if defined(LISP_FEATURE_UD2_BREAKPOINTS) && !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    if (*((unsigned short *)*os_context_pc_addr(context)) == UD2_INST) {
        *os_context_pc_addr(context) += 2;
        return sigtrap_handler(signal, siginfo, context);
    }
#elif defined(LISP_FEATURE_INT4_BREAKPOINTS) && !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    if (*((unsigned char *)*os_context_pc_addr(context)) == BREAKPOINT_INST) {
        *os_context_pc_addr(context) += BREAKPOINT_WIDTH;
        return sigtrap_handler(signal, siginfo, context);
    }
#endif

    fake_foreign_function_call(context);
    lose("Unhandled SIGILL at %p.", (void*)*os_context_pc_addr(context));
}

#ifdef X86_64_SIGFPE_FIXUP
#define MXCSR_IE (0x01)         /* Invalid Operation */
#define MXCSR_DE (0x02)         /* Denormal */
#define MXCSR_ZE (0x04)         /* Devide-by-Zero */
#define MXCSR_OE (0x08)         /* Overflow */
#define MXCSR_UE (0x10)         /* Underflow */
#define MXCSR_PE (0x20)         /* Precision */

static inline int
mxcsr_to_code(unsigned int mxcsr)
{
    /* Extract unmasked exception bits. */
    mxcsr &= ~(mxcsr >> 7) & 0x3F;

    /* This order is defined at "Intel 64 and IA-32 Architectures
     * Software Developerfs Manual" Volume 1: "Basic Architecture",
     * 4.9.2 "Floating-Point Exception Priority". */
    if (mxcsr & MXCSR_IE)
        return FPE_FLTINV;
    else if (mxcsr & MXCSR_ZE)
        return FPE_FLTDIV;
    else if (mxcsr & MXCSR_DE)
        return FPE_FLTUND;
    else if (mxcsr & MXCSR_OE)
        return FPE_FLTOVF;
    else if (mxcsr & MXCSR_UE)
        return FPE_FLTUND;
    else if (mxcsr & MXCSR_PE)
        return FPE_FLTRES;

    return 0;
}

static void
sigfpe_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int *mxcsr = arch_os_context_mxcsr_addr(context);

#ifndef LISP_FEATURE_DARWIN
    /* Darwin doesn't handle accrued bits right. */
    if (siginfo->si_code == 0)
#endif
    { /* XMM exception */
        siginfo->si_code = mxcsr_to_code(*mxcsr);

        /* Clear sticky exception flag. */
        *mxcsr &= ~0x3F;
    }

    interrupt_handle_now(signal, siginfo, context);
}
#endif

void
arch_install_interrupt_handlers()
{
    SHOW("entering arch_install_interrupt_handlers()");

    /* Note: The old CMU CL code here used sigtrap_handler() to handle
     * SIGILL as well as SIGTRAP. I couldn't see any reason to do
     * things that way. So, I changed to separate handlers when
     * debugging a problem on OpenBSD, where SBCL wasn't catching
     * SIGILL properly, but was instead letting the process be
     * terminated with an "Illegal instruction" output. If this change
     * turns out to break something (maybe breakpoint handling on some
     * OS I haven't tested on?) and we have to go back to the old CMU
     * CL way, I hope there will at least be a comment to explain
     * why.. -- WHN 2001-06-07 */
#if !defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER) && !defined(LISP_FEATURE_WIN32)
    undoably_install_low_level_interrupt_handler(SIGILL , sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
#endif

#if defined(X86_64_SIGFPE_FIXUP) && !defined(LISP_FEATURE_WIN32)
    undoably_install_low_level_interrupt_handler(SIGFPE, sigfpe_handler);
#endif

    SHOW("returning from arch_install_interrupt_handlers()");
}

#ifdef LISP_FEATURE_LINKAGE_TABLE
void
arch_write_linkage_table_entry(char *reloc_addr, void *target_addr, int datap)
{
    if (datap) {
        *(uword_t *)reloc_addr = (uword_t)target_addr;
        return;
    }
    reloc_addr[0] = 0xFF; /* Opcode for near jump to absolute reg/mem64. */
    reloc_addr[1] = 0x25; /* ModRM #b00 100 101, i.e. RIP-relative. */
    UNALIGNED_STORE32((reloc_addr+2), 2); /* 32-bit displacement field = 2 */
    reloc_addr[6] = 0x66; reloc_addr[7] = 0x90; /* 2-byte NOP */
    *(void**)(reloc_addr+8) = target_addr;
}
#endif

/* These setup and check *both* the sse2 and x87 FPUs. While lisp code
   only uses the sse2 FPU, other code (such as libc) may use the x87 FPU.
 */

unsigned int
arch_get_fp_modes()
{
    unsigned int temp;
    unsigned int result;
    /* return the x87 exception flags ored in with the sse2
     * control+status flags */
    asm ("fnstsw %0" : "=m" (temp));
    result = temp;
    result &= 0x3F;
    asm ("stmxcsr %0" : "=m" (temp));
    result |= temp;
    /* flip exception mask bits */
    return result ^ (0x3F << 7);
}

struct fpenv
{
    unsigned short cw;
    unsigned short unused1;
    unsigned short sw;
    unsigned short unused2;
    unsigned int other_regs[5];
};

void
arch_set_fp_modes(unsigned int mxcsr)
{
    struct fpenv f_env;
    unsigned int temp;

    /* turn trap enable bits into exception mask */
    mxcsr ^= 0x3F << 7;

    /* set x87 modes */
    asm ("fnstenv %0" : "=m" (f_env));
    /* set control word: always long double precision
     * get traps and rounding from mxcsr word */
    f_env.cw = 0x300 | ((mxcsr >> 7) & 0x3F) | (((mxcsr >> 13) & 0x3) << 10);
    /* set status word: only override exception flags, from mxcsr */
    f_env.sw &= ~0x3F;
    f_env.sw |= (mxcsr & 0x3F);

    asm ("fldenv %0" : : "m" (f_env));

    /* now, simply, load up the mxcsr register */
    temp = mxcsr;
    asm ("ldmxcsr %0" : : "m" (temp));
}

/// Return the Lisp object that fdefn's raw_addr slot jumps to.
/// This will either be:
/// (1) a simple-fun,
/// (2) a funcallable-instance with an embedded trampoline that makes
///     it resemble a simple-fun in terms of call convention, or
/// (3) a code-component with no simple-fun within it, that makes
///     closures and other funcallable-instances look like simple-funs.
/// If the fdefn jumps to the UNDEFINED-FDEFN routine, then return 0.
lispobj fdefn_callee_lispobj(struct fdefn* fdefn) {
    lispobj* raw_addr = (lispobj*)fdefn->raw_addr;
    if (!raw_addr || points_to_asm_code_p((lispobj)raw_addr))
        // technically this should return the address of the code object
        // containing asm routines, but it's fine to return 0.
        return 0;
    // If the object to which raw_addr points was already forwarded,
    // this returns the "old" pointer, prior to forwarding, so that
    // scavenging that pointer alters it. Otherwise scav_fdefn would not
    // decide to rewrite the raw_addr slot of the fdefn.
    // This logic is rather nasty, but I don't know what else to do.
    lispobj word;
    if (header_widetag(word = raw_addr[-2]) == SIMPLE_FUN_WIDETAG
        || (word == 1 &&
            widetag_of(native_pointer(forwarding_pointer_value(raw_addr-2)))
            == SIMPLE_FUN_WIDETAG))
        return make_lispobj(raw_addr - 2, FUN_POINTER_LOWTAG);
    int widetag;
    if ((widetag = header_widetag(word = raw_addr[-4])) == CODE_HEADER_WIDETAG
        || (word == 1 &&
            widetag_of(native_pointer(forwarding_pointer_value(raw_addr-4)))
            == CODE_HEADER_WIDETAG))
        return make_lispobj(raw_addr - 4, OTHER_POINTER_LOWTAG);
    if (widetag == FUNCALLABLE_INSTANCE_WIDETAG
        || (word == 1 &&
            widetag_of(native_pointer(forwarding_pointer_value(raw_addr-4)))
            == FUNCALLABLE_INSTANCE_WIDETAG))
        return make_lispobj(raw_addr - 4, FUN_POINTER_LOWTAG);
    lose("Unknown object in fdefn raw addr: %p", raw_addr);
}

#include "genesis/vector.h"
#define LOCK_PREFIX 0xF0
#undef SHOW_PC_RECORDING

extern unsigned int alloc_profile_n_counters;
extern unsigned int max_alloc_point_counters;
#ifdef LISP_FEATURE_SB_THREAD
extern pthread_mutex_t alloc_profiler_lock;
#endif

static unsigned int claim_index(int qty)
{
    static boolean warning_issued;
    unsigned int index = alloc_profile_n_counters;
    alloc_profile_n_counters += qty;
    if (alloc_profile_n_counters <= max_alloc_point_counters)
       return index;
    if (!warning_issued) {
       fprintf(stderr, "allocation profile buffer overflowed\n");
       warning_issued = 1;
    }
    return 0; // use the overflow bin(s)
}

static boolean instrumentp(uword_t* sp, uword_t** pc, uword_t* old_word)
{
    int __attribute__((unused)) ret = thread_mutex_lock(&alloc_profiler_lock);
    gc_assert(ret == 0);
    uword_t next_pc = *sp;
    // The instrumentation site was 8-byte aligned
    uword_t return_pc = ALIGN_DOWN(next_pc, 8);
#if 0
    if (!(return_pc >= instrument_from && return_pc < instrument_to))
        return 0;
#endif
    uword_t word = *(uword_t*)return_pc;
    unsigned char opcode = word & 0xFF;
    // Adjust the return PC to where the call instruction was,
    // not the instruction after it.
    *sp = return_pc;
    // If > 1 thread called this routine at the same time,
    // one of them would already have patched the call site.
    if (opcode == LOCK_PREFIX)
        return 0;
    *pc = (uword_t*)return_pc;
    *old_word = word;
    return 1;
}

// logical index 'index' in the metadata array stores the code component
// and pc-offset relative to the component base address
static void record_pc(char* pc, unsigned int index, boolean sizedp)
{
    lispobj *code = component_ptr_from_pc(pc);
    if (!code) {
        fprintf(stderr, "can't identify code @ %p\n", pc);
    } else {
#ifdef SHOW_PC_RECORDING
        fprintf(stderr, "%#lx (%#lx) -> %d%s\n",
                (uword_t)pc, make_lispobj(code, OTHER_POINTER_LOWTAG),
                index, sizedp?" (sized)":"");
#endif
    }
#if 0
    if (!code) {
        int ret = thread_mutex_lock(&free_pages_lock);
        gc_assert(ret == 0);
        ensure_region_closed(code_region);
        int ret = thread_mutex_unlock(&free_pages_lock);
        gc_assert(ret == 0);
        code = component_ptr_from_pc(pc);
    }
#endif
    struct vector* v = VECTOR(alloc_profile_data);
    index <<= 1;
    if (sizedp) {
       v->data[index] = v->data[index+1] = NIL;
       index += 2;
    }
    if (code) {
        v->data[index] = make_lispobj(code, OTHER_POINTER_LOWTAG);
        v->data[index+1] = make_fixnum((lispobj)pc - (lispobj)code);
    } else {
        gc_assert(!((uword_t)pc & LOWTAG_MASK));
        v->data[index] = make_fixnum(-1); // no code component found
        v->data[index+1] = (lispobj)pc;
    }
}

void AMD64_SYSV_ABI
allocation_tracker_counted(uword_t* sp)
{
    uword_t *pc, word_at_pc;
    if (instrumentp(sp, &pc, &word_at_pc)) {
        unsigned int index = claim_index(1);
        if (index == 0)
            index = 2; // reserved overflow counter for fixed-size alloc
        // rewrite call into: LOCK INC QWORD PTR, [R11+n] ; opcode = 0xFF / 0
        uword_t new_inst =
          0xF0 | (0x49 << 8) | (0xFF << 16) | (0x83L << 24) | ((index*8L) << 32);
        // Ensure atomicity of the write. A plain store would probably do,
        // but since this is self-modifying code, the most stringent memory
        // order is prudent.
        if (!__sync_bool_compare_and_swap(pc, word_at_pc, new_inst))
            lose("alloc profiler failed to rewrite instruction @ %p", pc);
        if (index != 2)
            record_pc((char*)pc, index, 0);
    }
    int __attribute__((unused)) ret = thread_mutex_unlock(&alloc_profiler_lock);
    gc_assert(ret == 0);
}

void AMD64_SYSV_ABI
allocation_tracker_sized(uword_t* sp)
{
    uword_t *pc, word_at_pc;
    if (instrumentp(sp, &pc, &word_at_pc)) {
        int index = claim_index(2);
        uword_t word_after_pc = pc[1];
        unsigned char prefix = word_after_pc & 0xFF;
        unsigned char opcode = (word_after_pc >>  8) & 0xFF;
        unsigned char modrm  = (word_after_pc >> 16) & 0xFF;
        if ((prefix == 0x48 || prefix == 0x4D) && /* REX w=1 or w=r=b=1 */
            (opcode == 0x85) && /* TEST */
            (modrm & 0xC0) == 0xC0 && /* register-direct mode */
            ((modrm >> 3) & 0x7) == (modrm & 0x7)) { /* same register */
        } else {
            lose("Can't decode instruction @ pc %p\n", pc);
        }
        // rewrite call into:
        //  LOCK INC QWORD PTR, [R11+n] ; opcode = 0xFF / 0
        uword_t new_inst1 =
          0xF0 | (0x49 << 8) | (0xFF << 16) | (0x83L << 24) | ((index * 8L) << 32);
        //  LOCK ADD [R11+n], Rxx ; opcode = 0x01
        prefix = 0x49 | ((prefix & 1) << 2); // 'b' bit becomes 'r' bit
        modrm  = 0x83 | (modrm & (7<<3)); // copy 'reg' into new modrm byte
        uword_t new_inst2 =
          0xF0 | (prefix << 8) | (0x01 << 16) | ((long)modrm << 24)
            | (((1 + index) * 8L) << 32);
        // Overwrite the second instruction first, because as soon as the CALL
        // opcode is changed, fallthrough to the next instruction occurs.
        if (!__sync_bool_compare_and_swap(pc+1, word_after_pc, new_inst2) ||
            !__sync_bool_compare_and_swap(pc,   word_at_pc,    new_inst1))
            lose("alloc profiler failed to rewrite instructions @ %p", pc);
        if (index != 0) // can't record a PC for the overflow counts
            record_pc((char*)pc, index, 1);
    }
    int __attribute__((unused)) ret = thread_mutex_unlock(&alloc_profiler_lock);
    gc_assert(ret == 0);
}
