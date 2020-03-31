/*
 * stuff to handle internal errors
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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "sbcl.h"
#include "arch.h"
#include "signal.h"

#include "runtime.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"
#include "genesis/static-symbols.h"
#include "genesis/vector.h"
#include "code.h"
#include "thread.h"
#include "monitor.h"
#include "breakpoint.h"
#include "var-io.h"
#include "sc-offset.h"
#include "gc.h"

/* the way that we shut down the system on a fatal error */
void lisp_backtrace(int frames);

static void
default_lossage_handler(void)
{
    static int backtrace_invoked = 0;
    if (!backtrace_invoked) {
        backtrace_invoked = 1;
        // This may not be exactly the right condition for determining
        // whether it might be possible to backtrace, but at least it prevents
        // lose() from itself losing early in startup.
        if (arch_os_get_current_thread()) lisp_backtrace(100);
    }
    exit(1);
}
static void (*lossage_handler)(void) = default_lossage_handler;

#if QSHOW
static void
configurable_lossage_handler()
{

    if (dyndebug_config.dyndebug_backtrace_when_lost) {
        fprintf(stderr, "lose: backtrace follows as requested\n");
        lisp_backtrace(100);
    }

    if (dyndebug_config.dyndebug_sleep_when_lost) {
        fprintf(stderr,
"The system is too badly corrupted or confused to continue at the Lisp.\n"
"level.  The monitor was enabled, but you requested `sleep_when_lost'\n"
"behaviour though dyndebug.  To help with your debugging effort, this\n"
"thread will not enter the monitor, and instead proceed immediately to an\n"
"infinite sleep call, maximizing your chances that the thread's current\n"
"state can be preserved until you attach an external debugger. Good luck!\n");
        for (;;)
#         ifdef LISP_FEATURE_WIN32
            Sleep(10000);
#         else
            sleep(10);
#         endif
    }

    monitor_or_something();
}
#endif

void enable_lossage_handler(void)
{
#if QSHOW
    lossage_handler = configurable_lossage_handler;
#else
    lossage_handler = monitor_or_something;
#endif
}
void disable_lossage_handler(void)
{
    lossage_handler = default_lossage_handler;
}

static
void print_message(char *fmt, va_list ap)
{
    fprintf(stderr, " in SBCL pid %d",getpid());
#if defined(LISP_FEATURE_SB_THREAD)
    fprintf(stderr, "(tid %p)", (void*)thread_self());
#endif
    if (fmt) {
        fprintf(stderr, ":\n");
        vfprintf(stderr, fmt, ap);
    }
    fprintf(stderr, "\n");
}

static inline void
call_lossage_handler() never_returns;

static inline void
call_lossage_handler()
{
    lossage_handler();
    fprintf(stderr, "Argh! lossage_handler() returned, total confusion..\n");
    exit(1);
}

void
lose(char *fmt, ...)
{
    va_list ap;
    /* Block signals to prevent other threads, timers and such from
     * interfering. If only all threads could be stopped somehow. */
    block_blockable_signals(0);
    fprintf(stderr, "fatal error encountered");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    call_lossage_handler();
}

boolean lose_on_corruption_p = 0;

void
corruption_warning_and_maybe_lose(char *fmt, ...)
{
    va_list ap;
#ifndef LISP_FEATURE_WIN32
    sigset_t oldset;
    block_blockable_signals(&oldset);
#endif
    fprintf(stderr, "CORRUPTION WARNING");
    va_start(ap, fmt);
    print_message(fmt, ap);
    va_end(ap);
    fprintf(stderr, "The integrity of this image is possibly compromised.\n");
    if (lose_on_corruption_p || gc_active_p) {
        fprintf(stderr, "Exiting.\n");
        fflush(stderr);
        call_lossage_handler();
    }
    else {
        fprintf(stderr, "Continuing with fingers crossed.\n");
        fflush(stderr);
#ifndef LISP_FEATURE_WIN32
        thread_sigmask(SIG_SETMASK,&oldset,0);
#endif
    }
}

void print_constant(os_context_t *context, int offset) {
    lispobj code = find_code(context);
    if (code != NIL) {
        struct code *codeptr = (struct code *)native_pointer(code);
        putchar('\t');
        if (offset >= code_header_words(codeptr)) {
            printf("Constant offset %d out of bounds for the code object @ %p\n",
                   offset, codeptr);
        } else {
            brief_print(codeptr->constants[offset -
                                           (offsetof(struct code, constants) >> WORD_SHIFT)]);
        }
    }
}

#include "genesis/errnames.h"
char *internal_error_descriptions[] = {INTERNAL_ERROR_NAMES};
char internal_error_nargs[] = INTERNAL_ERROR_NARGS;

void skip_internal_error (os_context_t *context) {
    unsigned char *ptr = (unsigned char *)*os_context_pc_addr(context);
#ifdef LISP_FEATURE_ARM64
    // This code is broken. The program counter as received in *context
    // points one instruction beyond the BRK opcode.
    // I wrote a test vop that emits a cerror break thusly:
    //
    // ; 62C:       40A122D4         BRK #5386                       ; Cerror trap
    // ; 630:       30               BYTE #X30                       ; R2
    //
    // and added a printf to see the instruction pointer here:
    //   skip_internal_error: pc=0x1002441630
    //
    // which got a trap code of 0. This is due to the fact that DESCRIPTOR-REG
    // is the lowest SC number, so the encoded SC+OFFSET of R2 is a small
    // value, and therefore shifting right by 13 bits extracts a 0.
    // Internal error number 0 has 0 arguments, so we skip nothing in the varint
    // decoder loop, which is the right thing for the wrong reason.
    u32 trap_instruction = *(u32 *)ptr;
    unsigned char code = trap_instruction >> 13 & 0xFF;
    ptr += 4;
#else
    unsigned char code = *ptr;
    ptr++; // skip the byte indicating the kind of trap
#endif
    if (code > sizeof(internal_error_nargs)) {
        printf("Unknown error code %d at %p\n", code, (void*)*os_context_pc_addr(context));
    }
    int nargs = internal_error_nargs[code];
    int nbytes = 0;
    while (nargs--) read_var_integer(ptr, &nbytes);
    ptr += nbytes;
    *((unsigned char **)os_context_pc_addr(context)) = ptr;
}

/* internal error handler for when the Lisp error system doesn't exist
 *
 * FIXME: Shouldn't error output go to stderr instead of stdout? (Alas,
 * this'd require changes in a number of things like brief_print(..),
 * or I'd have changed it immediately.) */
void
describe_internal_error(os_context_t *context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    char count;
    int position, sc_and_offset, sc_number, offset, ch;
    void * pc = (void*)*os_context_pc_addr(context);
    unsigned char code;

#ifdef LISP_FEATURE_ARM64
    u32 trap_instruction = *(u32 *)ptr;
    code = trap_instruction >> 13 & 0xFF;
    ptr += 4;
#else
    unsigned char trap = *(ptr-1);
    if (trap >= trap_Error) {
        code = trap - trap_Error;
    } else {
        code = *ptr;
        ptr++;
    }
#endif

    if (code > sizeof(internal_error_nargs)) {
        printf("Unknown error code %d at %p\n", code, pc);
    }
    printf("Internal error #%d \"%s\" at %p\n", code, internal_error_descriptions[code], pc);

    for (count = internal_error_nargs[code], position = 0;
         count > 0;
         --count) {
        sc_and_offset = read_var_integer(ptr, &position);
        sc_number = sc_and_offset_sc_number(sc_and_offset);
        offset = sc_and_offset_offset(sc_and_offset);

        printf("    SC: %d, Offset: %d", sc_number, offset);
        switch (sc_number) {
        case sc_AnyReg:
        case sc_DescriptorReg:
            putchar('\t');
            brief_print(*os_context_register_addr(context, offset));
            break;

        case sc_CharacterReg:
            ch = *os_context_register_addr(context, offset);
#ifdef LISP_FEATURE_X86
            if (offset&1)
                ch = ch>>8;
            ch = ch & 0xff;
#endif
            switch (ch) {
            case '\n': printf("\t'\\n'\n"); break;
            case '\b': printf("\t'\\b'\n"); break;
            case '\t': printf("\t'\\t'\n"); break;
            case '\r': printf("\t'\\r'\n"); break;
            default:
                if (ch < 32 || ch > 127)
                    printf("\\%03o", ch);
                else
                    printf("\t'%c'\n", ch);
                break;
            }
            break;
        case sc_SapReg:
#ifdef sc_WordPointerReg
        case sc_WordPointerReg:
#endif
            printf("\t0x%08lx\n", (unsigned long) *os_context_register_addr(context, offset));
            break;
        case sc_SignedReg:
            printf("\t%ld\n", (long) *os_context_register_addr(context, offset));
            break;
        case sc_UnsignedReg:
            printf("\t%lu\n", (unsigned long) *os_context_register_addr(context, offset));
            break;
#ifdef sc_SingleFloatReg
        case sc_SingleFloatReg:
            printf("\t%g\n", *(float *)&context->sc_fpregs[offset]);
            break;
#endif
#ifdef sc_DoubleFloatReg
        case sc_DoubleFloatReg:
            printf("\t%g\n", *(double *)&context->sc_fpregs[offset]);
            break;
#endif
        case sc_Constant:
            print_constant(context, offset);
            break;
        default:
            printf("\t???\n");
            break;
        }
    }
}

/* utility routines used by miscellaneous pieces of code */

lispobj debug_print(lispobj string)
{
    /* This is a kludge.  It's not actually safe - in general - to use
       %primitive print on the alpha, because it skips half of the
       number stack setup that should usually be done on a function
       call, so the called routine (i.e. this one) ends up being able
       to overwrite local variables in the caller.  Rather than fix
       this everywhere that %primitive print is used (it's only a
       debugging aid anyway) we just guarantee our safety by putting
       an unused buffer on the stack before doing anything else
       here */
    char untouched[32];
    if (header_widetag(VECTOR(string)->header) != SIMPLE_BASE_STRING_WIDETAG)
        fprintf(stderr, "debug_print: can't display string\n");
    else
        fprintf(stderr, "%s\n", (char *)(VECTOR(string)->data));
    /* shut GCC up about not using this, because that's the point.. */
    (void)untouched;
    return NIL;
}
