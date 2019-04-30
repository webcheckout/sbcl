/*
 * This is the RISC-V Linux incarnation of arch-dependent OS-dependent
 * routines. See also "linux-os.c".
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
#include <sys/cachectl.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "validate.h"
size_t os_vm_page_size;

int
arch_os_thread_init(struct thread *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
#warning "Check threading support functions"
#endif
    return 1;                  /* success */
}

int
arch_os_thread_cleanup(struct thread *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
#warning "Check threading support functions"
#endif
    return 1;                  /* success */
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&(context->uc_mcontext.__gregs[offset]);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return os_context_register_addr(context, 0);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
#warning "Implement correct fp control."
}

os_context_register_t   *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&(context->uc_mcontext.__fpregs.__d.__f[offset]);
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __riscv_flush_icache(address, end_address, 0);
}
