#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

static inline void 
get_spinlock(lispobj *word,int value)
{
    *word=value;		/* FIXME for threads */
}

static inline void
release_spinlock(lispobj *word)
{
    *word=0;
}


#define ARCH_HAS_LINK_REGISTER

#endif /* _PPC_ARCH_H */
