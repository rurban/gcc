/* DWARF2 EH unwinding support for AIX.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Useful register numbers.  */

#define R_LR             65
#define R_CR2            70
#define R_XER            76
#define R_FIRST_ALTIVEC  77
#define R_VRSAVE        109
#define R_VSCR          110

/* If the current unwind info (FS) does not contain explicit info
   saving R2, then we have to do a minor amount of code reading to
   figure out if it was saved.  The big problem here is that the
   code that does the save/restore is generated by the linker, so
   we have no good way to determine at compile time what to do.  */

#ifdef __64BIT__
#define MD_FROB_UPDATE_CONTEXT(CTX, FS)					\
  do {									\
    if ((FS)->regs.how[2] == REG_UNSAVED)				\
      {									\
	unsigned int *insn						\
	  = (unsigned int *)						\
	    _Unwind_GetGR ((CTX), R_LR);				\
	if (*insn == 0xE8410028)					\
	  _Unwind_SetGRPtr ((CTX), 2, (CTX)->cfa + 40);			\
      }									\
  } while (0)
#else
#define MD_FROB_UPDATE_CONTEXT(CTX, FS)					\
  do {									\
    if ((FS)->regs.how[2] == REG_UNSAVED)				\
      {									\
	unsigned int *insn						\
	  = (unsigned int *)						\
	    _Unwind_GetGR ((CTX), R_LR);				\
	if (*insn == 0x80410014)					\
	  _Unwind_SetGRPtr ((CTX), 2, (CTX)->cfa + 20);			\
      }									\
  } while (0)
#endif

/* Now on to MD_FALLBACK_FRAME_STATE_FOR.
   32bit AIX 5.2, 5.3, 6.1, 7.X and
   64bit AIX 6.1, 7.X only at this stage.  */

#include <stdlib.h>
#include <stddef.h>
#include <signal.h>
#include <sys/machine.h>

#ifdef __64BIT__

typedef struct __context64 mstate_t;

#else

typedef struct mstsave mstate_t;

#endif

#define MD_FALLBACK_FRAME_STATE_FOR ppc_aix_fallback_frame_state

/* If we are compiling on AIX < 5.3, the VMX related datastructs are not
   defined and we take measures to obtain proper runtime behavior if the
   compiled code happens to run on a later version with VMX enabled.  */

#ifndef MSR_VMX
#define MSR_VMX 0x2000000
#endif

typedef unsigned int uint;
typedef struct { uint v[4]; } vreg_t;
typedef struct {
  vreg_t regs[32];
  uint   pad1 [3];
  uint   vscr;
  uint   vrsave;
  uint   pad2 [3];
} vstate_t;

#define EXT_CONTEXT_MARK 0x45435458
#define EXT_CONTEXT_SIZE 4096
#define BUMPER_SIZE (EXT_CONTEXT_SIZE - sizeof(vstate_t) - (5 * sizeof(int)))

typedef struct {
  uint     pad1 [4];
  vstate_t vstate;
  char     bumper [BUMPER_SIZE];
  int      mark;
} extended_context_t;

typedef struct {
  char bumper [offsetof (ucontext_t, uc_stack) + sizeof (stack_t)];
  extended_context_t * ectx;
  int mark;
} vmx_ucontext_t;

/* Determine whether CONTEXT designates a signal handler, and return the
   associated ucontext_t address if so.  Return NULL otherwise.  */

static ucontext_t *
ucontext_for (struct _Unwind_Context *context)
{
  const unsigned int * ra = context->ra;

  /* AIX 5.2, 5.3, 6.1 and 7.X, threaded or not, share common patterns
     and feature variants depending on the configured kernel (unix_mp
     or unix_64).  */

#ifdef __64BIT__
  if (*(ra - 5) == 0x4c00012c     /* isync             */
      && *(ra - 4) == 0xe8ec0000  /* ld      r7,0(r12) */
      && *(ra - 3) == 0xe84c0008  /* ld      r2,8(r12) */
      && *(ra - 2) == 0x7ce903a6  /* mtctr   r7        */
      && *(ra - 1) == 0x4e800421  /* bctrl             */
      && *(ra - 0) == 0x7de27b78) /* mr      r2,r15   <-- context->ra */
    {
      /* unix_64 */
      if (*(ra - 6) == 0x7d000164)  /* mtmsrd  r8 */
	{
	  /* AIX 6.1, 7.1 and 7.2 */
	  return (ucontext_t *)(context->cfa + 0x70);
	}
    }
#else
  if (*(ra - 5) == 0x4c00012c     /* isync             */
      && *(ra - 4) == 0x80ec0000  /* lwz     r7,0(r12) */
      && *(ra - 3) == 0x804c0004  /* lwz     r2,4(r12) */
      && *(ra - 2) == 0x7ce903a6  /* mtctr   r7        */
      && *(ra - 1) == 0x4e800421  /* bctrl             */
      && *(ra - 0) == 0x7dc37378) /* mr      r3,r14   <-- context->ra */
    {
      /* unix_64 */
      if (*(ra - 6) == 0x7d000164)  /* mtmsrd  r8 */
	{
	  switch (*(ra + 18))
	    {
	      /* AIX 5.2 */
	    case 0x835a0520: /* lwz r26,1312(r26) */
	      return (ucontext_t *)(context->cfa + 0x70);

	      /* AIX 5.3 */
	    case 0x835a0570:  /* lwz r26,1392(r26) */
	      return (ucontext_t *)(context->cfa + 0x40);

	      /* AIX 6.1 and 7.1 */
	    case 0x2c1a0000:  /* cmpwi   r26,0 */
	      return (ucontext_t *)(context->cfa + 0x40);

	      /* AIX 7.2 */
	    case 0x3800000a:  /* li   r0,A */
	      return (ucontext_t *)(context->cfa + 0x40);

	    default:
	      return 0;
	    }
	}

      /* unix_mp */
      if (*(ra - 6) == 0x7d000124)  /* mtmsr  r8 */
	{
	  typedef struct {
	    char pad[56];
	    ucontext_t ucontext;
	    siginfo_t siginfo;
	  } aix52_stack_t;

	  aix52_stack_t * frame = (aix52_stack_t *) context->cfa;
	  return &frame->ucontext;
	}
    }
#endif
  return 0;
}

/* The fallback proper.  */

#ifdef __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__
#define RETURN_COLUMN __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__
#else
#define RETURN_COLUMN ARG_POINTER_REGNUM
#endif

#define REGISTER_CFA_OFFSET_FOR(FS,REGNO,ADDR,CFA)\
do { \
(FS)->regs.how[REGNO] = REG_SAVED_OFFSET; \
(FS)->regs.reg[REGNO].loc.offset = (long) (ADDR) - (CFA); \
} while (0)

static _Unwind_Reason_Code
ppc_aix_fallback_frame_state (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs)
{
  ucontext_t * uctx = ucontext_for (context);
  mstate_t * mctx;

  long new_cfa;
  int i;

  if (uctx == NULL)
    return _URC_END_OF_STACK;

  mctx = &uctx->uc_mcontext.jmp_context;

  /* The "kernel" frame cfa is the stack pointer at the signal occurrence
     point.  */
  new_cfa = mctx->gpr[__LIBGCC_STACK_POINTER_REGNUM__];

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* And we state how to find the various registers it has saved with
     relative offset rules from there.  */

  for (i = 0; i < 32; i++)
    if (i != __LIBGCC_STACK_POINTER_REGNUM__)
      REGISTER_CFA_OFFSET_FOR (fs, i, &mctx->gpr[i], new_cfa);

  REGISTER_CFA_OFFSET_FOR (fs, R_CR2, &mctx->cr, new_cfa);
  REGISTER_CFA_OFFSET_FOR (fs, R_XER, &mctx->xer, new_cfa);
  REGISTER_CFA_OFFSET_FOR (fs, R_LR, &mctx->lr, new_cfa);

  fs->retaddr_column = RETURN_COLUMN;
  REGISTER_CFA_OFFSET_FOR (fs, RETURN_COLUMN, &mctx->iar, new_cfa);
  fs->signal_frame = 1;

  /* Honor FP Ever Used ...   */
  if (mctx->fpeu)
    {
      for (i = 0; i < 32; i++)
	REGISTER_CFA_OFFSET_FOR (fs, i+32, &mctx->fpr[i], new_cfa);
    }

  /* Honor VMX context, if any.  We expect the msr bit never to be set in
     environments where there is no VMX support, e.g. on AIX < 5.3.  */
  if (mctx->msr & MSR_VMX)
    {
      vmx_ucontext_t * uc = (vmx_ucontext_t *) uctx;

      if (uc->mark == EXT_CONTEXT_MARK && uc->ectx->mark == EXT_CONTEXT_MARK)
	{
	  vstate_t * vstate = &uc->ectx->vstate;

	  for (i = 0; i < 32; i++)
	    REGISTER_CFA_OFFSET_FOR
	    (fs, i+R_FIRST_ALTIVEC, &vstate->regs[i], new_cfa);

	  REGISTER_CFA_OFFSET_FOR (fs, R_VSCR, &vstate->vscr, new_cfa);
	  REGISTER_CFA_OFFSET_FOR (fs, R_VRSAVE, &vstate->vrsave, new_cfa);
	}
    }

  return _URC_NO_REASON;
}
