/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ptrue_pn0:
**	ptrue	pn([8-9]|1[0-5])\.h
**	mov	p0\.b, p\1\.b
**	ret
*/
TEST_PN (ptrue_pn0,
	 pn0 = svptrue_c16 (),
	 pn0 = svptrue_c16 ())

/*
** ptrue_pn7:
**	ptrue	pn([8-9]|1[0-5])\.h
**	mov	p7\.b, p\1\.b
**	ret
*/
TEST_PN (ptrue_pn7,
	 pn7 = svptrue_c16 (),
	 pn7 = svptrue_c16 ())

/*
** ptrue_pn8:
**	ptrue	pn8\.h
**	ret
*/
TEST_PN (ptrue_pn8,
	 pn8 = svptrue_c16 (),
	 pn8 = svptrue_c16 ())

/*
** ptrue_pn15:
**	ptrue	pn15\.h
**	ret
*/
TEST_PN (ptrue_pn15,
	 pn15 = svptrue_c16 (),
	 pn15 = svptrue_c16 ())
