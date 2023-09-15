/* { dg-do compile { target pie } } */
/* { dg-options "-fhardened" } */

#if __PIE__ != 2
# error "-fPIE not enabled"
#endif
