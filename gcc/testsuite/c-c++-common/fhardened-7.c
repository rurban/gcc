/* { dg-do compile { target pie } } */
/* { dg-options "-fhardened -fpie" } */

/* -fpie takes precedence over -fhardened */
#if __PIE__ != 1
# error "__PIE__ != 1"
#endif
