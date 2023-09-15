/* { dg-do compile { target pie } } */
/* { dg-options "-fhardened -fPIC" } */

/* -fPIC takes precedence over -fhardened */
#ifdef __PIE__
# error "PIE enabled when it should not be"
#endif
