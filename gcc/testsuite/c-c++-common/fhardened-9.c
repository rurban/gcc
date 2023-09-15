/* { dg-do compile } */
/* { dg-options "-fhardened -U_FORTIFY_SOURCE -U_GLIBCXX_ASSERTIONS" } */

#if defined(_FORTIFY_SOURCE) || defined(_GLIBCXX_ASSERTIONS)
# error "hardening enabled when it should not be"
#endif
