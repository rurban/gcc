/* { dg-do compile } */
/* { dg-options "-fhardened -fstack-protector" } */

#ifdef __SSP_STRONG__
# error "-fstack-protector-strong enabled when it should not be"
#endif
#ifndef __SSP__
# error "-fstack-protector not enabled"
#endif
