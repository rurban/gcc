/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <stdint.h>

#define N 256

int a[N] __attribute__ ((aligned (64)));
int b[N] __attribute__ ((aligned (64)));

void __attribute__ ((noipa))
foo (int *p, int *q)
{
   svint32_t va, vb, vc;
   int i;
   uint64_t sz = svcntw ();

#pragma omp simd aligned(p, q : 64) private (va, vb, vc) \
		 nontemporal (va, vb, vc)
  for (i = 0; i < N; i++)
    {
      if (i % sz == 0)
	{
	  va = svld1_s32 (svptrue_b32 (), p);
	  vb = svindex_s32 (1, 0);
	  vc = svadd_s32_z (svptrue_b32 (), va, vb);
	  svst1_s32 (svptrue_b32 (), q, vc);
	  q += sz;
	}
    }
}

int
main ()
{

  for (int i = 0; i < N; i++)
    {
      a[i] = 1;
      b[i] = 0;
    }

  foo (a, b);

  for (int i = 0; i < N; i++)
    if (b[i] != 2)
      __builtin_abort ();
}
