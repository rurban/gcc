/* { dg-do compile } */
/* { dg-options "-fhardened -ftrivial-auto-var-init=uninitialized -fdump-tree-gimple" } */

int
foo ()
{
  int i;
  return i;
}

/* { dg-final { scan-tree-dump-not ".DEFERRED_INIT" "gimple" } } */
