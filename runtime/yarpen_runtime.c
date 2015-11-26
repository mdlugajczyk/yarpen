#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long int pyscm_ptr;

extern pyscm_ptr pyscm_start();

static const int num_mask = 0x03;
static const int num_tag  = 0x00;
static const int num_shift = 2;
static const int bool_f = 0x2F;
static const int bool_t = 0x6F;
static const int object_mask = 0x07;
static const int closure_tag = 0x03;
static const int cons_tag = 0x01;

void pyscm_display(pyscm_ptr expr) {
  if ((expr & num_mask) == num_tag) {
    long int res = ((long int) expr) >> num_shift;
    printf("%ld", res);
  } else if (expr == bool_t) {
    printf("#t");
  } else if (expr == bool_f) {
    printf("#f");
  } else if ((expr & object_mask) == closure_tag) {
    printf("closure");
  } else if ((expr & object_mask) == cons_tag) {
    pyscm_ptr car = *((pyscm_ptr *)(expr-cons_tag));
    pyscm_ptr cdr = *((pyscm_ptr *)(expr-cons_tag) + 1);
    printf("(");
    pyscm_display(car);
    printf(" ");
    pyscm_display(cdr);
    printf(")");
  } else {
    printf("#<unknown 0x%08llx>", expr);
  }
}

void* pyscm_alloc(int size) {
  void *mem = malloc(size);
  if (!mem) {
    fprintf(stderr, "Failed to allocate %d bytes of memory. Aborting.\n", size);
    exit(1);
  }

  return mem;
}

int main(int argc, char **argv) {
  pyscm_display(pyscm_start());

  return 0;
}

