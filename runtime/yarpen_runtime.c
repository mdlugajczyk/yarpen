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
static const int nil_tag = 0x47;
static const int nil_mask = 0xCF;

void pyscm_display_expr(pyscm_ptr expr, int enclose_in_parens);

int is_pair(pyscm_ptr expr) {
  return (expr & object_mask) == cons_tag;
}

int is_nil(pyscm_ptr expr) {
  return (expr & nil_mask) == nil_tag;
}

void pyscm_display_pair(pyscm_ptr expr, int enclose_in_parens) {
  pyscm_ptr car = *((pyscm_ptr *)(expr-cons_tag));
  pyscm_ptr cdr = *((pyscm_ptr *)(expr-cons_tag) + 1);
  const int  valid_pair = is_pair(cdr) || is_nil(cdr);

  if (enclose_in_parens)
    printf("(");
  pyscm_display_expr(car, 1);
  if (is_nil(cdr)) {
    if (enclose_in_parens)
      printf(")");
    return;
  }
  if (valid_pair)
    printf(" ");
  else
    printf(" . ");

  pyscm_display_expr(cdr, 0);
  if (enclose_in_parens)
    printf(")");
}

void pyscm_display_expr(pyscm_ptr expr, int enclose_with_parens) {
  if ((expr & num_mask) == num_tag) {
    long int res = ((long int) expr) >> num_shift;
    printf("%ld", res);
  } else if (expr == bool_t) {
    printf("#t");
  } else if (expr == bool_f) {
    printf("#f");
  } else if ((expr & object_mask) == closure_tag) {
    printf("closure");
  } else if (is_pair(expr)) {
    pyscm_display_pair(expr, enclose_with_parens);
  } else if (is_nil(expr)) {
    printf("()");
  } else {
    printf("#<unknown 0x%08llx>", expr);
  }
}

void pyscm_display(pyscm_ptr expr) {
  pyscm_display_expr(expr, 1);
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

