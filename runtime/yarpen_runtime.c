#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef unsigned long long int pyscm_ptr;

extern pyscm_ptr pyscm_start();

static uint64_t stack_bottom;
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
static const int char_mask = 0x3F;
static const int char_tag = 0x0F;
static const int char_shift = 8;

static void pyscm_display_expr(pyscm_ptr expr, int enclose_in_parens);

static int is_pair(pyscm_ptr expr) {
  return (expr & object_mask) == cons_tag;
}

static int is_nil(pyscm_ptr expr) {
  return (expr & nil_mask) == nil_tag;
}

static int is_closure(pyscm_ptr expr) {
  return (expr & object_mask) == closure_tag;
}

static pyscm_ptr get_car(pyscm_ptr cons) {
  return *((pyscm_ptr *)(cons-cons_tag));
}

static pyscm_ptr get_cdr(pyscm_ptr cons) {
  return *((pyscm_ptr *)(cons-cons_tag) + 1);
}

static void pyscm_display_pair(pyscm_ptr expr, int enclose_in_parens) {
  const pyscm_ptr car = get_car(expr);
  const pyscm_ptr cdr = get_cdr(expr);
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

static void pyscm_display_expr(pyscm_ptr expr, int enclose_with_parens) {
  if ((expr & num_mask) == num_tag) {
    const long int res = ((long int) expr) >> num_shift;
    printf("%ld", res);
  } else if (expr == bool_t) {
    printf("#t");
  } else if (expr == bool_f) {
    printf("#f");
  } else if ((expr & char_mask) == char_tag) {
    const char c = (char)(expr >> char_shift);
    if (c == '\n' || c == ' ')
      printf("%c", c);
    else
      printf("#\\%c", c);
  } else if (is_closure(expr)) {
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

typedef struct memory_header {
  char marked;
  struct memory_header *next;
} memory_header;

static memory_header *first_memory_header = NULL;
static memory_header *last_memory_header = NULL;

static memory_header *cast_to_memory_header(pyscm_ptr expr) {
  return (memory_header *)(expr - sizeof(memory_header));
}

static void scan_expression(const pyscm_ptr expr) {
  if (is_closure(expr)) {
    cast_to_memory_header(expr)->marked = 1;
  }
}

static void scan_stack() {
  uint64_t stack_top;
  asm volatile ("movq	%%rbp, %0" : "=r" (stack_top));

  uint64_t *sp = (uint64_t *)stack_top;
  uint64_t *sp_end = (uint64_t *)stack_bottom;

  for (; sp < sp_end; sp++) {
    const pyscm_ptr expr = (pyscm_ptr)sp;
    scan_expression(expr);
  }
}

static void mark() {
  scan_stack();
}

static void sweep() {
}

static void gc() {
  mark();
  sweep();
}

void* pyscm_alloc(int size) {
  gc();
  char *mem = malloc(size + sizeof(memory_header));
  if (!mem) {
    fprintf(stderr, "Failed to allocate %d bytes of memory. Aborting.\n", size);
    exit(1);
  }

  ((memory_header *)(mem))->marked = 0;
  ((memory_header *)(mem))->next = NULL;

  if (first_memory_header == NULL)
    first_memory_header = (memory_header *)mem;

  if (last_memory_header)
    last_memory_header->next = (memory_header *)mem;
  else
    last_memory_header = (memory_header *)mem;

  return mem + sizeof(memory_header);
}

int main(int argc, char **argv) {
  asm volatile ("movq	%%rbp, %0" : "=r" (stack_bottom));
  pyscm_display(pyscm_start());

  return 0;
}

