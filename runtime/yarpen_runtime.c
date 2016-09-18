#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef unsigned long long int yarpen_ptr;

extern yarpen_ptr yarpen_start();

static uint64_t stack_bottom;
static const int num_mask = 0x03;
static const int num_tag  = 0x00;
static const int num_shift = 2;
static const int bool_f = 0x2F;
static const int bool_t = 0x6F;
static const int object_mask = 0x07;
static const int boxed_value_tag = 0x03;
static const int closure_tag = 0x02;
static const int cons_tag = 0x01;
static const int nil_tag = 0x47;
static const int nil_mask = 0xCF;
static const int char_mask = 0x3F;
static const int char_tag = 0x0F;
static const int char_shift = 8;

static void yarpen_display_expr(yarpen_ptr expr, int enclose_in_parens);

static int is_pair(yarpen_ptr expr) {
  return (expr & object_mask) == cons_tag;
}

static int is_nil(yarpen_ptr expr) {
  return (expr & nil_mask) == nil_tag;
}

static int is_closure(yarpen_ptr expr) {
  return (expr & object_mask) == closure_tag;
}

static int is_boxed_value(yarpen_ptr expr) {
  return (expr & object_mask) == boxed_value_tag;
}

static yarpen_ptr get_car(yarpen_ptr cons) {
  return *((yarpen_ptr *)(cons-cons_tag));
}

static yarpen_ptr get_cdr(yarpen_ptr cons) {
  return *((yarpen_ptr *)(cons-cons_tag) + 1);
}

static void yarpen_display_pair(yarpen_ptr expr, int enclose_in_parens) {
  const yarpen_ptr car = get_car(expr);
  const yarpen_ptr cdr = get_cdr(expr);
  const int  valid_pair = is_pair(cdr) || is_nil(cdr);

  if (enclose_in_parens)
    printf("(");
  yarpen_display_expr(car, 1);
  if (is_nil(cdr)) {
    if (enclose_in_parens)
      printf(")");
    return;
  }
  if (valid_pair)
    printf(" ");
  else
    printf(" . ");

  yarpen_display_expr(cdr, 0);
  if (enclose_in_parens)
    printf(")");
}

static void yarpen_display_expr(yarpen_ptr expr, int enclose_with_parens) {
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
    yarpen_display_pair(expr, enclose_with_parens);
  } else if (is_nil(expr)) {
    printf("()");
  } else {
    printf("#<unknown 0x%08llx>", expr);
  }
}

void yarpen_display(yarpen_ptr expr) {
  yarpen_display_expr(expr, 1);
}

typedef struct memory_header {
  char marked;
  struct memory_header *next;
} memory_header;

static memory_header *first_memory_header = NULL;
static memory_header *last_memory_header = NULL;

static memory_header *cast_to_memory_header(yarpen_ptr expr) {
  return (memory_header *)(expr - sizeof(memory_header));
}

static int is_valid_memory(yarpen_ptr expr) {
  memory_header *mem = NULL;
  memory_header *expr_header = cast_to_memory_header(expr);

  for (mem = first_memory_header; mem != NULL;  mem = mem->next) {
    if (expr_header == mem)
      return 1;
  }
  return 0;
}

static void scan_closure(const yarpen_ptr expr); 

static void scan_expression(const yarpen_ptr expr) {
  if (is_closure(expr)) {
    const yarpen_ptr untagged_closure = expr - closure_tag;
    if (!is_valid_memory(untagged_closure)) {
      return;
    }
    cast_to_memory_header(untagged_closure)->marked = 1;
    scan_closure(untagged_closure);
  } else if (is_pair(expr)) {
    const yarpen_ptr untagged_pair = expr - cons_tag;
    if (!is_valid_memory(untagged_pair)) {
      return;
    }
    memory_header *mem = cast_to_memory_header(untagged_pair);
    if (mem->marked)
      return;
    mem->marked = 1;

    scan_expression(get_car(expr));
    scan_expression(get_cdr(expr));
  } else if (is_boxed_value(expr)) {
    const yarpen_ptr untagged_value = expr - boxed_value_tag;
    if (!is_valid_memory(untagged_value)) {
      return;
    }

    memory_header *mem = cast_to_memory_header(untagged_value);
    mem->marked = 1;
    scan_expression(untagged_value);
  }
}

static void scan_closure(const yarpen_ptr expr) {
  uint64_t num_free_vars = *((uint64_t *)expr);
  uint64_t *first_free_var = (uint64_t *)(expr + 2 * sizeof(uint64_t));

  int i;
  for (i = 0; i < num_free_vars; i++)
    scan_expression(*first_free_var++);
}

static void scan_stack() {
  uint64_t stack_top;
  asm volatile ("movq	%%rbp, %0" : "=r" (stack_top));

  uint64_t *sp = (uint64_t *)stack_top;
  uint64_t *sp_end = (uint64_t *)stack_bottom;

  for (; sp < sp_end; sp++) {
    const yarpen_ptr expr = (yarpen_ptr)*sp;
    scan_expression(expr);
  }
}

static void mark() {
  scan_stack();
  uint64_t rbx;
  asm volatile ("movq	%%rbx, %0" : "=r" (rbx));
  scan_expression(rbx);
}

static void sweep() {
  memory_header *mem = NULL;
  memory_header *prev = NULL;

  for (mem = first_memory_header; mem != NULL;  mem = mem->next) {
    if (mem->marked) {
      mem->marked = 0;
      prev = mem;
      continue;
    } else {
      const yarpen_ptr expr = (yarpen_ptr)(mem + sizeof(memory_header));
      if (prev == NULL)
    	first_memory_header = mem->next;
      else
    	prev->next = mem->next;

      if (mem->next == NULL)
	last_memory_header = prev;
      free(mem);
    }
  }
}

static void gc() {
  mark();
  sweep();
}

void* yarpen_alloc(int size) {
  gc();
  char *mem = malloc(size + sizeof(memory_header));
  if (!mem) {
    exit(1);
  }

  ((memory_header *)(mem))->marked = 0;
  ((memory_header *)(mem))->next = NULL;

  if (first_memory_header == NULL)
    first_memory_header = (memory_header *)mem;

  if (last_memory_header) {
    last_memory_header->next = (memory_header *)mem;
    last_memory_header = (memory_header *)mem;
  } else {
    last_memory_header = (memory_header *)mem;
  }

  char *ret =  mem + sizeof(memory_header);
  return ret;
}

int main(int argc, char **argv) {
  asm volatile ("movq	%%rbp, %0" : "=r" (stack_bottom));
  yarpen_display(yarpen_start());

  // Normally we wouldn't bother calling the GC at the end, the OS will free
  // the memory anyway. But since we're running the integration tests under valgrind
  // we should dealocate all memory before exiting.
  // Therefore any unfreed memory reported by valgrind indicates a bug in the GC.
  gc();

  return 0;
}

