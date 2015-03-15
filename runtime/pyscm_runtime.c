#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long int pyscm_ptr;

extern pyscm_ptr pyscm_start();

static const int num_mask = 0x03;
static const int num_tag  = 0x00;
static const int num_shift = 2;
static const int bool_f = 0x2F;
static const int bool_t = 0x6F;

void pyscm_display(pyscm_ptr expr) {
  if ((expr & num_mask) == num_tag) {
    long int res = ((long int) expr) >> num_shift;
    printf("%ld", res);
  } else if (expr == bool_t) {
    printf("#t");
  } else if (expr == bool_f) {
    printf("#f");
  }
}

int main(int argc, char **argv) {
  pyscm_display(pyscm_start());

  return 0;
}

