#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long int pyscm_ptr;

extern pyscm_ptr pyscm_start();

static const int num_mask = 0x03;
static const int num_tag  = 0x00;
static const int num_shift = 2;

void pyscm_display(pyscm_ptr expr) {
  if ((expr & num_mask) == num_tag) {
    int res = ((int) expr) >> num_shift;
    printf("%d", res);
  }
}

int main(int argc, char **argv) {
  pyscm_display(pyscm_start());
  printf("\n");
  return 0;
}

