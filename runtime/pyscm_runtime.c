#include <stdio.h>
#include <stdlib.h>

typedef unsigned long int pyscm_ptr;

extern pyscm_ptr pyscm_start();

int main(int argc, char **argv) {
  pyscm_start();
  printf("\n");
  return 0;
}
