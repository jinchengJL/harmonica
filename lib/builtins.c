#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* memory */
/* TODO: garbage collection ? */
void * _ha_alloc(int n) {
  return malloc(n);
}

void _ha_free(void *ptr) {
  free(ptr);
}

/* printing */
void _ha_print(char *s) {
  printf("%s", s);
}

void _ha_printi(int i) {
  printf("%d\n", i);
}

void _ha_printf(double f) {
  printf("%f\n", f);
}

void _ha_println(char *s) {
  printf("%s\n", s);
}

/* strings */

/* mutex */

/* list */
