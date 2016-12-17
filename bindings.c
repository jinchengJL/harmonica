#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void* str_concat(void *str1, void *str2)
{
  strcat((char *)str1, (char *)str2);
  return str1;
}

void int_to_string(int n, void *buf)
{
  sprintf(buf, "%d", n);
}

int parallel(void *(*start_routine) (void *), void *arg, int asize, int nthreads)
{
  pthread_t thread[nthreads];
  int i;
  for (i = 0; i < nthreads; i ++) {
    void *addr = (void *) (((char *) arg) + i * asize);
    int err = pthread_create(&thread[i], NULL, start_routine, *(void **) addr);
    if (err != 0) {
      exit(err);
    }
  }
  
  int err;
  for (i = 0; i < nthreads; i++) {
    err = pthread_join(thread[i], NULL);
    if (err != 0) {
      break;
    }
  }
  
  if (err != 0)
    perror("pthread_join: ");
  
  return err;
}
