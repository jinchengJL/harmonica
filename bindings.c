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
    int err = 0;
    err = pthread_create(&thread[i], NULL, start_routine, *(void **) addr);
    if (err != 0) {
      exit(err);
    }
  }
  
  int err = 0;
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

void* mutex_create(void) {
	pthread_mutex_t* mtx = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
	if (mtx == NULL) {
		perror("mutex allocation failed: ");
	}
	int err = pthread_mutex_init(mtx, NULL);
	
	if (err != 0) {
		perror("mutext init failed");
		free(mtx);
		return NULL;
	}

	return mtx;
}

int lock(void* mtx) {
	int err = pthread_mutex_lock((pthread_mutex_t *) mtx);
    printf("locked\n");
	if (err != 0) {
		perror("mutex lock failed");
		return err;
	}
	return 0;
}

int unlock(void* mtx) {
    printf("unlocking\n");
	int err = pthread_mutex_unlock((pthread_mutex_t *) mtx);
	if (err != 0) {
		perror("mutex unlock failed");
		return err;
	}
	return 0;
}

int destroy(void* mtx) {
	int err = pthread_mutex_destroy((pthread_mutex_t *) mtx);
	if (err != 0) {
		perror("mutex unlock failed");
		return err;
	}
	return 0;
}
