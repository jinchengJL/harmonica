#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFSIZE 4096

void* str_concat(void *str1, void *str2)
{
    strcat((char *)str1, (char *)str2);
    return str1;
}

void int_to_string(int n, void *buf)
{
    sprintf(buf, "%d", n);
}
