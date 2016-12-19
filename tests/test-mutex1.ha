mutex m;
list[int] arr;
int i = 0;

int f(int v) {
    mutex_lock(m);
    int j;
    int s = 0;
    arr[i] = i;
    i += 1;
    mutex_unlock(m);

    return 0;
}

int main() {
   m = mutex_create();
   arr = malloc(16);
   parallel(f, [0,0,0,0], 4);
   mutex_destroy(m);

   for (i = 0; i < 4; i += 1) {
       printi(arr[i]);
   }

   free(arr);

   return 0;
}
