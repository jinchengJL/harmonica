struct mutex_t {
  mutex m;
  <int> lock;
  <int> unlock;
  <int> destroy;
};

mutex_t create_mutex_t() {
  mutex_t mut;
  mut.m = mutex_create();
  mut.lock = lambda () int (mutex_lock(mut.m));
  mut.unlock = lambda () int (mutex_unlock(mut.m));
  mut.destroy = lambda () int (mutex_destroy(mut.m));
  return mut;
}

mutex_t m;
list[int] arr;
int i = 0;

int f(int v) {
  m.lock();
  int j;
  int s = 0;
  arr[i] = i;
  i += 1;
  m.unlock();
  
  return 0;
}

int main() {
  m = create_mutex_t();
  arr = malloc(sizeof(0) * 4);
  parallel(f, [0,0,0,0], 4);
  m.destroy;
  
  for (i = 0; i < 4; i += 1) {
    printi(arr[i]);
  }
  
  free(arr);
  
  return 0;
}
