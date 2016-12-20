alias T int
alias INIT_SIZE 16

struct vector_T {
  list[T] elements;
  int length;
  int memsize;
};

T __dummy_T;

vector_T vector_T_create() {
  vector_T v;
  v.elements = malloc(sizeof(__dummy_T) * INIT_SIZE);
  v.length = 0;
  v.memsize = INIT_SIZE;
  return v;
}

void vector_T_append(vector_T v, T elem) {
  if (v.length >= v.memsize) {
    v.memsize = v.memsize * 2;
    list[T] dest = malloc(sizeof(__dummy_T) * v.memsize);
    int i;
    for (i = 0; i < v.length; i += 1) {
      dest[i] = v.elements[i];
    }
    v.elements = dest;
  }

  v.elements[v.length] = elem;
  v.length += 1;
}

T vector_T_get(vector_T v, int i) {
  if (i >= v.length)
    print("ERROR: vector index out of bounds");
  return v.elements[i];
}

void vector_T_set(vector_T v, int i, T elem) {
  if (i >= v.length)
    print("ERROR: vector index out of bounds");
  v.elements[i] = elem;
}

void vector_T_remove(vector_T v, int i) {
  if (i >= v.length)
    print("ERROR: vector index out of bounds");

  int j;
  for (j = i; j < v.length - 1; j += 1) {
    v.elements[j] = v.elements[j+1];
  }
  v.length -= 1;
}

void vector_T_destroy(vector_T v) {
  free(v.elements);
  free(v);
}
