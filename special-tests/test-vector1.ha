int main() {
  vector_int vec = vector_int_create();

  int i;
  for (i = 0; i < 8; i += 1) {
    vector_int_append(vec, i * i);
  }

  vector_int_set(vec, 3, 1000);
  vector_int_set(vec, 6, 1000000);

  vector_int_remove(vec, 4);

  for (i = 0; i < vec.length; i += 1)
    printi(vector_int_get(vec, i));

  vector_int_destroy(vec);

  return 0;
}
