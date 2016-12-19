int main() {
  vector_int vec = vector_int_create();

  int i;
  for (i = 0; i < 10; i += 1) {
    vector_int_append(vec, i * i);
  }

  vector_int_set(vec, 5, 768);
  vector_int_set(vec, 9, 153);

  for (i = 0; i < 10; i += 1)
    printi(vector_int_get(vec, i));

  vector_int_destroy(vec);

  return 0;
}
