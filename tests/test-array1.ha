int main() {
  array[int] l = alloc(16);
  int i;
  for (i = 0; i < 4; i += 1)
    l[i] = 999;
  int a = 3;
  l[a] = 1;

  printi(l[0]);
  printi(l[3]);
  free(l);

  array[int] l2 = alloc(16);
  l2[0] = 14;
  l2[2] = 33;
  l2[3] = 75;
  l2[1] = 5;
  printi(l2[1]);

  return 0;
}
