int main() {
  list[int] l = [999,999,999,999];
  int a = 3;
  l[a] = 1;

  printi(l[0]);
  printi(l[3]);
  free(l);

  list[int] l2 = malloc(16);
  l2[0] = 14;
  l2[2] = 33;
  l2[3] = 75;
  l2[1] = 5;
  printi(l2[1]);

  return 0;
}
