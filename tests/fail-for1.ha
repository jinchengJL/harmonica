int main()
{
  int i;
  for (;;) {} # OK: Forever

  for (i = 0 ; i < 10 ; i = i + 1) {
    if (i == 3) return 42;
  }

  for (j = 0; i < 10 ; i = i + 1) {} # j undefined

  return 0;
}
