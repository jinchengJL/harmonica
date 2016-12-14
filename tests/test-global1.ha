int a;
int b;

void printia()
{
  printi(a);
}

void printib()
{
  printi(b);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  a = 42;
  b = 21;
  printia();
  printib();
  incab();
  printia();
  printib();
  return 0;
}
