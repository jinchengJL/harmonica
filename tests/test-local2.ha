int foo(int a, bool b)
{
  int c;
  bool d;

  c = a;

  return c + 10;
}

int main() {
 int i = foo(37, false);
 printi(i);
 return 0;
}
