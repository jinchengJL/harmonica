int main()
{
  printib(true);
  printib(false);
  printib(true && true);
  printib(true && false);
  printib(false && true);
  printib(false && false);
  printib(true || true);
  printib(true || false);
  printib(false || true);
  printib(false || false);
  printib(!false);
  printib(!true);
  printi(-10);
  printi(--42);
}
