void map(<int int> f, list[int] arr, int length) {
  int i;
  for (i = 0; i < length; i += 1) {
    arr[i] = f(arr[i]);
  }
}

int reduce(<int int int> f, list[int] arr, int length) {
  if (length <= 0)
    return 0;
  int acc = arr[0];
  int i = 1;
  for (i = 1; i < length; i += 1) {
    acc = f(acc, arr[i]);
  }
  return acc;
}

int main() {
  <int int> plus1  = lambda (int x) int ( x + 1 );
  <int int> times2 = lambda (int x) int ( x * 2 );
  <int int int> plus = lambda (int x, int y) int ( x + y );

  list[int] arr = [1,2,3,4,5,6];
  map(plus1, arr, 6);
  map(times2, arr, 6);

  int i;
  for (i = 0; i < 6; i += 1) {
    printi(arr[i]);
  }

  print("sum: ");
  printi(reduce(plus, arr, 6));

  return 0;
}
