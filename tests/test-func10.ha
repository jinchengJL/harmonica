list[int] f(int x) {
  list[int] ret = [x, x+1, x-1];
  return ret;
}

int main() {
  list[int] arr = f(3);
  printi(arr[0]);
  printi(arr[1]);
  printi(arr[2]);
  return 0;
}
