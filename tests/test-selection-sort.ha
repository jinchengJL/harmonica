void selectionSort(list[int] arr, int length) {
    if (length < 0)
      return ;

    int i;
    int j;
    for (i = 0; i < length; i = i + 1) {
      int k = i;
      for (j = i+1; j < length; j = j + 1) {
        if (arr[j] < arr[k])
          k = j;
      }

      int temp = arr[k];
      arr[k] = arr[i];
      arr[i] = temp;
    }
}

int main() {
  list[int] arr = [10,-356,2147483647,4,3,2,1];
  int length = 7;
  selectionSort(arr, length);
  int i;
  for (i = 0; i < length; i = i + 1) {
    printi(arr[i]);
  }

  return 0;
}
