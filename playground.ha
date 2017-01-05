typedef int p;

struct person {
  string name;
  int age;
};

int plus1(int x) {
  return x + 1;
}

person person() {
  person p;
  return p;
}

array[int] newArray() {
  return alloc(10);
}

int main(int argc, array[string] argv) {
    person p = person();
    array[int] a;

    if (true) {
      return 0;
    } else {
      int b = 3;
    }

    int i;
    int j;
    for (i = 0; true; i += 1) {
      j += plus1(i);
    }

    return 0;
}