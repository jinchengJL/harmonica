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

<int> main = lambda () int {

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


# file: list.ha

type T;

struct list {
  array[T] elems;
  int len;
  int memsize;
  <T void> append;
  <int void> remove;
  <int> size;
};

void list_append(list[T] this, T elem) {
  
}

list[T] newlist() {
  list[T] lst = alloc(sizeof(list));
  list.elems = alloc(4 * sizeof(T));
  list.len = 0;
  list.memsize = 4;
  list.append = lambda (T elem) void {
    list_append(lst, elem);
  }
}


# file: main.ha
import list

int main() {
  list[int] lst;
}