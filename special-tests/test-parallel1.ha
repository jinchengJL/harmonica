struct p {
  int a;
  int b;
};

void f1(int x) {
  printi(x);
}

void f2(p x) {
  x.a += 1;
}

int main() {
  int a = 3;
  int b = 4;
  int c = 5;
  int d = 6;

  parallel(f1, [a, b, c, d], 4);

  p e;
  p f;

  e.a = 31;
  f.a = 41;

  parallel(f2, [e,f], 2);

  printi(e.a);
  printi(f.a);
}

