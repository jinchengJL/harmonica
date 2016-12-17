int i = 0;

struct p {
   int a;
   int b;
   <bool int> c;
};

void f1(int x){
     printi(x);
}

void f2(p x) {
  printi(x.a);
}

int main() {
    int a = 3;
    int b = 4;
    int c = 5;
    int d = 6;

    parallel(f1, [a, b, c, d], 4);

    p e;
    p f;
    p g;
    p h;

    e.a = 31;
    f.a = 41;
    g.a = 51;
    h.a = 61;

    parallel(f2, [e,f,g,h], 4);
}

