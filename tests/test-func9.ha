bool foo(<int bool> f, int c) {
     return f(c);
}

bool bar(int c) {
     return true;
}

int main() {
    <int bool> g = bar;
    printb(foo(g, 3));
    return 0;
}
