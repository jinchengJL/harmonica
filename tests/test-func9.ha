bool foo(<int bool> f, int c) {
     return f(c);
}

bool bar(int c) {
     if (c == 3) {
        printi(c + 1);
        return false;
     }
     return true;
}

int main() {
    <int bool> g = bar;
    printb(foo(g, 3));
    return 0;
}
