bool foo(<int bool> f, int c) {
     return f(c);
}

bool bar(int c) {
     return true;
}

int main() {
    printib(foo(bar, 3));
    return 0;
}
