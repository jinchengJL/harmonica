bool foo(<int bool> f, int c) {
     return f(c);
}

bool bar(int c) {
     return true;
}

int main() {
    foo(bar, 3);
    foo(bar, (false,2));
}
