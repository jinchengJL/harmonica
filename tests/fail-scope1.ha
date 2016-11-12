int a = 3;

list[int] f(int x) {
   return [x, x, x];
}

int main() {
    printi(a);
    {
        int b = 3;
        printi(b);
    }

    printi(b);
    return 0;
}