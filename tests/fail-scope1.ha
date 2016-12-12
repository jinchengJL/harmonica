int a = 3;

int main() {
    printi(a);
    {
        int b = 3;
        printi(b);
    }

    printi(b);
    return 0;
}