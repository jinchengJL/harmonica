int a = 3;

int main() {
    printii(a);
    {
        int b = 3;
        printii(b);
    }

    printii(b);
    return 0;
}
