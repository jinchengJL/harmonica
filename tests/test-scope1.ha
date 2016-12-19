int main() {
    int b = 3;
    {
        int b = 4;
        printi(b);
    }
    printi(b);

    return 0;
}
