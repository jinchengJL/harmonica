int main() {
    int b = 3;
    {
        int b = 4;
        printii(b);
    }
    printii(b);
}
