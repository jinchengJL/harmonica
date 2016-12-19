<int bool> isThree = lambda (int x) bool (x == 3);
<int void> printInt = lambda (int x) void ( printi(x) );

int main() {
    printb(isThree(3));
    printb(isThree(0));
    printInt(10);
    return 0;
}