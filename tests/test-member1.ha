struct person {
    int age;
    string name;
};

int main() {
    person p;
    p.age = 3;
    printii(p.age);

    person p2;
    p2.age = 4;
    printii(p2.age);
    printii(p.age);
}
