struct person {
    int age;
    string name;
};

int main() {
    person p;
    p.age = 3;
    printi(p.age);

    person p2;
    p2.age = 4;
    printi(p2.age);
    printi(p.age);
}
