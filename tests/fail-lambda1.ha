struct node {
  int x;
  string name;
};


int main() {
    node n;
    <node node> f = lambda (node n) node (n.x);
    f(n);

    return 0;
}