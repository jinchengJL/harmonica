int main() { 
  float f;
  string s;
  list[int] l;
  list[list[list[string]]] sss;
    
  if (2>1){
    print(1);
  } else if (1==2){
    print(2);
  }
  print(1);

  struct person {
    int age;
    string name;
  };

  typedef int ID;
  ID a;

  channel[int] c;
  c = chan(list[int]);
  parallel(f, l, 0, 2, c);

  c.data;
  c.func(c).pop();

  f = 1.23;

  tuple[int float] tup;
  typedef tuple[int float] TIF;
  <TIF int> func;

  l = [1,2,3,4,5];
  tup = (1, 2.67);
}
