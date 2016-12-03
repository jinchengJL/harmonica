int global;
int global_init = 132;

typedef int ID;
typedef tuple[int float] TIF;

struct person {
  int age;
  string name;
};

int main() { 
  float f;
  string s;
  list[int] l;
  list[list[list[string]]] sss;
    
  if (2>1) {
    print(1);
  } else if (1==2){
    print(2);
  }
  print(1);

  ID a;

  channel[int] c;
  c = chan(list[int]);
  parallel(f, l, 0, 2, c);

  c.data;
  pig some_struct = c.func(c);
  some_struct.pop();

  f = 1.23;

  tuple[int float] tup;
  <TIF int> func;

  l = [1,2,3,4,5];
  tup = (1, 2.67);
  
  string ha = "harmonica\\escape\"";
  "harmonica\\escape\"";
  int a = c;

  <int <int bool> <int int>> f;
}
