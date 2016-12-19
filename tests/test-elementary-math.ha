int mod(int a, int b) {
  # mod function for natural numbers
  int x;
  int y;
  if (a>b){
    x = a;
    y = b;
  }else{
    return a;
  }

  while (x >= y){
    x = x - y;
  }
  return x;
}

int isPrime(int a){
  # see if an integer is prime number 
  if (a<2){
    return 0;
  }else {
    int i = 0;
    for (i=2; i<a; i=i+1){
      if (mod(a, i)==0){
        return 0;
      }
    }
    return 1;
  }
  return 0;
}

float abs(float x){
  if (x>0.0) {
    return x;
  }
  
  if (x==0.0){
    return 0.0;
  }
  
  float y = 0.0 - x;
  return y;
}

float sqrt(float a){
  # a naive function to return the sqrt approximation. return -1 if a is negative
  float err_tol = 0.000001;
  if (a < -0.0){
    return (-1.0);
  }

  float min = 0.0;
  float max = a;
  float middle = (min+max)/2.0;

  float x = middle*middle;
  
  while ( abs(x-a)>err_tol ){
    if (x < a){
      min = middle;
      max = max;
    }else{
      min = min;
      max = middle;
    }
    middle =  (min+max)/2.0;
    x = middle*middle;

  }

  return middle;

}


int main() {
  # test mod
  printi(mod(12, 5)); # 2
  printi(mod(7,21)); # 7
  printi(mod(35, 6)); # 5

  # test isPrime
  printi(isPrime(1)); # 0
  printi(isPrime(4)); # 0
  printi(isPrime(7)); # 1

  # test sqrt
  printf(sqrt(2.0)); # 1.414214
  printf(sqrt(-2.0)); # -1.000000
  printf(sqrt(4.0)); # 2.000000


  return 0;
}
