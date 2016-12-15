struct point{
    float x;
    float y;
};



float abs(float x){
  if (x>0.0) {
    return x;
  } else {
    if (x==0.0){
      return 0.0;
    }
    float y = 0.0 - x;
    return y;
  }

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

struct line{
    point s;
    point e;



};

float l2norm(line l){
    float xdiff = l.s.x - l.e.x;
    float ydiff = l.s.y - l.e.y;
    float square = xdiff*xdiff+ydiff*ydiff;
    return sqrt(square);
}



int main(){
    point a;
    point b;
    a.x = 0.0;
    a.y = 1.0;
    b.x = 2.0;
    b.y = 3.0;

    line l;
    l.s = a;
    l.e = b;

    printf(l2norm(l)); # 2.828427

    return 0;
}
