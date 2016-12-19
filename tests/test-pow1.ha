float powi(float x, int n){
    if (n==0){
        return 1.0;
    }

    if (n>0){
        int i = 0;
        float y = 1.0;
        for (i=0; i<n; i=i+1){
            y = y*x;
        }
        return y;
    }
    
    int n_ = 0 - n;
    return (1.0/powi(x, n_));
}

float factorialf(float x){
    if (x==0.0){
        return 1.0;
    }
    return x*factorialf(x-1);
}

int factorial(int x){
    if (x==0){
        return 1;
    }
    if (x<0){
        return 0;
    }
    return x*factorial(x-1);
}

float exp(float x){
    float taylor = 0.0;
    int i = 0;
    float fi = 0.0;
    float up;
    float down;
    for (i=0; i<99; i=i+1){
        up = powi(x, i);
        down = factorialf(i/1.0);
        taylor = taylor + (up/down);
        fi = fi + 1.0;
    }
    return taylor;
}

float ln(float x){
    float taylor = 0.0;
    int i;
    float tmp;
    for (i=0; i<99; i=i+1){
        int i_p = 2*i + 1;
        tmp = 2*( powi((x-1)/(x+1), i_p) )/i_p;
        taylor = taylor + tmp;
    }
    return taylor;
}

float pow(float x, float y){
    return exp(y*ln(x));
}


int main(){
    printf(powi(2.5, 2)); # 6.250000
    printf(powi(0.5, -2)); # 4.000000

    printf(factorialf(4.0)); # 24.000000
    printf(exp(1.0)); # 2.718282
    printf(exp(2.0)); # 7.389056
    printf(exp(0.5)); # 1.648721

    printf(ln(3.0)); # 1.098612
    printf(ln(2.5)); # 0.916291

    printf(pow(2.0, 0.5)); # 1.414214
    printf(pow(7.2, -0.14));# 0.758531


    return 0;
}
