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
    } else {
        int n_ = 0 - n;
        return (1.0/powi(x, n_));
    }
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
        down = factorialf(fi);
        taylor = taylor + (up/down);
        fi = fi + 1.0;
    }
    return taylor;
}




int main(){
    printf(powi(2.5, 2)); # 6.250000
    printf(powi(0.5, -2)); # 4.000000

    printf(factorialf(4.0)); # 24.000000
    printf(exp(1.0)); # 2.718282
    printf(exp(2.0)); # 7.389056
    printf(exp(0.5)); # 1.648721

    return 0;
}
