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

float momentf(list[float] data, int n, int moment){
    float sum;
    int i = 0;
    for (i=0; i<n; i += 1){
        sum += powi(data[i], moment);
    }
    return sum/n;
}

int main(){
    list[float] gpa = [3.2, 3.45, 2.8, 4.0];
    int n = 4;

    int i = 0;
    print("GPA in this class: ");
    printendl("");
    for (i=0; i<n; i += 1){
        printf(gpa[i]);
    }

    float mean = momentf(gpa, n, 1);
    
    print("Mean GPA: ");
    printf(mean);

    float variance = momentf(gpa, n, 2) - powi(mean, 2);
    print("Variance GPA: ");
    printf(variance);

    concat("hi","there");

    return 0;
}

