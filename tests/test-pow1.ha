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





int main(){
    printf(powi(2.5, 2)); # 6.250000
    printf(powi(0.5, -2)); # 4.000000

    return 0;
}
