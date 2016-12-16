int f1(int a){
    return a+1
}

int f2(int b){
    return b+2
}

int f3(int c){
    return c+3
}

int f4(int d){
    return d+4
}

int main(){
    int a = 1
    parallel([f1,f2,f3,f4],[a,a,a,a],4)
    parallel([f1,f2,f3,f4],[a,a,a,a])
    parallel(f1,[a,a,a,a])
    parallel(f1,[a,a,a,a],4)
    parallel([f1,f2,f3,f4],[a,a,a,a],2)
}

