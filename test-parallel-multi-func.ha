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
    int b = 2
    int c = 3
    int d = 4

    parallel([f1,f2,f3,f4],[a,b,c,d],4)
    parallel([f1,f2,f3,f4],[a,b,c,d])
    parallel(f1,[a,b,c,d])
    parallel(f1,[a,b,c,d],4)
    parallel([f1,f2,f3,f4],[a,b,c,d],2)
}

