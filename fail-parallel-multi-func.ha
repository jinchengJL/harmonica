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

int f5(int a, int b){
    return a+b
}
int main(){
    int a = 1
    int b = 2
    int c = 3
    int d = 4
    parallel([f2,f3,f4],[a,b,c,d],4) # failed: number of functions doesn't equals to number of input values
    parallel([f1,f3,f4],[a,b,c,d]) # failed: number of functions doesn't equals to numb\
er of input values
    parallel(f5,[a,b,c,d]) # failed: the numbers of arguments doesn't match the nubmer of input
    parallel(f1,[a,b,c,d],0) # failed: smaller than 1
    parallel(f1,[a,b,c,d],-1) # failed: smaller than 1
    parallel([f1,f2,f3],[a,b,c,d],2) # failed: number of functions doesn't equals to numb\
er of input values
}

