#include <iostream>
#include <vector>
#include <stdlib.h>

using namespace std;
int main(){
    int power, i0, j0,value, last_4dgt = 0, n;
    vector<int> a;
    vector<int> element;
    cin>>power;
    cin>>i0;
    cin>>j0;
    int s0  = 10000;
    while(cin>>value){
    element.push_back(value%s0);
    }
    n = (int)element.size();
    vector<int> matpower(n);
    
    for(int i=1;i<=n;i++){
        a.push_back(element[abs(i0-i)]);
    }
    if(power == 1){
        cout << a[j0-1] <<endl;
    }
    else{
        for(int k=1; k < power; k++ ){
            for(int l=0; l<n; l++){
                for(int m = 0; m<n; m++){
                    last_4dgt = last_4dgt + (a[m]*element[abs(m-l)])%s0;
                }
                matpower[l] = last_4dgt;
                last_4dgt = 0;
            }
            a = matpower;
        }
        cout << matpower[j0-1]%s0<<endl;
    
    }
    return 0;
}
