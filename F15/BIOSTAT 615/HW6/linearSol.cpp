#include <iostream>
#include <vector>
using namespace std;
int main(){
    int element,n,i;
    vector<int> b;
    while(cin>>element) b.push_back(element);
    n=b.size();
    int x;
    if(n>=2) x=b[n-1]-b[n-2];
    else x=b[0];
    for(i=2;i<n;i++){
        if((2*b[n-i]-b[n-i+1]-b[n-i-1])>=x)
          x=2*b[n-i]-b[n-i+1]-b[n-i-1];
    }
    if(n>=2){
         if((2*b[0]-b[1])>=x) x=2*b[0]-b[1];
    }
    cout<<x<<endl;
}
