//  main.cpp
//  partialHMM

#include <iostream>
#include <iomanip>
#include <Eigen/Dense>
#include "Matrix615.h"
#include <string>

using namespace std;
using namespace Eigen;
/*
class HMM615 {
public:
    // parameters
    int nStates; // n : number of possible states
    int nObs; // m : number of possible output values
    int nTimes; // t : number of time slots with observations
    std::vector<double> pis; // initial states
    std::vector<int> outs; // observed outcomes
    Matrix615<double> trans; // trans[i][j] corresponds to A_{ij}
    Matrix615<double> emis;
    // storages for dynamic programming
    Matrix615<double> alphas, betas, gammas, deltas;
    Matrix615<int> phis;
    std::vector<int> path;
    
    HMM615(int states, int obs, int times) : nStates(states), nObs(obs), nTimes(times), trans(states, states, 0), emis(states, obs, 0), alphas(times, states, 0), betas(times, states, 0), gammas(times, states, 0), deltas(times, states, 0),phis(times, states, 0) {
        pis.resize(nStates);
        path.resize(nTimes);
    }
    void forward(); // given below
    void backward(); //
    void forwardBackward(); // given below
};

void HMM615::forward() {
    for(int i=0; i < nStates; ++i) {
        alphas.data[0][i] = pis[i] * emis.data[i][outs[0]];
    }
    for(int t=1; t < nTimes; ++t) {
        for(int i=0; i < nStates; ++i) {
            alphas.data[t][i] = 0;
            for(int j=0; j < nStates; ++j) {
                alphas.data[t][i] += (alphas.data[t-1][j] * trans.data[j][i] * emis.data[i][outs[t]]);
            }
        }
    }
}

void HMM615::backward() {
    for(int i=0; i < nStates; ++i) {
        betas.data[nTimes-1][i] = 1;
    }
    for(int t=nTimes-2; t >=0; --t) {
        for(int i=0; i < nStates; ++i) {
            betas.data[t][i] = 0;
            for(int j=0; j < nStates; ++j){
                betas.data[t][i] += (betas.data[t+1][j] * trans.data[i][j] * emis.data[j][outs[t+1]]);
            }
        }
    }
}

void HMM615::forwardBackward() {
    forward(); backward();
    for(int t=0; t < nTimes; ++t) {
        double sum = 0;
        for(int i=0; i < nStates; ++i) {
            sum += (alphas.data[t][i] * betas.data[t][i]);
        }
        for(int i=0; i < nStates; ++i) {
            gammas.data[t][i] = (alphas.data[t][i] * betas.data[t][i])/sum; }
    }
}
*/
int main(int argc, const char * argv[]) {
    double p = atof(argv[1]);
    double a1 = atof(argv[2]), a2 = atof(argv[3]), b1 = atof(argv[4]), b2 = atof(argv[5]);
    string observed = argv[6];
    int n = observed.length();

    Vector2d P(p, 1-p), result;
    Matrix2d A, B;
    A << a1, 1-a1, a2, 1-a2;
    B << b1, 1-b1, b2, 1-b2;
    
    for (int i = 0; i < n; i++) {
        result = B.transpose() * P;
        
        if (observed[i] == '.'){
            if (result(0,0) > 0.5)
                cout << "H";
            else
                cout << "T";
        }
        
        else
            cout << observed[i];

        
        P = A.transpose() * P;
    }
    cout << endl;
}
