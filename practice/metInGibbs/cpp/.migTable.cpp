#include <cstdlib>
#include <cmath>
#include <math.h>
#include <vector>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <map>
#include <ctime>
#include <time.h>


using namespace std;

map<double,double> P;
map<double,double> Q;

double rnorm(double mu, double sig){
  double r = ( (double) rand() / (RAND_MAX) ) ;
  r = floor(r*10000)/10000;
  return( sig*(Q[r])+mu );
}

void readZ(){
  ifstream in;
  double p, q;
  
  in.open("ztable.txt");
  while (in >> p){
    in >> q;
    P[q] = p;
    Q[p] = q;  
  }
  in.close();
}

double la(vector<double> x, double a, double b){
  size_t n = x.size();
  double slx = 0;
  for (size_t i = 0; i < x.size(); i++){
    slx += log(x[i]);
  }
  return ( 4 * log(a) - a/20 + n*(lgamma(a+b)-lgamma(a)) + (a-1)*slx );
}
double lb(vector<double> x, double a, double b){
  size_t n = x.size();
  double slx = 0;
  for (size_t i = 0; i < x.size(); i++){
    slx += log(1-x[i]);
  }
  return ( 7 * log(b) - b/30 + n * (lgamma(a+b)-lgamma(b)) + (b-1) * slx );
}

void mig(vector<double> x){
    ofstream out;
    double csa = 50; double csb = 20; double N = 1000000;
    int cnta = 0; int cntb = 0;
    double canda, candb, r;
    vector< vector<double> > M;

    vector< double > m; m.push_back(5*20); m.push_back(8*30);
    M.push_back(m); M.push_back(m);

    for (double i = 1; i < N; i++){
        
      canda = rnorm(M[i][0], csa);
      if (canda > 0) {
        r = la(x,canda,M[i][1]) - la(x,M[i][0],M[i][1]);
        if (r > log( (double) rand() / (RAND_MAX) ) ){
          M[i][0] = canda;
          cnta++;
        }
      } 
           
      candb = rnorm(M[i][1], csb);
      if (candb > 0){
        r = lb(x,M[i][0],candb)-lb(x,M[i][0],M[i][1]);
        if (r > log( (double) rand() / (RAND_MAX) ) ){
          M[i][1] = candb;
          cntb++;
        }
      }
      
      M.push_back(M[i]);
    }

    cout<< cnta/N <<" "<< cntb/N <<endl;
    M.erase( M.begin(),M.begin()+5000);
    
    out.open("cout.txt");
    for (size_t i = 0; i < M.size(); i++){
      out << M[i][0] <<" "<< M[i][1] <<endl;
    }
    out.close();
}

int main(){
  vector <double> x;
  x.push_back(.81);
  x.push_back(.83);
  x.push_back(.79);
  x.push_back(.75);
  x.push_back(.8);
  readZ();
  mig(x);
  return 0;
}
