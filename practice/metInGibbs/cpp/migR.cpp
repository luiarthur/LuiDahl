#include <Rcpp.h>

using namespace Rcpp;

double la(NumericVector x, double a, double b){
  size_t n = x.size();
  double slx = 0;
  for (size_t i = 0; i < x.size(); i++){
    slx = slx + log(x[i]);
  }
  return ( 4 * log(a) - a/20 + n*(lgamma(a+b)-lgamma(a)) + (a-1)*slx );
}
double lb(NumericVector x, double a, double b){
  size_t n = x.size();
  double slx = 0;
  for (size_t i = 0; i < x.size(); i++){
    slx = slx + log(1-x[i]);
  }
  return ( 7 * log(b) - b/30 + n * (lgamma(a+b)-lgamma(b)) + (b-1) * slx );
}

// [[Rcpp::export]]
NumericMatrix mig2( Rcpp::NumericVector x){
    //NumericVector x = NumericVector::create(.81, .83, .79, .75, .8 );
    double csa = 50; double csb = 20; double N = 100000;
    int cnta = 0; int cntb = 0;
    double canda, candb, r;
    NumericMatrix out(N,2);

         out(0,1) = 5 * 20;
         out(0,2) = 8 * 30;

    for (double i = 1; i < N; i++){
        
           out(i,_) = out(i-1,_);
           
           canda = rnorm(1, out(i,1), csa)[0];
           if (canda > 0) {
             r = la(x,canda,out(i,2)) - la(x,out(i,1),out(i,2));
             if (r>log(runif(1)[0])){
               out(i,1) = canda;
               cnta = cnta + 1;
             }
           } 
           
           candb = rnorm(1,out(i,2), csb)[0];
           if (candb>0){
             r = lb(x,out(i,1),candb)-lb(x,out(i,1),out(i,2));
             if (r > log(runif(1)[0])) {
               out (i,2) = candb;
               cntb = cntb + 1;
             }
           }

         //print(cnta/N); print(cntb/N);
         return(out( Range(5000,1), Range(5000,1) ) );
    }
}


