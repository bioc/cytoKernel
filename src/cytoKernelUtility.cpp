
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(.tracecpp)]]
double tracecpp(const NumericMatrix& x) {
  int nr = x.nrow();
  double sum = 0;
  for (int i = 0; i < nr; i++) {
    sum += x(i,i);
  }
  return sum;
}

// [[Rcpp::export(.inverseLogitcpp)]]
long double inverseLogitcpp(long double x) {
  long double y=0;
  y= exp(x)/(1+exp(x));
  return y;
}

// [[Rcpp::export(.distKernelcpp)]]
long double distKernelcpp(long double x1, long double x2,
                          long double rho) {
  long double t=0;
  t= exp(-((x1-x2)*(x1-x2))/rho);
  return t;
}

// [[Rcpp::export(.DEfeatures)]]
int DEfeatures(NumericVector p, long double a) {
  int DEfeatures=0;
  DEfeatures= sum(p<= a);;
  return DEfeatures;
}

// [[Rcpp::export(.rowMaxcpp)]]
NumericVector rowMaxcpp(const NumericMatrix& x) {
  int nr = x.nrow(); 
  NumericVector maxVec(nr);
    for (int i = 0; i < nr; i++) {
      maxVec[i] = max(x(i,_));
    }
  return maxVec;
}