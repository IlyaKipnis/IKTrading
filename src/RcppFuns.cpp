#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector computeHaClose(NumericVector haOpen, NumericVector haClose) {
  int n = haClose.size();
  for(int i=1; i<n; i++) {
    haClose[i] = (haOpen[i-1]+haClose[i-1])/2;
  }
  return haClose;
}