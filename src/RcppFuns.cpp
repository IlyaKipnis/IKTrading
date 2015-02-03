#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector computeHaOpen(NumericVector haOpen, NumericVector haClose) {
  int n = haOpen.size();
  for(int i=1; i<n; i++) {
    haOpen[i] = (haOpen[i-1]+haClose[i-1])/2;
  }
  return haOpen;
}