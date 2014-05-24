#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector computeHaClose(NumericVector haOpen, NumericVector haClose) {
  int n = haClose.size();
  for(int i=1; i<n; i++) {
    haClose[i] = (haOpen[i-1]+haClose[i-1])/2;
  }
  return haClose;
}