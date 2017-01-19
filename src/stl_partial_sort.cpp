#include <Rcpp.h>
using namespace Rcpp;

#include "stl_partial_sort.h"

// [[Rcpp::export]]
NumericVector stl_partial_sort(NumericVector x, int n, bool increasing = true) {
  NumericVector y = clone(x);
  
  if (n < 1 || n > x.size()) {
    stop("Inadmissible index  value for 'nth'.");
  }
  
  if (increasing) {
    std::partial_sort(y.begin(), y.begin()+n, y.end());
  } else {
    std::partial_sort(y.begin(), y.begin()+n, y.end(), std::greater<double>());
  }
  return y;
}
