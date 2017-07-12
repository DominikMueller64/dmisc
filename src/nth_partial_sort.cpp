// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

#include "nth_partial_sort.h"

// [[Rcpp::export]]
NumericVector nth_partial_sort(NumericVector x, int nth, bool increasing = true) {
  NumericVector y = clone(x);
  
  if (nth < 1 || nth > x.size()) {
    stop("Inadmissible index  value for 'nth'.");
  }
  
  if (increasing) {
    std::nth_element(y.begin(), y.begin() + nth, y.end());
  } else {
    std::nth_element(y.begin(), y.begin() + nth, y.end(), std::greater<double>());
  }
  return y;
}
