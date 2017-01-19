#include <Rcpp.h>
using namespace Rcpp;

#include "nth_partial_sort.h"
#include "stl_partial_sort.h"

// [[Rcpp::export('.topN')]]
NumericVector topN(NumericMatrix x, int nth) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  NumericVector retval(nrow);
  
  for (int i = 0; i < nrow; i++) {
    NumericVector tmp = nth_partial_sort(x(i,_), nth, false);
    // NumericVector tmp = stl_partial_sort(x(i,_), nth);
    retval[i] = std::accumulate(tmp.begin(), tmp.begin() + nth, 0.0) / nth;
  }
  return retval;
}
