// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector match_dbl_cpp(NumericVector x, NumericVector table,
                        int nomatch, double tolerance) {

  int n = x.size();
  int m = table.size();
  IntegerVector out(n, nomatch);

  for (int i = 0; i < n; ++i) {
    int j = 0;
    while (j < m) {
      if (std::abs(x[i] - table[j]) < tolerance) {
        out[i] = j + 1;
        break;
      }
      ++j;
    }
  }
  return out;
}


