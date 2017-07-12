// [[Rcpp::plugins(cpp11)]]
// #include <Rcpp.h>
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export('col_scale_inplace')]]
void col_scale_inplace(arma::mat& x, const arma::vec& center,
                       const arma::vec& scale) {
  int len_center = center.n_elem;
  int len_scale = scale.n_elem;
  int nrow = x.n_rows;
  int ncol = x.n_cols;
  double c = center[0];
  double s = scale[0];
  bool lc =  (len_center > 1);
  bool ls =  (len_scale > 1);
  for (int j = 0; j < ncol; j++) {
    if (lc) {
      c = center[j];
    }
    if (ls) {
      s = scale[j];
    }
    for (int i = 0; i < nrow; i++) {
      x.at(i, j) = (x.at(i, j) - c) / s;
      // double* ptr = &x[i, j]; // Weiredly, this is way slower.
      // *ptr = (*ptr - c) / s;
    }
    // Armadilo uses column-major order, i.e., data are store column-by-column.
  }
}


// [[Rcpp::export('col_scale_copy')]]
arma::mat col_scale_copy(const arma::mat& x, const arma::vec& center,
                       const arma::vec& scale) {
  mat y(x);
  col_scale_inplace(y, center, scale);
  return y;
}
