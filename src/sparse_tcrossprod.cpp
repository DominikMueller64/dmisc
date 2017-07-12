// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cstddef> // std::size_t

// This function is (unfortunately) way slower compared to
// subsetting + tcrossprod.
// // [[Rcpp::export(".sparse_tcrossprod")]]
arma::mat tcrossprod(const arma::mat& x,
                     const arma::mat& y,
                     const arma::uvec& nonzero)
{
  const arma::mat& xx = x.cols(nonzero);
  const arma::mat& yy = y.cols(nonzero);
  std::size_t n = x.n_rows;
  std::size_t p = y.n_rows;
  arma::mat xyt(n, p);
  for (size_t i = 0; i != n; ++i) {
    for (size_t j = i; j != p; ++j){
      double prod = arma::dot(xx.row(i),yy.row(j));
      if (i != j) {
        xyt(i, j) = (xyt(j, i) = prod);
      } else {
        xyt(i, i) = prod;
      }
    }
  }
  return xyt;
}


