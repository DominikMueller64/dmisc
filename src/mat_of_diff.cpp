// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <cmath>

double haldane(const double x) {
  return 0.5 * (1.0 - std::exp(-2.0 * x));
}

//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix prob_recomb(const Rcpp::NumericVector& x)
{
  R_xlen_t n = x.size();
  Rcpp::NumericMatrix diff(n, n);

  for (R_xlen_t j = 0; j < n - 1; ++j) {
    for (R_xlen_t i = j + 1; i < n; ++i) {
      diff(i, j) = diff(j, i) = haldane(std::abs(x[i] - x[j]) / 100.0);
    }
  }

  return diff;
}

//' @export
// [[Rcpp::export]]
double segr_var(const Rcpp::NumericVector& pat,
                const Rcpp::NumericVector& mat,
                const Rcpp::NumericVector& pos,
                const Rcpp::NumericVector& p,
                const Rcpp::NumericVector& alpha)
{
  auto n = pat.size();
  double segvar = 0.0;
  for (R_xlen_t j = 0; j < n; ++j) {
    segvar += 0.5 * std::pow(alpha[j], 2.0);
  }

  for (R_xlen_t j = 0; j < n - 1; ++j) {
    for (R_xlen_t i = j + 1; i < n; ++i) {
      auto r = haldane(std::abs(pos[i] - pos[j]) / 100.0);
      auto pij = (pat[i] * pat[j] + mat[i] * mat[j]) / 2.0;
      auto pixpj =  p[i] * p[j];
      auto D = (1.0 - r) * (pij - pixpj);
      segvar += 2.0 * D * alpha[i] * alpha[j];
    }
  }

  return segvar;
}





