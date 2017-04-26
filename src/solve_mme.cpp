// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::mat inv_sympd(const arma::mat& A) {
  return arma::inv_sympd(A);
}

// [[Rcpp::export(".solve_mme")]]
arma::mat solve_mme(const arma::vec& y,
                    const arma::mat& A,
                    const double lambda) {

  const auto n = A.n_rows;
  const auto n_ts = y.size();
  const auto n_ps = n - n_ts;

  // Ainv
  arma::mat lAinv = lambda * arma::inv_sympd(A);
  for(arma::uword i = n_ps; i < n; ++i) lAinv(i, i) += 1.0;

  // rhs
  auto rhs = arma::vec(n + 1);
  rhs[0] = arma::sum(y);
  for(arma::uword i = 1; i < n_ps + 1; ++i) rhs[i] = 0.0;
  arma::uword j;
  for(arma::uword i = n_ps + 1, j = 0; i < n + 1; ++i, ++j) rhs[i] = y[j];

  // lhs
  auto lhs = arma::mat(n + 1, n + 1);
  lhs(0, 0) = n_ts;
  for(arma::uword i = 1; i < n_ps + 1; ++i) {
    lhs(0, i) = (lhs(i, 0) = 0.0);
  }
  for(arma::uword i = n_ps + 1; i < n + 1; ++i) {
    lhs(0, i) = (lhs(i, 0) = 1.0);
  }
  lhs(arma::span(1, n), arma::span(1, n)) = lAinv;

  return arma::solve(lhs, rhs, arma::solve_opts::fast);

}


// // [[Rcpp::export(".solve_sie")]]
// arma::vec solve_sie(const arma::vec& y,
//                     const arma::mat& A,
//                     const double lambda) {

//   const auto n = A.n_rows;
//   const auto n_ts = y.size();
//   const auto n_ps = n - n_ts;

//   // V_inv
//   // arma::mat Ajj = A(arma::span(n_ps, n - 1), arma::span(n_ps, n - 1));
//   arma::mat Ajj = A.submat(n_ps, n_ps, n - 1, n - 1);
//   Ajj.diag() += lambda;
//   const auto& V_inv = arma::inv_sympd(Ajj);

//   // beta_hat
//   const auto& cs = arma::sum(V_inv, 0);
//   const auto& beta_hat = (cs * y) / arma::sum(cs);
//   // Rcpp::Rcout << "beta : " << beta_hat << std::endl;

//   return A.cols(n_ps, n - 1) * V_inv * (y - arma::ones(n_ts) * beta_hat);
// }



// [[Rcpp::export(".solve_sie")]]
Rcpp::List solve_sie(const arma::vec& y,
                     const arma::mat& A,
                     const double lambda) {

  const auto n = A.n_rows;
  const auto n_ts = y.size();
  const auto n_ps = n - n_ts;

  // V_inv
  // arma::mat Ajj = A(arma::span(n_ps, n - 1), arma::span(n_ps, n - 1));
  arma::mat Ajj = A.submat(n_ps, n_ps, n - 1, n - 1);
  Ajj.diag() += lambda;
  const auto& V_inv = arma::inv_sympd(Ajj);

  // beta_hat
  const auto& cs = arma::sum(V_inv, 0);
  const auto& beta_hat = arma::as_scalar((cs * y) / arma::sum(cs));

  const auto& u_hat = Rcpp::wrap(A.cols(n_ps, n - 1) * V_inv * (y - arma::ones(n_ts) * beta_hat));
  // const arma::vec& u_hat = A.cols(n_ps, n - 1) * V_inv * (y - beta_hat);

  return Rcpp::List::create(Rcpp::Named("beta_hat", beta_hat),
                            Rcpp::Named("u_hat", u_hat));
}


