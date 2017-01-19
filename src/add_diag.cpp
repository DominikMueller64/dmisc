// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;


// [[Rcpp::export]]
void add_diag_double(arma::mat& x, double epsilon) {
  x.diag() += epsilon;
} 

// [[Rcpp::export]]
void add_diag_double_vec(arma::mat& x, arma::vec& epsilon) {
  x.diag() += epsilon;
} 


// [[Rcpp::export]]
void add_diag_int(arma::imat& x, int epsilon) {
  x.diag() += epsilon;
} 

// [[Rcpp::export]]
void add_diag_int_vec(arma::imat& x, arma::ivec& epsilon) {
  x.diag() += epsilon;
} 




// using namespace Rcpp;
// 
// template <int RTYPE>
// IntegerVector fast_factor_template( const Vector<RTYPE>& x ) {
//   Vector<RTYPE> levs = sort_unique(x);
//   IntegerVector out = match(x, levs);
//   out.attr("levels") = as<CharacterVector>(levs);
//   out.attr("class") = "factor";
//   return out;
// }
// 
// // [[Rcpp::export]]
// SEXP fast_factor( SEXP x ) {
//   switch( TYPEOF(x) ) {
//   case INTSXP: return fast_factor_template<INTSXP>(x);
//   case REALSXP: return fast_factor_template<REALSXP>(x);
//   case STRSXP: return fast_factor_template<STRSXP>(x);
//   }
//   return R_NilValue;
// }