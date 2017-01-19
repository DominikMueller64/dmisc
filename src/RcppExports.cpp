// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// add_diag_double
void add_diag_double(arma::mat& x, double epsilon);
RcppExport SEXP dmisc_add_diag_double(SEXP xSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    add_diag_double(x, epsilon);
    return R_NilValue;
END_RCPP
}
// add_diag_double_vec
void add_diag_double_vec(arma::mat& x, arma::vec& epsilon);
RcppExport SEXP dmisc_add_diag_double_vec(SEXP xSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type epsilon(epsilonSEXP);
    add_diag_double_vec(x, epsilon);
    return R_NilValue;
END_RCPP
}
// add_diag_int
void add_diag_int(arma::imat& x, int epsilon);
RcppExport SEXP dmisc_add_diag_int(SEXP xSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type epsilon(epsilonSEXP);
    add_diag_int(x, epsilon);
    return R_NilValue;
END_RCPP
}
// add_diag_int_vec
void add_diag_int_vec(arma::imat& x, arma::ivec& epsilon);
RcppExport SEXP dmisc_add_diag_int_vec(SEXP xSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::ivec& >::type epsilon(epsilonSEXP);
    add_diag_int_vec(x, epsilon);
    return R_NilValue;
END_RCPP
}
// col_scale_inplace
void col_scale_inplace(arma::mat& x, const arma::vec& center, const arma::vec& scale);
RcppExport SEXP dmisc_col_scale_inplace(SEXP xSEXP, SEXP centerSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type center(centerSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type scale(scaleSEXP);
    col_scale_inplace(x, center, scale);
    return R_NilValue;
END_RCPP
}
// col_scale_copy
arma::mat col_scale_copy(const arma::mat& x, const arma::vec& center, const arma::vec& scale);
RcppExport SEXP dmisc_col_scale_copy(SEXP xSEXP, SEXP centerSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type center(centerSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type scale(scaleSEXP);
    rcpp_result_gen = Rcpp::wrap(col_scale_copy(x, center, scale));
    return rcpp_result_gen;
END_RCPP
}
// match_dbl_cpp
IntegerVector match_dbl_cpp(NumericVector x, NumericVector table, int nomatch, double tolerance);
RcppExport SEXP dmisc_match_dbl_cpp(SEXP xSEXP, SEXP tableSEXP, SEXP nomatchSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type table(tableSEXP);
    Rcpp::traits::input_parameter< int >::type nomatch(nomatchSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(match_dbl_cpp(x, table, nomatch, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// nth_partial_sort
NumericVector nth_partial_sort(NumericVector x, int nth, bool increasing);
RcppExport SEXP dmisc_nth_partial_sort(SEXP xSEXP, SEXP nthSEXP, SEXP increasingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nth(nthSEXP);
    Rcpp::traits::input_parameter< bool >::type increasing(increasingSEXP);
    rcpp_result_gen = Rcpp::wrap(nth_partial_sort(x, nth, increasing));
    return rcpp_result_gen;
END_RCPP
}
// stl_partial_sort
NumericVector stl_partial_sort(NumericVector x, int n, bool increasing);
RcppExport SEXP dmisc_stl_partial_sort(SEXP xSEXP, SEXP nSEXP, SEXP increasingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< bool >::type increasing(increasingSEXP);
    rcpp_result_gen = Rcpp::wrap(stl_partial_sort(x, n, increasing));
    return rcpp_result_gen;
END_RCPP
}
// topN
NumericVector topN(NumericMatrix x, int nth);
RcppExport SEXP dmisc_topN(SEXP xSEXP, SEXP nthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nth(nthSEXP);
    rcpp_result_gen = Rcpp::wrap(topN(x, nth));
    return rcpp_result_gen;
END_RCPP
}