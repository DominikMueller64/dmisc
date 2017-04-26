// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <string>
#include <vector>



//' @export
// [[Rcpp::export]]
double routine(const std::vector<std::string>& vec)
{
  std::string ccstr;
  ccstr.reserve(vec.size());
  for (const auto& chr: vec)
    ccstr.append(chr);
  return std::stod(ccstr);
}


//' @export
// [[Rcpp::export]]
std::vector<std::string> string_collapse(const Rcpp::DataFrame& data)
{
  R_xlen_t nrow = data.nrow();
  R_xlen_t ncol = data.ncol();
  std::vector<std::string> ret(ncol);
  for (R_xlen_t j = 0; j < ncol; ++j) {
    const auto& col = Rcpp::as<Rcpp::NumericVector>(data[j]);
    std::string ccstr;
    ccstr.reserve(nrow);
    for (const auto& chr: col) {
      ccstr += std::to_string(chr)[0];
    }
    ret[j] = ccstr;
  }
  return ret;
}


