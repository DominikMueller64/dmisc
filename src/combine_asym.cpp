// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <numeric> // std::accumulate
#include <cstddef> // std::size_t

using namespace Rcpp;

// [[Rcpp::export(".combine_asym_arma")]]
arma::mat combine_asym_arma(const Rcpp::List& x,
                            const arma::ivec& rows,
                            const arma::ivec& cols)
{
  std::size_t n = std::accumulate(rows.begin(), rows.end(), 0);
  std::size_t m = std::accumulate(cols.begin(), cols.end(), 0);
  arma::mat mat(n, m); // implicitly filled with 0;

  std::size_t cib = 1;
  for (size_t ib = 0; ib != rows.size(); ++ib) {
    std::size_t cjb = 1;
    const Rcpp::List& xib = x[ib];
    for (size_t jb = 0; jb != cols.size(); ++jb) {
      const arma::mat& xijb = xib[jb];
      // const auto& xijb = x[ib][jb];
      for (size_t j = 0; j != cols(jb); ++j) {
        for (size_t i = 0; i != rows(ib); ++i) {
          mat(cib + i - 1, cjb + j - 1) = xijb(i, j);
        } // i
      } // j
      cjb += cols(jb);
    } // jb
    cib += rows(ib);
  } // ib

  return mat;
}


// // [[Rcpp::export(".combine_sym_arma")]]
// arma::mat combine_sym_arma(const Rcpp::List& x,
//                            const arma::ivec& rows,
//                            const arma::ivec& cols)
// {
//   std::size_t n = std::accumulate(rows.begin(), rows.end(), 0);
//   std::size_t m = std::accumulate(cols.begin(), cols.end(), 0);
//   arma::mat mat(n, m); // implicitly filled with 0;

//   std::size_t cib = 1;
//   for (size_t ib = 0; ib != rows.size(); ++ib) {
//     std::size_t cjb = 1;
//     const Rcpp::List& xib = x[ib];
//     for (size_t jb = ib; jb != cols.size(); ++jb) {
//       const arma::mat& xijb = xib[jb];
//       // const auto& xijb = x[ib][jb];

//       if (ib == jb) { // diagonal block
//         for (size_t i = 0; i != rows(ib); ++i) {
//           for (size_t j = i; j != cols(jb); ++j) {
//             if (i == j) { // main diagonal
//               mat(i, i) = xijb(i, i);
//             } else {
//               mat(j, i) = (mat(i, j) = xijb(i, j));
//             }
//           }
//         }
//       } else { // off-diagonal block
//         for (size_t i = 0; i != rows(ib); ++i) {
//           for (size_t j = 0; j != cols(jb); ++j) {
//             mat(cjb + j - 1, cib + i - 1) = (mat(cib + i - 1, cjb + j - 1) = xijb(i, j));
//           } // i
//         } // j
//       }
//       cjb += cols(jb);
//     } // jb
//     cib += rows(ib);
//   } // ib

//   return mat;
// }


