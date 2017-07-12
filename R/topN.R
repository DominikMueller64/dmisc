#' Average value of the top elements in each row of a matrix.
#'     
#' @description Takes a matrix and computes the arithmetic mean of the top elemens for
#' each single row.
#' 
#' @param x a numeric matrix
#' @param nth an integer giving the desired number of top elemens
#' 
#' @return A numeric vector containing the respective means of the top \code{nth}
#' elements in each row.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' (x <- apply(matrix(seq_len(16), ncol = 4), 2L, sample))
#' topN(x, 2L)
#' 
#' # benchmarking with a pure R solution
#' x <- matrix(rnorm(1000L ^ 2L), ncol = 1000L)
#' microbenchmark::microbenchmark(times = 1L,
#'   a <- apply(x, 1L, function(x) mean(head(sort(x, decreasing = TRUE), 20L))),
#'   b <- topN(x, 20L)
#' )
#' all(dplyr::near(a, b))
#' 
#' @export
topN <- function(x, nth) {

  if (!is.matrix(x) || !is.numeric(x))
    stop("'x' must be a numeric matrix.")
  
  if (nth < 1L || ncol(x) < nth)
    stop("'nth' must be an integer between 1 and 'ncol(x)'.")
  
  topN_rcpp(x, nth)
}

