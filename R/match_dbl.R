#' @useDynLib dmisc
#' @importFrom Rcpp sourceCpp
NULL
#'
#'
#' A version of \code{match} working exclusively on numeric data.
#'     
#' @description \code{match_dbl} returns a vector of the positions of (first) matches
#' of its first argument in its second.
#' 
#' @param x A numeric vector with the values to be matched.
#' @param table A numeric vector with the values to be matched against.
#' @param nomatch An integer as the value to be returned in the
#' case when no match is found.
#' @param numeric scalar >= 0. Differences smaller than `tolerance`
#' are not recognized. The default value is close to 1.5e-8.
#' 
#' @return An integer vector giving the position in table of the first match
#' if there is a match, otherwise nomatch.
#' 
#' @author Dominik Mueller
#' 
#' @seealso \code{\link{\%in_dbl\%}} for an numerical analog to
#' \code{\link[base]{\%in\%}}
#' 
#' @examples
#' # generate some random numeric data
#' set.seed(123)
#' table <- runif(1000L)
#' table <- sample(c(table, table)) # 'table' now contains duplicates
#' x <- sample(table, 100L)
#' 
#' m1 <- match(x, table)
#' m1_dbl <- match_dbl(x, table)
#' identical(m1, m1_dbl) # TRUE according to expectation
#' 
#' microbenchmark::microbenchmark(match(x, table),
#'                                match_dbl(x, table)) # speed is fine
#' 
#' # minimally disturb x
#' x <- x + runif(n = length(x), min = -1e-10, max = 1e-10)
#' 
#' identical(m1, match(x, table)) # now FALSE 
#' identical(m1_dbl, match_dbl(x, table)) # still TRUE
#' identical(m1_dbl, match_dbl(x, table, tolerance = 1e-11)) # also FALSE now
#'
#' 
#' 
#' @export
match_dbl <- function(x, table, nomatch = NA_integer_,
                      tolerance = sqrt(.Machine$double.eps)) {
  
  if (!is.integer(nomatch))
    stop("'nomatch' must be an integer'")
  
  if (!is.numeric(tolerance) || tolerance <= 0.0)
    stop("'tolerance' must be a positive number")
  
  match_dbl_cpp(x, table, nomatch, tolerance)
}


