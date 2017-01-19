#' A version of \code{\link{\%in\%}} working exclusively on numeric data.
#'     
#' @description \code{\%in_dbl\%} is an intuitive interface analogous to
#' \code{\link[base]{\%in\%}} as binary operator, which returns a logical vector indicating
#' if there is a match or not for its left operand
#' 
#' @inheritParams match_dbl
#' 
#' @details \code{\%in_dbl\%} is defined as
#' 
#' \code{`\%in_dbl\%` <- function(x, table) match_dbl(x, table, nomatch = 0) > 0}
#' 
#' @return A vector of the same length as x. A logical vector, indicating if a
#' match was located for each element of x: thus the values are TRUE or FALSE
#' and never NA.
#' 
#' @author Dominik Mueller
#' 
#' @seealso \code{\link{match_dbl}}
#' 
#' @export
#' @rdname in_dbl
`%in_dbl%` <- function(x, table, tolerance = sqrt(.Machine$double.eps)) {
  match_dbl(x, table, nomatch = 0L, tolerance = tolerance) > 0L
}
  