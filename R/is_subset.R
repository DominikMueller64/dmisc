#' Test if one set is a subset of another.
#'     
#' @description \code{is_subset} tests if a set \code{x} is a subset
#' of another set \code{y}
#' 
#' @param x A vector containing the first set.
#' @param y A vector containing the second set.
#' 
#' @details This functions takes special care of numeric input and
#' uses \code{\link[dmisc]{\%in_dbl\%}} in this case.
#' 
#' @return A logical vector of length one. It is \code{TRUE} if all the
#' values in \code{x} are found in \code{y} and \code{FALSE} otherwise.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' x <- c(1 - 1e-9, 2 + 1e-10)
#' y <- c(1, 2, 3)
#' all(x %in% y) # returns FALSE
#' is_subset(x, y) # returns TRUE
#' 
#' is_subset(letters[1:5], letters)
#' is_subset(c(1L, 3L), c(1.0, 6.0, 3.0))
#' @export
is_subset <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  
  f <- purrr::partial(`%in_dbl%`, tolerance = tolerance)
  x_ <- deparse(substitute(x))
  y_ <- deparse(substitute(x))
  fmt <- '%s cannot be coerced to numeric'
  
  if (purrr::is_double(x)) {
    if (!purrr::is_double(y)) {
      y <- tryCatch(as.numeric(y), 
                    error = function(e) cat(sprintf(fmt, y_)))
    }
  } else {
    if (purrr::is_double(y)) {
      x <- tryCatch(as.numeric(x),
                    error = function(e) cat(sprintf(fmt, x_)))
    } else {
      f <- `%in%` # neither x nor y are double
    }
  }
  all(f(x, y))
}

