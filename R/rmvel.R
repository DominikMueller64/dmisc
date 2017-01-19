#' Remove elements from a vector or list
#'     
#' @description \code{rmvel} removes specified elements from a
#' vector or list.
#' 
#' @param x the original data
#' @param ... elements that should be removed
#' 
#' @details This function is primariy intended for integers and
#' character literals. Doubles are not specifically treated.
#' 
#' @return A vector or list with the specified elements being removed.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' rmvel(list(5, 7, 'hi'), 'hi', 7, 90)
#' 
#' @export
rmvel <- function(x, ...) {
  dots <- list(...)
  x[!x %in% dots]
}

