#' Inverse of \link[base]{which}.
#'     
#' @description Converts a vector of indices to a logical vector.
#' 
#' @param indices A vector of indices.
#' @param length.out The length of the output
#' @param use.names Whether names of indices should be used.
#' 
#' @return A logical vector of length \code{length.out} with
#' \code{TRUE} at the positions specified in \code{indices} and
#' \code{FALSE} elsewhere.
#' 
#' @author flying sheep (\url{http://stackoverflow.com/a/31114463/4477223})
#' 
#' @examples
#' invwhich(c(5, 3, 7), 10)
#' 
#' @export
invwhich <- function(indices, length.out, useNames = TRUE)
{
  rv <- logical(length.out)
  if (length(indices) > 0)
  {
    rv[indices] <- TRUE
    if (useNames)
      names(rv)[indices] <- names(indices)
  }
  return(rv)
}

