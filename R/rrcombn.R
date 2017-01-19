#' All round robin combinations.
#'     
#' @description Returns all pairs of elements occuring in a round robin design.
#' 
#' @param x a vector or list
#' @param closed Whether the round robin design is closed, i.e., the last element is
#' paired up with the first.
#' @param simplify Whether the result should be simplified to a matrix.
#' 
#' @return A list or matrix containing all combinations.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' rrcombn(letters) 
#' 
#' @export
rrcombn <- function(x, closed = TRUE, simplify = TRUE) {
  # produce all combination of a reduced round robin design where that last item of 'x' is not
  # being crossed with the first ('open circle').
  n <- length(x)
  if (length(x) == 0L)
    return(x)

  out <- lapply(X = seq_len(length.out = n - 1L), function(i) c(x[[i]], x[[i + 1L]]))
  if (closed) 
    out[[n]] <- c(x[[n]], x[[1L]])
  
  if (simplify)
    return(do.call(what = cbind, args = out))

  out
}