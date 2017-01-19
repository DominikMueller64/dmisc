#' Draw sorted sample.
#'     
#' @description Draw a sorted random sample from a vector, conserving
#' the original order of the elements. 
#' 
#' @inheritParams base::sample
#' 
#' @return A vector with the sample according to the original order.
#'
#' @details Note that the sample is not actually sorted, but reorder by
#' matching it back with \code{\link[base]{match}} to the original vector.
#'
#' @author Dominik Mueller
#' 
#' @examples
#' sorted_sample(letters, 5L)
#' 
#' sorted_sample(letters[seq_len(3L)], 10L, replace = TRUE)
#' 
#' @export
sorted_sample <- function(x, size, replace = FALSE, prob = NULL) {
  smp <- sample(x, size, replace, prob)
  ord <- match(smp, x)
  smp[order(ord)]
}