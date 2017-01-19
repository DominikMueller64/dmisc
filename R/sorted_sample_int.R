#' Draw sorted sample of integers.
#'     
#' @description Draw a sorted sample of integers.
#' 
#' @inheritParams base::sample.int
#' 
#' @return A vector of length \code{size} with an increasingly sorted sample
#'  of integers between \code{1} and \code{n}.
#'
#' @author Dominik Mueller
#' 
#' @examples
#' sorted_sample_int(10L, 5L)
#' 
#' @export
sorted_sample_int <- function(n, size, replace = FALSE, prob = NULL) {
  sort(sample.int(n = n, size = size, replace = replace, prob = prob))
}