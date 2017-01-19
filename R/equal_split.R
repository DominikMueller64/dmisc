#' Split a set x into subset of nearly equal size.
#' 
#' Split a set x stored in a \code{vector} or \code{list} into a number of maximally equally
#' sized subsets.
#' 
#' @param x The set of value to be split into subsets, a \code{vector} or \code{list}.
#' @param n The desired number of subsets.
#' @param random Should the elements of the subsets be randomly sampled from \code{x}?
#' @param beginning Should elements in excess should be added to the subsets from the
#' beginning of the output or randomly assigned?
#' 
#' @return A list containing the subsets. An attribte \code{'indices'} contained the 
#' indices of the entries that contain the excess elements.
#' 
#' @details Note that if the number of desired subsets \code{n} is larger than the supplied set
#' \code{x}, the output will still be a list with length \code{n} where \code{length(x)} entries
#' will contain exactly one element and \code{n - length(x)} entries will be empty.
#'
#' @author Domink Mueller (\email{dominikmueller64@yahoo.de})
#' 
#' @examples
#' x <- seq.int(from = 1L, to = 10L)
#' n <- 3L
#' equal_split(x, n, random = TRUE, beginning = FALSE)
#' equal_split(x, n, random = TRUE, beginning = TRUE)
#' equal_split(x, n, random = FALSE, beginning = FALSE)
#' equal_split(x, n, random = FALSE, beginning = TRUE)
#' 
#' # lists as input work as well
#' equal_split(as.list(x), n, random = TRUE, beginning = FALSE)
#'
#' A too large number of desired subsets is handeled gracefully.
#'equal_split(x, n = 20L, random = TRUE, beginning = FALSE)
#'
#' @export
equal_split <- function(x, n, random = TRUE, beginning = FALSE) {
  len <- length(x)
  base_size <- len %/% n
  rem <- len %% n
  sizes <- rep(base_size, times = n)
  ix <- if (beginning) seq.int(from = 1L, to = rem) else sample.int(n, size = rem)
  sizes[ix] <- sizes[ix] + 1L
  
  ret <- purrr::map(sizes, function(size) { 
    if (random) {
      smp <- unlist(sample(x, size), recursive = FALSE, use.names = FALSE) 
    } else {
      smp <- x[seq.int(from = 1L, to = size)]
    }
    x <<- setdiff(x, smp)
    smp
  })
  attr(ret, which = 'indices') <- ix
  
  ret
}