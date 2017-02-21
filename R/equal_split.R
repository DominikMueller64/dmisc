#' Split a set x into subset of nearly equal size.
#'
#' Split a set x stored in a \code{vector} or \code{list} into a number of maximally equally
#' sized subsets.
#'
#' @param x The set of value to be split into subsets, a \code{vector} or \code{list}.
#' @param n The desired number of subsets.
#' @param random Should the elements of the subsets be randomly sampled from \code{x}?
#' @param beginning Should excess elements be added starting at the beginning of
#' the subsets or randomly assigned? 
#' @return A list containing the subsets. If set sizes are unequal, an attribute
#' \code{'indices'} is added and containes the indices of the subsets to which excess
#' elements were added.
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
  if (rem > 0L) {
    ix <- if (beginning) seq.int(from = 1L, to = rem) else sample.int(n, size = rem)
    sizes[ix] <- sizes[ix] + 1L
  }

  s <- seq_len(length(x))
  ret <- purrr::map(sizes, function(size) {
    if (random) {
      smp <- sample(x = s, size = size)
    } else {
      smp <- s[seq.int(from = 1L, to = size)]
    }
    s <<- setdiff(s, smp)
    x[smp]
  })
  if (rem > 0L)
    attr(ret, which = 'indices') <- ix
  ret
}
