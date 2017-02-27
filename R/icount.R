#' A count iterator.
#'
#' Constructs a count iterator with start, (maximum) count and stepsize.
#'
#' @param start An integer. Start of the count iterator (default = 0L)
#' @param count An integer. Stop of the count iterator (default = NULL).
#' Never ceases counting by default.
#' @param step An integer. Step size of the count iterator (default = 1L).
#' @return A count iterator.
#'
#' @details The use of negative integers is also possible. If \code{step} is
#' negative, this will generate a decreasing sequence. If \code{step} is zero,
#' it will generate a constant, neverending sequence, independent of
#' \code{count}.
#'
#' @author Domink Mueller (\email{dominikmueller64@yahoo.de})
#'
#' @seealso \code{\link[iterators]{icount}} and
#' \code{\link[itertools2]{icount}}.
#'
#' @examples
#' ct <- icount(start = 3L, count = 10L, step = 3L)
#' iterators::nextElem(ct)
#' iterators::nextElem(ct)
#' iterators::nextElem(ct)
#' iterators::nextElem(ct)
#'
#' @export
icount <- function (start = 0L, count = NULL, step = 1L){
  start <- as.integer(start)
  step <- as.integer(step)
  if (!is.null(count))
    count <- as.integer(count)
  for (what in c('start' = start, 'count' = count, 'step' = step))
    if (length(what) != 1L)
      stop(sprintf("'%s' must be a numeric value of length 1", names(what)))

    i <- start - step
    nextElem <- function() {
      ## if (is.null(count) || i + step < count)
      if(is.null(count) ||
           (step >= 0L && (i + step <= count)) ||
           (step < 0L && (i + step >= count)))
        (i <<- i + step)
      else stop('StopIteration', call. = FALSE)
    }
    it <- list(nextElem = nextElem)
    class(it) <- c('abstractiter', 'iter')
    it
}
