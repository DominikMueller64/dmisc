#' @title Bootstrapping of correlation coefficients
#'
#' @description Function of bootstrapping of correlation coefficients
#'
#' @param x double vector. The observations of the first variable.
#' @param y double vector. The observations of the second variable.
#' @param n integer. The number of replicates.
#' @param \dots further arguments to the function \code{\link[base]{cor}}
#'
#' @return A vector of \code{n} bootstrapping samples.
#'
#' @examples
#' ## Sample some data
#' set.seed(123L)
#' x <- rnorm(50)
#' y <- x + rnorm(length(x))
#' cor(x, y)
#' ## bootstrapping
#' bs <- bootstrap(x, y)
#' ## bootstrap distribution
#' hist(bs)
#' ## mean and standard-deviation of bootstrap distribution
#' c(mean(bs), sd(bs))
#' ## two-sided 5% quantiles
#' quantile(x = bs, probs = c(0.025, 0.975))
#'
#' @export
bootstrap <- function(x, y, n = 1e4L, ...) {

  if (!(k <- length(x)) == length(y))
    stop("'x' and 'y' must have equal length.")

  n <- as.integer(n)
  if (!is.integer(n) || length(n) != 1L || n <= 0L)
    stop("'n' must be a positive integer.")

  retval <- numeric(n)
  for (i in seq_len(n)) {
    idx <- sample.int(k, replace = TRUE)
    retval[i] <- cor(x[idx], y[idx], ...)
  }
  retval
}
