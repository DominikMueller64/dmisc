#' @title Create a function for re-mapping values.
#'
#' @description This is a function factory for creating a function that maps old values
#' to new ones.
#'
#'
#' @param old Vector. The old values.
#' @param new Vector. The new values.
#' @param na_when_missing Logical. If a value cannot be found, should \code{NA} be used or
#' value queried for? (defaults to \code{TRUE}).
#' @param fun A function applied to the result before returning.
#' @param \dots Further arguments for \code{fun}.
#' @return A vector with re-mapped values.
#'
#' @details The factory internally uses a \code{\link[hashmap]{hashmap}} for mapping old to new
#' values. For the possible data types of \code{old} and \code{new}, consult the
#' documentation of \code{\link[hashmap]{hashmap}} (Here, \code{keys} corresponds to \code{old}
#' and \code{values} to \code{new}).
#'
#' @author Domink Mueller (\email{dominikmueller64@yahoo.de})
#'
#' @examples
#' old <- letters
#' new <- toupper(old)
#' rmp <- remapper(old = old, new = new)
#' x <- c('a', 'qrz', 'd')
#' rmp(x)
#' remapper(old = old, new = new, na_when_missing = FALSE)(x)
#'
#' @export
remapper <- function(old, new, na_when_missing = TRUE, fun = NULL, ...){
  map <- hashmap::hashmap(keys = old, values = new)
  function(x) {
    val <- map[[x]]
    if (!na_when_missing) {
      has_keys <- map$has_keys(x)
      val[!has_keys] <- x[!has_keys]
    }
    if (is.function(fun)) {
      val <- lapply(X = val, FUN = fun, ...)
      val <- unlist(x = val, recursive = FALSE, use.names = TRUE)
    }
    val
  }
}
