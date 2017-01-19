#' Turn data.frame to list of named vectors.
#' @description Turn a data.frame into a list of named vectors, splitting by another
#' variable.
#' 
#' @param data A \code{data.frame}-like object (\code{data.frame}, \code{tibble}, ...).
#' @param values A character identifying the column holding the values.
#' @param ids A character identifying the column holding the names.
#' @param by A character identifying the column holding the factor for splitting.
#' 
#' @return A list of name vectors.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' dat <- data.frame(chrom = c(1L, 1L, 2L, 2L, 3L, 3L),
#' pos = c(1.4, 5.5, 1.2, 6.8, 3.3, 5.4),
#' id = c('m1', 'm2', 'm3', 'm4', 'm5', 'm6'))
#' dftlov(dat, values = 'pos', ids = 'id', by = 'chrom')
#' 
#' @export
dftlov <- function(data, values, ids, by) {
  # vl <- split(x = dat[[values]], f = dat[[by]])
  # idl <- split(x = dat[[ids]], f = dat[[by]])
  # ret <- purrr::map2(vl, idl, ~ purrr::set_names(.x, .y))
  # Good old base::by does a pretty good job here.
  ret <- by(data = data, INDICES = data[[by]], FUN = function(x) setNames(x[[values]], x[[ids]]))
  class(ret) <- 'list'
  attr(x = ret, which = 'call') <- NULL
  ret
}
