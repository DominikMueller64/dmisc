#' Split matrix
#'     
#' @description Split a matrix by rows or columns into a list.
#' 
#' @param x A matrix to by split
#' @param Either \code{'row'} or \code{'col'} indicating the direction of 
#' the split. Defaults to \code{'row'}.
#' 
#' @return A list containing the row or columns of the matrix.
#' 
#' @details A shorthand not preserving rownames is \code{split(x, row(x))}
#' for splitting by rows and \code{split(x, col(x))} for splitting by
#' columns.
#'
#' @author Dominik Mueller
#' 
#' @examples
#' x <- matrix(seq_len(4L), nrow = 2L) 
#' dimnames(x) <- list(c('a', 'b'), c('c', 'd'))
#' split_matrix(x, 'row')
#' split_matrix(x, 'col')
#' 
#' 
#' 
#' @export
split_matrix <- function(x, by = c('row', 'col')) {
  
  by <- match.arg(arg = by)
  if (!is.matrix(x))
    stop("'X' must be a matrix.")
  
  if (by == 'row') {
    s <- seq_len(nrow(x))
    ret <- purrr::map(s, ~x[., , drop = FALSE])
    names(ret) <- rownames(x)
  } else if (by == 'col') {
    s <- seq_len(ncol(x))
    ret <- purrr::map(s, ~x[, ., drop = FALSE])
    names(ret) <- colnames(x)
  }
  ret
}