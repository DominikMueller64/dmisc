#' Transform matrix to data.frame.
#' @description Transform a matrix into a data.frame where each row corresponds to a single
#' matrix entry.
#' 
#' @param mat The matrix to be transformed.
#' 
#' @return A data.frame where one row corresponds to one matrix entry. If the matrix has row or
#' column names, these will be included.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' mat <- matrix(1:12, nrow = 3)
#' mat2df(mat)
#' rownames(mat) <- 1:3 
#' mat2df(mat)
#' colnames(mat) <- letters[1:4]
#' mat2df(mat)
#' rownames(mat) <- NULL
#' mat2df(mat)
#' 
#' @export
mat2df <- function(mat) {
  aix <- arrayInd(ind = which(!is.na(mat)), .dim = dim(mat), useNames = TRUE)
  row_name <- rownames(mat)[aix[, 'row', drop = TRUE]]
  col_name <- colnames(mat)[aix[, 'col', drop = TRUE]]
  df <- data.frame(aix, value = mat[aix])
  if (!is.null(row_name))
    df$row_names <- row_name
  if (!is.null(col_name))
    df$col_names <- col_name
  df
}

