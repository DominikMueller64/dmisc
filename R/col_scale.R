#' Center and scale a matrix.
#'     
#' @description Center and scale a matrix using given vectors for
#' centering and scaling.
#' 
#' @param x A numeric matrix, preferable with storage mode double
#' as the calculations might be faster.
#' @param center Logical or a numeric vector (length one or \code{ncol(x)} being
#' subtracted from the columns of \code{x} for centering.
#' @param scale Logical or a numeric vector (length one or \code{ncol(x)} to 
#' divide the columns of \code{x} for scaling.
#' @param inplace Logical. If \code{TRUE}, the matrix is modified in memory,
#' i.e., no copy is being made. This is faster and saves
#' memory, but only possible if storage mode is double. If \code{FALSE},
#' a copy is made, modified and returned.
#' 
#' @return Either a modified copy of the original matrix if
#' \code{inplace = FALSE}, or nothing if \code{inplace = TRUE}.
#' 
#' @details Arguments to \code{center} and \code{scale} can be either logical
#' or numeric vectors. If \code{TRUE}, centering is done by column means
#' and scaling by the sample standard deviation of the columns. If arguments are
#' numerical, they must be either of length one (recycled to the number of
#' columns of \code{x}) or of length equal to \code{ncol(x)}. Arguments to
#' \code{center} and \code{scale} are independent.
#' 
#' This function was specifically desiged for scaling with known \code{center}
#' and \code{scale} arguments. Note that maximum speed is achieved if the
#' matrix has storage mode double and is modified in 
#' memory (\code{inplace = TRUE}).
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' x <- matrix(as.double(1:20), nrow = 4)
#' col_scale(x, center = 4, scale = TRUE)
#' 
#' x <- matrix(rnorm(1e6L), nrow = 1e2L, ncol = 1e4L)
#' center <- colMeans(x)
#' scaling <- matrixStats::colSds(x, center = center)
#' xx <- x + 0.0
#' xx_ <- x + 0.0
#' 
#' microbenchmark::microbenchmark(times = 10L,
#'  scale(x, center = center, scale = scaling),
#'  scale(xx, center = center, scale = scaling),
#'  col_scale(x, center, scaling),
#'  col_scale(xx, center, scaling),
#'  col_scale(xx, center, scaling, inplace = TRUE)
#' )
#' 
#' @export
col_scale <- function(x, center = FALSE, scale = FALSE, inplace = FALSE) {
  
  if (!is.numeric(x))
    stop("'x' must be numeric.")
  
  x <- as.matrix(x)
  
  if (storage.mode(x) == 'integer') {
    if (inplace)
      stop("'x' is an integer matrix and inplace modification not possible")
    
    warning(paste("'x' is an integer matrix and will be converted to double,",
                  'which costs time!'))
    storage.mode(x) <- 'double'
  }
  
  nc <- ncol(x)
  if (is.logical(center)) {
    if (center) {
      center <- colMeans(x, na.rm = TRUE)
    } else {
      center <- 0.0
    }
  } else if (is.numeric(center) && (length(center) %in% c(1L, nc))) {
    center <- center
  } else stop("Length of 'center' must be one or equal the number of columns of 'x'.")
  
  if (is.logical(scale)) {
    if (scale) {
      if (length(center) == 1L) 
      scale <- matrixStats::colSds(x, center = colMeans(x, na.rm = TRUE))
    } else {
      scale <- 1.0
    }
  } else if (is.numeric(scale) && length(scale) %in% c(1L, nc)) {
    scale <- scale
  } else stop("Length of 'scale' must be one or equal the number of columns of 'x'.")
  
  if (!inplace) {
    x <- .col_scale_copy(x, center, scale)
    attr(x, "scaled:center") <- center
    attr(x, "scaled:scale") <- scale
    return(x)
  } else {
    attr(x, "scaled:center") <- center
    attr(x, "scaled:scale") <- scale
    .col_scale_inplace(x, center, scale)
  }
}