#' Add to the diagonal of a square matrix.
#'     
#' @description Adds a constant or a vector to the diagonal of a numeric square matrix.
#' 
#' @param x A square matrix, either of type double or integer.
#' @param epsilon Values to be added to the main diagonal of \code{x}, either a single numeric
#' value or a numeric vector. Its type must match the type of \code{x}.
#' @param check Should checks of the input be performed? Not doing so is faster.
#' 
#' @details This function modifies the matrix \code{x} in place for getting maximum speed by
#' avoiding making copies. 
#' 
#' @return Noting, the matrix is modified in place. 
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' d <- diag(5L)
#' add_diag(d, 1)
#' diag(d)
#' 
#' # Speed comparison:
#' d <- diag(1000L)
#' epsilon <- 1e-6
#' microbenchmark::microbenchmark(times = 100L,
#' add_diag(d, epsilon),
#' add_diag(d, epsilon, check = FALSE),
#' diag(d) <- diag(d) + epsilon # canonical approach
#' )
#' 
#' @export
add_diag <- function(x, epsilon = sqrt(.Machine$double.eps), check = TRUE) {
  
  typeof_x <- typeof(x)
  length_epsilon <- length(epsilon)
  
  if (check) { 
    
    if (!is.matrix(x) || !is.numeric(x))
      stop("'x' must be a numeric matrix.")
    
    if (nrow(x) != ncol(x))
      stop("'x' must be square.")
    
    if (!is.vector(epsilon) || !is.numeric(epsilon) || !(length_epsilon %in% c(1L, nrow(x))))
      stop(paste0("'epsilon' must be a numeric vector either of length 1 or matching ",
                  "the dimension of 'x'."))
    
    if (typeof_x != typeof(epsilon))
      stop("'x' and 'epsilon' must have the same type.")
  }

  # Dispatch different methods at the R-level. At least for the distinction between double and
  # int, C++ templates could be employed.
  if (typeof_x == 'double') {
    if (length_epsilon == 1L) {
      add_diag_double(x, epsilon)
    } else {
      add_diag_double_vec(x, epsilon)
    }
  } else if (typeof_x == 'integer') {
    if (length_epsilon == 1L) {
      add_diag_int(x, epsilon)
    } else {
      add_diag_int_vec(x, epsilon)
    }
  }
}





