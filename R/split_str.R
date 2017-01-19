#' Split elements of a string.
#'     
#' @description Split the elements of a string \code{x} into substrings accroding
#' to (fixed, no regex) matches of \code{split}.
#' 
#' @inheritParams base::strsplit
#' 
#' @param split String to split \code{x}.
#' 
#' @details This function is equivalent to \code{strsplit(x, split, fixed = TRUE)[[1L]]}.
#' 
#' @return A list of the same length as \code{x}, the i-th element of which contains the vector 
#' of splits of \code{x[i]}.
#' 
#' @author Dominik Mueller
#' 
#' @seealso \code{\link[base]{strsplit}}
#' 
#' @export
#' 
#' @examples
#' x <- '2_3_4'
#' split_str(x, '_')
split_str <- function(x, split) {
  if (!is.character(x) || length(x) != 1)
    stop("'x' must be a character vector of length 1.")
  
  if (!is.character(split) || length(split) != 1)
    stop("'split' must be a character vector of length 1.")
  
  strsplit(x, split, fixed = TRUE)[[1L]]
}
