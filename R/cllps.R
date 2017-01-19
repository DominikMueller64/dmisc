#' Collapse elements of a vector
#'     
#' @description \code{cllps} collapses the elements of a vector
#' 
#' @param x A vector to be collapsed.
#' @param sep Seperator character used for collapsing.
#' 
#' @details This function is defined as
#' \code{cllps <- function(x, sep = '') paste(x, collapse = sep)}
#' 
#' @return A character vector of length one containing the 
#' collapsed input.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' cllps(c(letters), '_')
#' 
#' @export
cllps <- function(x, sep = '') paste(x, collapse = sep)
