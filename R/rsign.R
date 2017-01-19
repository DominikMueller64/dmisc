#' Sample random signe
#'     
#' @description Sample a random sign according to a specified probability.
#' 
#' @param n number of samples to draw
#' @param p probability that a positive sign is drawn.
#' 
#' @return A numeric vector with elements \code{1} and \code{-1}.
#' 
#' @author Dominik Mueller
#' 
#' @examples
#' rsign(10L)
#' rsign(10L, p = 0.95)
#' 
#' @export
rsign <- function(n, p = 0.5) {
  
  if (p < 0.0 || 1.0 < p)
    stop("'p' must be a double between 0 and 1.")
  
  sample(x = c(-1.0, 1.0), size = n, replace = TRUE, prob = c(1.0 - p, p))
}
  
