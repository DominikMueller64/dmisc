#' Crease nested lists.
#' 
#' @description Create deeply nested list and initialize the values at
#' the lowest level.
#' 
#' @param len A integer vector indicating the number of lists a different
#' hierarchical levels of the nested list. For instance, \code{len = c(2, 3)}
#' produces a nested list with 2 lists at the first level and 3 at the 
#' second level, i.e., 2 * 3 entries in total.
#' 
#' @param fill Value to fill in at the lowest hierarchical level.
#' 
#' @return A nested list initialized with \code{fill}.
#' 
#' @author Dominik Mueller (\email{dominikmueller64@yahoo.de})
#' 
#' @examples 
#' nested_list(len = c(2, 4, 3), fill = NA)
#' 
#' @export
nested_list <- function(len, fill = NA){
  if (length(len) == 0L)
    return(fill)
  purrr::map(seq.int(len[1L]), ~ nested_list(len[-1L],  fill))
}


# nested_list <- function(lev, fill = NA) {
#   if (length(lev) > 0) {
#     n <- lev[1L]
#     l <- vector(mode = 'list', length = n)
#     for (i in seq_along(l)) {
#       l[[i]] <- nested_list(lev = lev[-1L], fill = fill)
#     }
#     return(l)
#   }
#   fill
# }