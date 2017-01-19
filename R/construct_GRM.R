#' Compute genomic relationship matrices.
#'     
#' @description Compute genomic relationship matrices according to different methods.
#' 
#' @param ... Genotype matrices of the differen populations or families, coded with
#' (0, 1, 2).
#' 
#' @param scaling The scaling procedure that should be applied to the columns, one of
#' \code{'VR1'} or \code{'VR2'}. With \code{'VR1'}, all columns are scaled by the same
#' constand computed from the allele frequencies and with \code{'VR2'}, columns are
#' scaled individually.
#' 
#' @param offblock The scope of the applied allele frequencies, one of \code{'canonical'},
#' \code{'melchinger'} or \code{'chen'}. If \code{'canonical'}, ovarall allele frequencies are
#' either computed or taken from argument \code{p} and used for centering and scaling in all
#' populations or families. If \code{'melchinger'} or \code{'chen'}, population-specific
#' allele frequencies are computed and used for centering and scaling. 
#' 
#' @param check Should checks of the input be performed? Not doing so will be faster.
#' 
#' @param p Allele frequencies, either a vector if \code{offblock} is \code{canonical} or
#' a list of vectors if \code{offblock} is  \code{'melchinger'} or \code{'chen'}.
#' 
#' @param weighted A logical value. Should the allele frequencies be weighted?
#' Only regarded if \code{offblock} is \code{canonical} and no allele frequencies (p)
#'  were given.
#' 
#' @details 
#' 
#' @author Dominik Mueller
#' 
#' @examples
#'  Xa <- structure(c(2, 0, 0, 2, 0, 2, 0, 2, 2, 2, 0, 0, 2, 0, 2, 0, 2,
#'                   2, 0, 2, 2, 0, 2, 2, 0, 2, 2, 0, 2, 2, 2, 2, 0, 0, 0, 0),
#'                  .Dim = c(4L, 9L),
#'                   .Dimnames = list(c("PopA_Ind_1", "PopA_Ind_2", "PopA_Ind_3",
#'                                     "PopA_Ind_4"),
#'                                   c("Locus_1", "Locus_2", "Locus_3", "Locus_4",
#'                                     "Locus_5", "Locus_6", "Locus_7", "Locus_8", "Locus_9")))
#' 
#'  Xb <- structure(c(2, 0, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 2, 0, 0,
#'                   2, 2, 2, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 0, 2, 0, 2, 0, 0),
#'                 .Dim = c(4L, 9L),
#'                 .Dimnames = list(c("PopB_Ind_1", "PopB_Ind_2", "PopB_Ind_3", "PopB_Ind_4"),
#'                                  c("Locus_1", "Locus_2", "Locus_3", "Locus_4",
#'                                    "Locus_5", "Locus_6", "Locus_7", "Locus_8", "Locus_9")))
#' 
#' # We assume that the there are bi-parental families from completely inbreed parents AND
#' # that all loci monomorphic in the progeny are also monomophic in the parents (which is by no
# means a necessity for such small populations.)  
#'  
#' hf <- function(x) {
#'  # Little helper for getting the expected frequencies according to our assumptions.
#'  x[x > 0.0 & x < 1.0] <- 0.5
#'  x
#' } 
#' p_spec <- list(hf(colMeans(Xa) / 2.0), hf(colMeans(Xb) / 2.0))
#' p <- Reduce(`+`, p_spec) / 2.0 # Weighting or not makes no difference here, as the population size are equal.
#'  
#' 
#' dots <- list(Xa, Xb)
#' scaling_levels <- c('VR1', 'VR2')
#' offblock_levels <- c('canonical', 'melchinger', 'chen')
#' 
#' library('tibble')
#' library('purrr')
#' library('dplyr')
#' library('forcats')
#' library('ggplot2')
#' 
#' d <- cross_d(list(scaling = scaling_levels, offblock = offblock_levels))
#' 
#' dat <- map2_df(d[[1L]], d[[2L]], function(scaling, offblock) {
#'   
#'   if (offblock %in% c('melchinger', 'chen')) {
#'     tmp <- p_spec
#'   } else
#'     tmp <- p
#'   # tmp <- NULL
#'   G <- purrr::invoke(compute_GRM, dots, scaling = scaling, offblock = offblock,
#'                      p = tmp, weighted = TRUE)
#'   if (!is.null(G))
#'     mat2df(G) %>% as_tibble() %>% mutate(scaling = scaling, offblock = offblock)
#' })
#' dat$row_names <- forcats::fct_rev(dat$row_names)
#' 
#' ggplot(data = dat,
#'        mapping = aes(x = col_names, y = row_names)) +
#'   geom_tile(aes(fill = value), colour = "white") +
#'   facet_grid(facets = scaling ~ offblock) +
#'   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#'                        midpoint = 0, limit = c(min(dat$value), max(dat$value)), space = "Lab", 
#'                        name = "Coefficient") +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#'  geom_text(aes(col_names, row_names, label = signif(value, 3L)), color = "darkgreen", size = 4)
#'
#' @export
compute_GRM <- function(..., scaling = c('VR1', 'VR2'),
                        offblock = c('canonical', 'chen', 'melchinger'),
                        p = NULL, weighted = NULL,
                        check = TRUE) {
 
  dots <- list(...)
  scaling <- match.arg(arg = scaling)
  offblock <- match.arg(arg = offblock)
  
  DELTA <- sqrt(.Machine$double.eps) # Constant for numerical comparisons.
  
  matlist_combine <- function(ll) {
    purrr::invoke(rbind, purrr::map(ll, ~ purrr::invoke(cbind, .x)))
  }
  
  is_poly <- function(x, epsilon = sqrt(.Machine$double.eps)) {
    x * (1.0 - x) > epsilon
  }
  
  list_of_lists <- function(n, m = n) { 
    replicate(n = n,
              expr = vector(mode = 'list', length = m),
              simplify = FALSE)
  }
  
  tcrossprod_list <- function(dots) {
    ll <- list_of_lists(n = length(dots))
    # off-diagonals
    for (i in seq.int(1L, length(dots) - 1L)) {
      for (j in seq.int(i + 1L, length(dots))) {
        tcp <- tcrossprod(dots[[i]], dots[[j]])
        ll[[i]][[j]] <- tcp 
        ll[[j]][[i]] <- t(tcp)
      }
    }
    # diagonals
    for (i in seq_along(dots)) {
      ll[[i]][[i]] <- tcrossprod(dots[[i]])
    }
    matlist_combine(ll)
  }
  
  n_col <- unique(purrr::map_int(dots, ncol))
  if (length(ncol) > 1L)
    stop("All matrices must have the same number of columns.")
  
  if (check) {

    elem <- unique(unlist(purrr::map(dots, purrr::compose(unique, as.integer))))
    if (!dmisc::is_subset(elem, c(0L, 1L, 2L)))
      stop("Use correct coding of genotypes! Only 0, 1 and 2 is allowed.")
    
    if (offblock %in% c('melchinger', 'chen') && !is.null(weighted))
      warning("Argument 'weighted' is ignored for 'offblock' being 'melchinger' or 'chen'.")
    
    if (offblock == 'canonical' && is.null(p) && is.null(weighted))
      stop("If 'offblock == canonical' and 'p' is not given, 'weighted' must be specified.")
    
    if (!is.null(p)) {
      
      if (offblock == 'canonical') {
        
        if (!is.null(weighted))
          warning("Argument 'weighted' is ignored for 'offblock == canonical' and given 'p'.")
        
        if (!is.atomic(p))
          stop("If 'offblock == canonical', 'p' must be an atomic vector.")
        
        if (any(p < 0.0 || p > 1.0))
          stop("All elements of 'p' must be between 0 and 1.")
        
        if (length(p) != n_col)
          stop("The length of 'p' is not conformable with the genotypic data.")
        
      }
      
      if (offblock %in% c('melchinger', 'chen')) {
        
        if (!is.list(p))
          stop("If 'offblock' is 'mechinger' or 'chen', 'p' must be a list.")
        
        if (length(p) != length(dots))
          stop("The list 'p' must have the same length as the number of families.")
        
        for (p_ in p) {
          if (any(p_ < 0.0 || p_ > 1.0))
            stop("At lead one lower level element of 'p' is not between 0 and 1.")
          
          if (length(p_) != n_col)
            stop("The length of a vector in 'p' is not conformable with the genotypic data.")
        }
      }
    }
  }


  n_row <- sum(purrr::map_int(dots, nrow))
  dim_ratio <- n_col / n_row
  
  # Compute allele frequencies if necessary.
  if (offblock == 'canonical') {
    # if (!is.null(p))
    #   warning("Provided allele frequencies are ignored for offblock == 'canonical'.")
    if (is.null(p)) {
      if (weighted)
        p <- purrr::reduce(purrr::map(dots, ~ colMeans(.x) / 2.0 * nrow(.x)), `+`) / n_row 
      else 
        p <- purrr::reduce(purrr::map(dots, ~ colMeans(.x) / 2.0), `+`) / length(dots) 
    }
    # If p is provided, it is the job of the user to make sure its properly computed!
  } else if (offblock %in% c('chen', 'melchinger')) {
    if (is.null(p))
      p <- purrr::map(dots, ~ colMeans(.x) / 2.0) 
      # There is no weighting here, as frequnecies are population-specific.
  }
  
  
  if (scaling == 'VR1') {
    # In this case, there is no need to cater for monomorphic loci.
    if (offblock %in% c('canonical', 'melchinger')) {
      if (offblock == 'canonical') {
        # Center and scale each matrix by the overall current allele frequency.
        dots <- purrr::map(dots, ~ col_scale(.x, center = 2.0 * p,
                                              scale = rep(sqrt(2.0 * sum(p * (1.0 - p))),
                                                          times = length(p)),
                                              inplace = FALSE))
      } else if (offblock == 'melchinger') {
        # Center and scale each matrix by population-specific current allele frequencies.
        dots <- purrr::map2(dots, p, ~ col_scale(.x, center = 2.0 * .y,
                                                  scale =  rep(sqrt(2.0 * sum(.y * (1.0 - .y))),
                                                               times = length(.y)),
                                                  inplace = FALSE))
      } 
      
      if (dim_ratio > 1.0) {
        G <- tcrossprod_list(dots)
      } else {
        G <- tcrossprod(purrr::invoke(rbind, dots))
      }
      
    } else if (offblock == 'chen') {
      # Center only by population-specific current allele frequencies, as the later
      # denominator of the off-diagonal blocks of the genomic relatinship matrix cannot
      # be factorized with this approach.
      dots <- purrr::map2(dots, p, ~ col_scale(.x, center = 2.0 * .y,
                                               scale =  rep(1.0, times = length(.y)),
                                               inplace = FALSE))
      ll <- list_of_lists(n = length(dots))
      pprod <- purrr::map(p, ~ .x * (1.0 - .x))
      # off-diagonals
      for (i in seq.int(1L, length(dots) - 1L)) {
        for (j in seq.int(i + 1L, length(dots))) {
          denom <- 2.0 * sum(sqrt(pprod[[i]] * pprod[[j]])) # cannot be factorized!
          tcp <- tcrossprod(dots[[i]], dots[[j]]) / denom
          ll[[i]][[j]] <- tcp
          ll[[j]][[i]] <- t(tcp)
        }
      }
      # diagonals
      for (i in seq_along(dots)) {
        ll[[i]][[i]] <- tcrossprod(dots[[i]]) / (2.0 * sum(pprod[[i]]))
      }
      G <- matlist_combine(ll)
    }
  } else if (scaling == 'VR2') {
    # In this case, it is necessary to diligently cater for monomorphyic loci!
    if (offblock == 'canonical') {
      # Center and scale by the overall allele frequency. Columns of zeros at monomorphic
      # loci after centering are retained and scaled by a nonzero value (here: 1.0) to
      # circumvent division by 0.
      poly <- is_poly(p)
      scale <- sqrt(2.0 * sum(poly) * p * (1.0 - p))
      scale[!poly] <- 1.0
      dots <- purrr::map(dots, ~ col_scale(.x, center = 2.0 * p,
                                           scale =  scale,
                                           inplace = FALSE))
      if (dim_ratio > 1.0) {
        G <- tcrossprod_list(dots)
      } else {
        G <- tcrossprod(purrr::invoke(rbind, dots))
      }

    } else if (offblock == 'melchinger') {
      # Center and scale each matrix by population-specific current allele frequencies. 
      # The division factors for the off-diagonal blocks have to be determined for each
      # block seperately, as they depend on the magnitude of the intersection of polymorpic
      # loci. 
      dots <- purrr::map2(dots, p, function(.x, .y) {  
        poly <- is_poly(.y)
        scale <- sqrt(2.0 * .y * (1.0 - .y))
        scale[!poly] <- 1.0
        col_scale(.x, center = 2.0 * .y,
                  scale =  scale,
                  inplace = FALSE)
      })
      ll <- list_of_lists(n = length(dots))
      pprod <- purrr::map(p, ~ .x * (1.0 - .x))
      # off-diagonals
      for (i in seq.int(1L, length(dots) - 1L)) {
        for (j in seq.int(i + 1L, length(dots))) {
          denom <- sum((pprod[[i]] > DELTA) & (pprod[[j]] > DELTA))
          tcp <- tcrossprod(dots[[i]], dots[[j]]) / denom
          ll[[i]][[j]] <- tcp
          ll[[j]][[i]] <- t(tcp)
        }
      }
      # diagonals
      for (i in seq_along(dots)) {
        ll[[i]][[i]] <- tcrossprod(dots[[i]]) / sum(pprod[[i]] > DELTA)
      }
      # combine
      G <- matlist_combine(ll)
    } else if (offblock == 'chen') {
      warning("Not yet implemented")
      return(NULL)
    }
  }
  G
}