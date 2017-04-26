#' @title Solve selection index equations for animal model
#'
#' @description This is a function for getting BLUPs of genomic/breeding values from the
#'              animal model using the selection index approach.
#'
#' @param y double vector. Phenotypic values (only for training individuals).
#' @param A double matrix. A semi-positive-definite matrix (pedigree- or marker-based).
#' @param lambda double. The ratio of environmental to genetic variance component.
#' @param ps_only boolean. Should predictions only be give for the
#'                prediction set?
#' @param check boolean. Should checks on the input be performed?
#'
#' @return A list with two components. The first components hold the BLUE of the general mean,
#' the second component the BLUPs of the genomic or breeding values.
#'
#' @details This function has two requirements on the sorting of \code{y} and \code{A}:
#' (i) any individuals to be predicted (having no phenotype) comes before any
#' training individual with p henotype and (ii) the order of training
#' individuals in \code{y} is the same as in \code{A}.
#'
#' @author Dominik Mueller
#' @export
solve_sie <- function(y, A, lambda, ps_only = TRUE, check = TRUE) {

  if (check) {
    if (!is.atomic(y) || !is.double(y))
      stop("'y' must be a double vector.")

    if (!is.matrix(A) || !is.double(A) || nrow(A) != ncol(A))
      stop("'A' must be a square double matrix.")

    if (length(y) > nrow(A))
      stop("Length of 'y' must not be larger than the dimensions of 'A'")
  }

  n <- nrow(A)
  n_ts <- length(y)
  n_ps <- n - n_ts

  ## Compute V
  ts_ix <- (n_ps + 1L):n
  Ajj <- A[ts_ix, ts_ix]
  diag(Ajj) <- diag(Ajj) + lambda
  V_inv <- chol2inv(chol(Ajj))

  ## Compute beta_hat
  cs <- colSums(V_inv)
  beta_hat <- drop((cs %*% y) / sum(cs))

  ## Compute u
  if (ps_only) {
    u_hat <- A[1L:n_ps, ts_ix] %*% (V_inv %*% (y - beta_hat))
  } else {
    u_hat <- A[, ts_ix] %*% (V_inv %*% (y - beta_hat))
  }
  list('beta_hat' = beta_hat, 'u_hat' = drop(u_hat))
}

