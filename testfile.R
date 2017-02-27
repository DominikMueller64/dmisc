bn <- microbenchmark::microbenchmark

a <- rnorm(50)
b <- sample(c(0, 1), size = 5000, replace = TRUE, prob = c(0.6, 0.4))
x <- a %o% b
p <- colMeans(x)
nonzero <- which(abs(p * (1 - p)) > 0)
non <- p * (1-p) >0
length(nonzero)

bn(times = 1e2,
   tcrossprod(x),
   tcrossprod(x[,nonzero]),
   dmisc:::.sparse_tcrossprod(x, x, nonzero)
)

u <- dmisc:::.sparse_tcrossprod(x, x, nonzero)
dim(u)

library(purrr)
library(Matrix)

bn <- microbenchmark::microbenchmark

set.seed(123)
rown <- rep(50, 5)
coln <- rep(50, 5)

## rown <- rep(2, 2)
## coln <- rep(2, 2)
x <- purrr::map(rown, function(n) purrr::map(coln, function(m) matrix(rnorm(n*m), nrow = n)))

u <- do.call(rbind, lapply(x, function(xx) do.call(cbind, xx)))
uu <- dmisc:::.combine_asym_arma(x, rows=rown, cols=coln)
sum((u -uu)^2)

bn(times = 100,
do.call(rbind, lapply(x, function(xx) do.call(cbind, xx))),
dmisc:::.combine_asym_arma(x, rows=rown, cols=coln)
)

x1 <- x
## x1[[2]][[1]] <- NULL; x1[[3]][[2]] <- NULL; x1[[3]][[1]] <- NULL
## x1[[2]][[1]] <- NULL
## x1[[2]][[1]] <- NULL; x1[[3]][[2]] <- NULL; x1[[3]][[1]] <- NULL


bn(times = 100,
do.call(rbind, lapply(x, function(xx) do.call(cbind, xx))),
 dmisc:::.combine_sym_arma(x, rows=rown, cols=coln)
)

do.call(rbind, lapply(x, function(xx) do.call(cbind, xx)))

dmisc:::.combine_sym_arma(x1, rows=rown, cols=coln)

dim(u)
image(Matrix(u))

x <- x1
x <- x2
x <- x3
symmetric <- F
symmetric <- T

bind_matlist <- function(x, symmetric = FALSE, byrow = TRUE, check = FALSE)
{
  n_vert <- length(x)
  n_horiz <- vapply(X = x, FUN = length, FUN.VALUE = integer(1L))

  if (check) {

    if (symmetric) {
      ## block-level tests
      t1 <- max(n_horiz) != n_vert
      t2 <- any(abs(diff(n_horiz)) != 1L)

      ## sublock-level tests
      ## TODO


      if (t1 || t2)
        stop("Input not conformable with a symmetric structure.")
    } else {


    }

    ## bn(times = 10,     {})
    n_col <- n_row <- matrix(data = NA_integer_, nrow = n_vert, ncol = max(n_horiz))
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        jp <- if (symmetric && n_horiz[1L] > 1L) j + i - 1L else j
        n_col[i, jp] <- ncol(x[[i]][[j]])
        n_row[i, jp] <- nrow(x[[i]][[j]])
      }
    }
    ## n_col
    ## n_row

    ## Test if all dimensions are conformable.
    tf <- function(x) length(unique(na.omit(x))) != 1L
    tc <- any(apply(X = n_col, MARGIN = 2L, FUN = tf))
    tr <- any(apply(X = n_row, MARGIN = 1L, FUN = tf))
    if (tc || tr)
      stop("Dimensions of submatrices are not conformable.")

    cols <- n_col[1L, ]
    rows <- n_row[, ncol(n_col)]

    combine_asym <- function(x, rows, cols) {
      n <- sum(rows)
      m <- sum(cols)
      mat <- matrix(data = 0.0, nrow = n, ncol = m)

      cib <- 0L
      for (ib in seq_along(rows)) {
        cjb <- 0L
        for (jb in seq_along(cols)) {
          xx <- x[[ib]][[jb]]
          for (i in seq_len(rows[ib])) {
            for (j in seq_len(cols[jb])) {
              im <- cib + i
              jm <- cjb + j
              mat[im, jm] <- xx[i, j]
            } # j
          }  # i
          cjb <- cjb + cols[jb]
        } # jb
        cib <- cib + rows[ib]
      } # ib

      mat
    }


  }

}

sum((mat - X)^2)

bn(times = 100,
do.call(rbind, lapply(x, function(xx) do.call(cbind, xx))),
do.call(rbind, purrr::map(x, .f = do.call, what = 'cbind')),
combine_asym(x, rows, cols)
)

dmisc:::.combine_asym(x, rows, cols)


