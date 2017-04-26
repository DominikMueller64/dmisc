cranlogs::cran_downloads(packages = 'hypred', when = 'last-month')
library('dmisc')
dmisc::routine(c('1','2', '0', '8'))

n_row <- 10L
n_col <- 1000L

dat <- as.data.frame(replicate(n = n_col,
                               expr = sample(x = c(0, 1, 2), size = n_row, replace = TRUE)))

microbenchmark::microbenchmark(times = 10,
     apply(X = dat, MARGIN = 2L, FUN = function(x) paste0(x, collapse = "")),
     string_collapse(dat)
)

dat <- data.frame(V1=c(1, 0, 2),
                  V2=c(1, 1, 0),
                  V3=c(1, 0, 1),
                  V4=c(0, 1, 2),
                  V5=c(1, 1, 1))
string_collapse(dat)

dmisc::routine(as.character(dat$V1))

matrix(apply(dat, 2, paste0, collapse = ""), ncol = 1)


n <- 200L
pos <- sort(runif(n, min = 0, max = 200))
haldane <- function(x) 0.5 * (1 - exp(-2 * x))
r1 <- outer(X = pos, Y = pos, FUN = function(x, y) haldane(abs(x - y) / 100))
r2 <- prob_recomb(pos)
sum((r1-r2)^2)


microbenchmark::microbenchmark(times = 100,
outer(X = pos, Y = pos, FUN = function(x, y) haldane(abs(x - y) / 100)),
prob_recomb(pos)
)





dat <- mtcars[, c(1, 3)]
N <- nrow(dat)
R <- 2500

cor.orig <- cor(dat)[1,2]
cor.boot <- NULL

for (i in 1:R) {
  idx <- sample.int(N, N, replace = TRUE) 
  cor.boot[i] <- cor(dat[idx, ])[1,2] 
}

library(dmisc)
library(BGLR)
data(wheat)
set.seed(123L)
n <- 500L
n_ts <- 499L
A <- wheat.A[1:n,1:n]
y <- wheat.Y[,1][1:n][(n - n_ts + 1L):n]
lambda <- 0.124

## dmisc:::.solve_sie(y,A,lambda)

solve_mme_R <- function(y, A, lambda) {
  Ainv <- chol2inv(chol(A))
  n <- nrow(A)
  n_ts <- length(y)
  n_ps <- n - n_ts
  xtz <- rep(x = c(0, 1), times = c(n_ps, n_ts))
  xty <- sum(y)
  zty <- c(rep(x = 0, times = n_ps), y)
  Ainv <- lambda * Ainv
  diag(Ainv) <- diag(Ainv) + xtz
  lhs_ <- rbind(c(n_ts, xtz), cbind(matrix(xtz, ncol = 1L), Ainv))
  rhs_ <- c(xty, zty)
  sol <- solve(lhs_, rhs_)
  list('beta_hat' = sol[1L], 'u_hat' = sol[-1L])
}

solve_mme_R_org <- function(y, A, lambda) {
  Ainv <- chol2inv(chol(A))
  n <- nrow(A)
  n_ts <- length(y)
  n_ps <- n - n_ts
  x <- rep(1, n_ts)
  xtx <- crossprod(x)
  z <- diag(n)[-seq_len(n_ps),]
  ztz <- crossprod(z)
  ztx <- crossprod(z, x)
  xtz <- t(ztx)
  lhs <- cbind(rbind(xtx, ztx), rbind(xtz, ztz + lambda * Ainv))
  rhs <- rbind(crossprod(x, y), crossprod(z, y))
  sol <- solve(lhs, rhs)
  list('beta_hat' = sol[1L], 'u_hat' = sol[-1L])
}

solve_sie_org <- function(y, A, lambda) {

  sigma_e2 <- 4234.34234 ## arbitrary
  sigma_a2 <- sigma_e2 / lambda

  n <- nrow(A)
  n_ts <- length(y)
  n_ps <- n - n_ts

  ## Compute V
  Z <- diag(n)[-seq_len(n_ps),]
  V <- sigma_a2 * Z%*%A%*%t(Z) + sigma_e2 * diag(n_ts)
  V_inv <- chol2inv(chol(V))

  ## Compute beta_hat
  X <- matrix(1, nrow = n_ts)
  beta_hat <- solve(t(X) %*% V_inv %*% X) * t(X) %*% V_inv %*% y

  ## Compute u
  list('beta_hat' = beta_hat, 'u_hat' = sigma_a2 * A %*% t(Z) %*% V_inv %*% (y - beta_hat))
}

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
  beta_hat <- (cs %*% y) / sum(cs)

  ## Compute u
  if (ps_only) {
    u_hat <- A[1L:n_ps, ts_ix] %*% (V_inv %*% (y - beta_hat))
  } else {
    u_hat <- A[, ts_ix] %*% (V_inv %*% (y - beta_hat))
  }
  list('beta_hat' = beta_hat, 'u_hat' = u_hat)
}


cor(cbind(solve_mme_R(y,A,lambda)$u_hat,
          solve_mme_R_org(y,A,lambda)$u_hat,
          solve_sie_org(y,A,lambda)$u_hat,
          solve_sie_R(y,A,lambda)$u_hat))

## microbenchmark::microbenchmark(times = 100,
##                                solve(A),
##                                chol2inv(chol(A)),
##                                inv_sympd(A)
##                                )

solve_mme <- function(y, A, lambda, check = TRUE) {
  if (check) {
    if (!is.atomic(y) || !is.double(y))
      stop("'y' must be a double vector.")

    if (!is.matrix(A) || !is.double(A) || nrow(A) != ncol(A))
      stop("'A' must be a square double matrix.")

    if (length(y) > nrow(A))
      stop("Length of 'y' must not be larger than the dimensions of 'A'")
  }
  .Call('dmisc_solve_mme', PACKAGE = 'dmisc', y, A, lambda)
}




solve_sie <- function(y, A, lambda, check = TRUE) {
  if (check) {
    if (!is.atomic(y) || !is.double(y))
      stop("'y' must be a double vector.")

    if (!is.matrix(A) || !is.double(A) || nrow(A) != ncol(A))
      stop("'A' must be a square double matrix.")

    if (length(y) > nrow(A))
      stop("Length of 'y' must not be larger than the dimensions of 'A'")
  }
  .Call('dmisc_solve_sie', PACKAGE = 'dmisc', y, A, lambda)
}


## dmisc:::.solve_sie(y,A,lambda)
## solve_sie_org(y,A,lambda)[1]

cor(cbind(solve_sie_org(y,A,lambda)[-1],
          solve_sie(y,A,lambda)$u_hat,
          .solve_sie(y,A,lambda)$u_hat,
          solve_sie(y,A,lambda)$u_hat,
          solve_mme_R_org(y,A,lambda)[-1]))

microbenchmark::microbenchmark(times = 100,
                               .solve_mme(y, A, lambda),
                               solve_sie_org(y,A,lambda),
                               solve_sie(y,A,lambda),
                               .solve_sie(y,A,lambda)

)
str(solve_sie(y,A,lambda))

u <- solve_sie_org(y,A,lambda)
u2 <- solve_mme_R_org(y,A,lambda)
u3 <- solve_sie(y,A,lambda)
head(u)
head(u2)
head(u3)
cor(u2,u)


microbenchmark::microbenchmark(times = 100,
                               solve_mme_R_org(y, A, lambda),
                               solve_mme_R(y, A, lambda),
                               dmisc:::.solve_mme(y, A, lambda),
                               f(y, A, lambda, check = FALSE),
)

?solve

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


