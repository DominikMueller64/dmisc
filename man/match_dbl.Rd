% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_dbl.R
\name{match_dbl}
\alias{match_dbl}
\title{A version of \code{match} working exclusively on numeric data.}
\usage{
match_dbl(x, table, nomatch = NA_integer_,
  tolerance = sqrt(.Machine$double.eps))
}
\arguments{
\item{x}{A numeric vector with the values to be matched.}

\item{table}{A numeric vector with the values to be matched against.}

\item{nomatch}{An integer as the value to be returned in the
case when no match is found.}

\item{numeric}{scalar >= 0. Differences smaller than `tolerance`
are not recognized. The default value is close to 1.5e-8.}
}
\value{
An integer vector giving the position in table of the first match
if there is a match, otherwise nomatch.
}
\description{
\code{match_dbl} returns a vector of the positions of (first) matches
of its first argument in its second.
}
\examples{
# generate some random numeric data
set.seed(123)
table <- runif(1000L)
table <- sample(c(table, table)) # 'table' now contains duplicates
x <- sample(table, 100L)

m1 <- match(x, table)
m1_dbl <- match_dbl(x, table)
identical(m1, m1_dbl) # TRUE according to expectation

microbenchmark::microbenchmark(match(x, table),
                               match_dbl(x, table)) # speed is fine

# minimally disturb x
x <- x + runif(n = length(x), min = -1e-10, max = 1e-10)

identical(m1, match(x, table)) # now FALSE 
identical(m1_dbl, match_dbl(x, table)) # still TRUE
identical(m1_dbl, match_dbl(x, table, tolerance = 1e-11)) # also FALSE now



}
\seealso{
\code{\link{\%in_dbl\%}} for an numerical analog to
\code{\link[base]{\%in\%}}
}
\author{
Dominik Mueller
}
