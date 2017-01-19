% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equal_split.R
\name{equal_split}
\alias{equal_split}
\title{Split a set x into subset of nearly equal size.}
\usage{
equal_split(x, n, random = TRUE, beginning = FALSE)
}
\arguments{
\item{x}{The set of value to be split into subsets, a \code{vector} or \code{list}.}

\item{n}{The desired number of subsets.}

\item{random}{Should the elements of the subsets be randomly sampled from \code{x}?}

\item{beginning}{Should elements in excess should be added to the subsets from the
beginning of the output or randomly assigned?}
}
\value{
A list containing the subsets. An attribte \code{'indices'} contained the 
indices of the entries that contain the excess elements.
}
\description{
Split a set x stored in a \code{vector} or \code{list} into a number of maximally equally
sized subsets.
}
\details{
Note that if the number of desired subsets \code{n} is larger than the supplied set
\code{x}, the output will still be a list with length \code{n} where \code{length(x)} entries
will contain exactly one element and \code{n - length(x)} entries will be empty.
}
\examples{
x <- seq.int(from = 1L, to = 10L)
n <- 3L
equal_split(x, n, random = TRUE, beginning = FALSE)
equal_split(x, n, random = TRUE, beginning = TRUE)
equal_split(x, n, random = FALSE, beginning = FALSE)
equal_split(x, n, random = FALSE, beginning = TRUE)

# lists as input work as well
equal_split(as.list(x), n, random = TRUE, beginning = FALSE)

A too large number of desired subsets is handeled gracefully.
equal_split(x, n = 20L, random = TRUE, beginning = FALSE)

}
\author{
Domink Mueller (\email{dominikmueller64@yahoo.de})
}
