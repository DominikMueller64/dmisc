% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mat2df.R
\name{mat2df}
\alias{mat2df}
\title{Transform matrix to data.frame.}
\usage{
mat2df(mat)
}
\arguments{
\item{mat}{The matrix to be transformed.}
}
\value{
A data.frame where one row corresponds to one matrix entry. If the matrix has row or
column names, these will be included.
}
\description{
Transform a matrix into a data.frame where each row corresponds to a single
matrix entry.
}
\examples{
mat <- matrix(1:12, nrow = 3)
mat2df(mat)
rownames(mat) <- 1:3 
mat2df(mat)
colnames(mat) <- letters[1:4]
mat2df(mat)
rownames(mat) <- NULL
mat2df(mat)

}
\author{
Dominik Mueller
}
