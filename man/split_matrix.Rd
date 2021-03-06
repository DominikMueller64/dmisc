% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_matrix.R
\name{split_matrix}
\alias{split_matrix}
\title{Split matrix}
\usage{
split_matrix(x, by = c("row", "col"))
}
\arguments{
\item{x}{A matrix to by split}

\item{Either}{\code{'row'} or \code{'col'} indicating the direction of 
the split. Defaults to \code{'row'}.}
}
\value{
A list containing the row or columns of the matrix.
}
\description{
Split a matrix by rows or columns into a list.
}
\details{
A shorthand not preserving rownames is \code{split(x, row(x))}
for splitting by rows and \code{split(x, col(x))} for splitting by
columns.
}
\examples{
x <- matrix(seq_len(4L), nrow = 2L) 
dimnames(x) <- list(c('a', 'b'), c('c', 'd'))
split_matrix(x, 'row')
split_matrix(x, 'col')



}
\author{
Dominik Mueller
}
