% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icount.R
\name{icount}
\alias{icount}
\title{A count iterator.}
\usage{
icount(start = 0L, count = NULL, step = 1L)
}
\arguments{
\item{start}{An integer. Start of the count iterator (default = 0L)}

\item{count}{An integer. Stop of the count iterator (default = NULL).
Never ceases counting by default.}

\item{step}{An integer. Step size of the count iterator (default = 1L).}
}
\value{
A count iterator.
}
\description{
Constructs a count iterator with start, (maximum) count and stepsize.
}
\details{
The use of negative integers is also possible. If \code{step} is
negative, this will generate a decreasing sequence. If \code{step} is zero,
it will generate a constant, neverending sequence, independent of
\code{count}.
}
\examples{
ct <- icount(start = 3L, count = 10L, step = 3L)
iterators::nextElem(ct)
iterators::nextElem(ct)
iterators::nextElem(ct)
iterators::nextElem(ct)

}
\seealso{
\code{\link[iterators]{icount}} and
\code{\link[itertools2]{icount}}.
}
\author{
Domink Mueller (\email{dominikmueller64@yahoo.de})
}
