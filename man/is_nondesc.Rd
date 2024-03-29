% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_nondesc}
\alias{is_nondesc}
\title{Check whether all elements given are sorted in non-descending order}
\usage{
is_nondesc(...)
}
\arguments{
\item{...}{anything!}
}
\value{
\code{TRUE} if the elements are sorted in non-descending order, otherwise \code{FALSE}
}
\description{
Check whether all elements given are sorted in non-descending order
}
\examples{

is_nondesc(1, 2, 3)
is_nondesc(c(1, 2), 3)
is_nondesc(6, 4, 1)
is_nondesc("a", "b", "c")
is_nondesc(c("z", "y"))
}
\author{
Kelly Sovacool \email{sovacool@umich.edu}
}
