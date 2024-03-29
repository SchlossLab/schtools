% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate.R
\name{calc_relabun}
\alias{calc_relabun}
\title{Calculate OTU relative abundances from a shared file}
\usage{
calc_relabun(abs_abun_dat)
}
\arguments{
\item{abs_abun_dat}{a data frame from reading in a \href{https://mothur.org/wiki/shared_file/}{shared file}.
Should contain a \code{Group} column for sample names,
\code{Otu} columns for absolute counts of each OTU,
and rows as each sample.}
}
\value{
a new data frame with OTU relative abundances in long format.
}
\description{
Calculate OTU relative abundances from a shared file
}
\examples{
shared_dat <- readr::read_tsv(system.file("extdata", "test.shared",
  package = "schtools"
))
shared_dat \%>\% calc_relabun()
}
\author{
Kelly Sovacool \email{sovacool@umich.edu}
}
