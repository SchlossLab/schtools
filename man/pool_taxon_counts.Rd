% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared.R
\name{pool_taxon_counts}
\alias{pool_taxon_counts}
\title{Pool OTU counts at a particular taxonomic level}
\usage{
pool_taxon_counts(otu_shared_dat, otu_tax_dat, taxon_level)
}
\arguments{
\item{otu_shared_dat}{data frame created from a \href{https://mothur.org/wiki/shared_file/}{shared file}
at the OTU level.}

\item{otu_tax_dat}{data frame created from a \href{https://mothur.org/wiki/taxonomy_file/}{taxonomy file}
at the OTU level. Must be from the same dataset as the shared file.}

\item{taxon_level}{taxonomic level to pool OTUs into.
Options: "kingdom", "phylum", "class", "order", "family", "genus".
This should be the name of a column in \code{otu_tax_dat} as a character string.}
}
\value{
a shared data frame with the OTUs at the specified \code{taxon_level} and
a corresponding taxonomy dataframe with new OTU numbers.
}
\description{
Enables comparing analyses at different taxonomic resolutions,
as seen in \doi{10.1128/mbio.03161-21}.
Implementation adapted from \href{https://github.com/SchlossLab/Armour_Resolution_mBio_2021/blob/master/code/get_phylotype_shared.R}{here}.
}
\examples{
tax_dat <- read_tax(system.file("extdata", "test.taxonomy",
  package = "schtools"
))
shared_dat <- readr::read_tsv(system.file("extdata", "test.shared",
  package = "schtools"
))
pool_taxon_counts(shared_dat, tax_dat, "genus")
pool_taxon_counts(shared_dat, tax_dat, "family")
pool_taxon_counts(shared_dat, tax_dat, "phylum")
}
\author{
Kelly Sovacool, \email{sovacool@umich.edu}

Pat Schloss \email{pschloss@umich.edu}
}
