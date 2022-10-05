
#' Pool OTU counts at a particular taxonomic level
#'
#' Enables comparing analyses at different taxonomic resolutions,
#' as seen in \doi{10.1128/mbio.03161-21}.
#' Implementation adapted from [here](https://github.com/SchlossLab/Armour_Resolution_mBio_2021/blob/master/code/get_phylotype_shared.R).
#'
#' @param otu_shared_dat data frame created from a [shared file](https://mothur.org/wiki/shared_file/)
#'   at the OTU level.
#' @param otu_tax_dat data frame created from a [taxonomy file](https://mothur.org/wiki/taxonomy_file/)
#'  at the OTU level. Must be from the same dataset as the shared file.
#' @param taxon_level taxonomic level to pool OTUs into (kingdom, phylum,
#'  class, order, family, genus). This should be a column in `otu_tax_dat`.
#'
#' @return a shared data frame with the OTUs at the specified `taxon_level` and
#'   a corresponding taxonomy dataframe with new OTU numbers.
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Pat Schloss \email{pschloss@@umich.edu}
#'
#' @examples
#' tax_dat <- read_tax(system.file("extdata", "test.taxonomy",
#'   package = "schtools"
#' ))
#' shared_dat <- readr::read_tsv(system.file("extdata", "test.shared",
#'   package = "schtools"
#' ))
#' pool_taxon_counts(shared_dat, tax_dat, genus)
#' pool_taxon_counts(shared_dat, tax_dat, family)
#' pool_taxon_counts(shared_dat, tax_dat, phylum)
pool_taxon_counts <- function(otu_shared_dat, otu_tax_dat, taxon_level) {
  countsum <- Group <- label <- numOtus <- otu <- otu_counts <- NULL
  shared_long <- otu_shared_dat %>%
    tidyr::pivot_longer(tidyr::starts_with("Otu"),
      names_to = "otu",
      values_to = "otu_counts"
    )

  new_otu_nums <- otu_tax_dat %>%
    dplyr::select({{ taxon_level }}) %>%
    unique() %>%
    dplyr::mutate(otu = paste0(
      "Otu",
      stringr::str_pad(1:nrow(.),
        width = nchar(nrow(.)),
        pad = "0"
      )
    ))

  pooled_shared <- dplyr::inner_join(shared_long,
    otu_tax_dat %>% dplyr::select(otu, {{ taxon_level }}),
    by = "otu"
  ) %>%
    dplyr::group_by({{ taxon_level }}, Group) %>%
    dplyr::summarize(countsum = sum(otu_counts)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(new_otu_nums, by = rlang::as_name(rlang::enquo(taxon_level))) %>%
    dplyr::select(-{{ taxon_level }}) %>%
    tidyr::pivot_wider(names_from = otu, values_from = countsum) %>%
    dplyr::mutate(
      numOtus = ncol(.) - 1,
      label = rlang::as_name(rlang::enquo(taxon_level))
    ) %>%
    dplyr::select(order(colnames(.))) %>%
    dplyr::select(label, Group, numOtus, tidyr::starts_with("Otu"))

  new_tax <- pooled_shared %>%
    dplyr::select(-c(label, numOtus)) %>%
    tidyr::pivot_longer(tidyr::starts_with("Otu"),
      names_to = "otu",
      values_to = "otu_counts"
    ) %>%
    dplyr::group_by(otu) %>%
    dplyr::summarize(countsum = sum(otu_counts)) %>%
    dplyr::inner_join(new_otu_nums, by = "otu") %>%
    dplyr::rename("size" = "countsum")

  return(list(
    shared = pooled_shared,
    tax = new_tax
  ))
}
