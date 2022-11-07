#' Convert taxonomy strings into dataframe of labels based on taxonomic classification
#'
#' @param dat dataframe from mothur taxonomy file with columns `OTU`, `Size`, and `Taxonomy`
#'
#' @return a wide dataframe with taxonomic labels
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' taxonomy_filepath <- system.file("extdata",
#'   "test.taxonomy",
#'   package = "schtools"
#' )
#' taxonomy_tbl <- read_tax(taxonomy_filepath)
#' head(taxonomy_tbl)
parse_tax <- function(dat) {
  levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
  dat <- dat %>%
    dplyr::mutate(Taxonomy = gsub("_", " ", all_of("Taxonomy"))) %>%
    tidyr::separate(all_of("Taxonomy"), levels, sep = "\\(\\d{2,3}\\);", extra = "drop") %>%
    dplyr::select(-all_of("Size"))
  # in older version of mothur unclassified are listed as unclassified
  # without information from higher level classification
  # for those cases, append with lowest identified classification
  if (any(dat$Genus == "unclassified")) {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = -all_of("OTU"),
        names_to = "Level",
        values_to = "Classification"
      ) %>%
      # order classification level
      dplyr::mutate(Level = factor(all_of("Level"), levels)) %>%
      dplyr::left_join(dplyr::group_by(., all_of("OTU")) %>%
        dplyr::filter(all_of("Classification") != "unclassified") %>%
        # select lowest level classification
        dplyr::filter(all_of("Level") == levels[max(as.numeric(all_of("Level")))]) %>%
        dplyr::select(all_of("OTU"), Lowest_classified = all_of("Classification")),
      by = "OTU"
      ) %>%
      dplyr::mutate(Classification = ifelse(all_of("Classification") == "unclassified",
        # append unclassified with lowest classification
        paste(all_of("Lowest_classified"), all_of("Classification"), sep = " "),
        all_of("Classification")
      )) %>%
      dplyr::select(-all_of("Lowest_classified")) %>%
      tidyr::pivot_wider(
        names_from = "Level",
        values_from = "Classification"
      )
  }
  # create label options for OTU and lowest taxonomic classification with the OTU
  dat <- dat %>%
    dplyr::mutate(
      tax_otu_label = paste0(all_of("Genus"), " (", gsub("tu0*", "TU ", all_of("OTU")), ")"),
      tax_otu_label = gsub(" unclassified", "", all_of("tax_otu_label")),
      otu_label = paste0(gsub("tu0*", "TU ", all_of("OTU"))),
      label_html = stringr::str_replace(
        all_of("tax_otu_label"),
        "([a-zA-Z]+) (.*)",
        glue::glue("<i>\\1</i> \\2")
      )
    )
  colnames(dat) <- tolower(colnames(dat))
  dat <- dat %>%
    dplyr::relocate(c("otu_label", "tax_otu_label", "label_html"),
      .after = "otu"
    )
  return(dat)
}

#' Read in a taxonomy file and parse it to a wide dataframe
#'
#' @param taxonomy_filename filename of taxonomy file
#' @param sep Character that separates fields of the taxonomy file. (Default: `\t`).
#'
#' @return dataframe of taxonomic labels, formatted by `parse_tax()`
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' taxonomy_filepath <- system.file("extdata",
#'   "test.taxonomy",
#'   package = "schtools"
#' )
#' taxonomy_tbl <- read_tax(taxonomy_filepath)
#' head(taxonomy_tbl)
read_tax <- function(taxonomy_filename, sep = "\t") {
  if (endsWith(taxonomy_filename, ".csv")) sep <- ","
  tax_df <- utils::read.table(taxonomy_filename,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
    parse_tax()
  return(tax_df)
}
