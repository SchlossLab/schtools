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
  Classification <- Genus <- Level <- Lowest_classified <- OTU <- Size <- Taxonomy <- tax_otu_label <- NULL
  dat_sep <- dat %>%
    dplyr::mutate(Taxonomy = gsub("_", " ", Taxonomy)) %>%
    tidyr::separate(Taxonomy, levels, sep = "\\(\\d{2,3}\\);", extra = "drop") %>%
    dplyr::select(-Size)
  # in older version of mothur unclassified are listed as unclassified
  # without information from higher level classification
  # for those cases, append with lowest identified classification
  if ("unclassified" %in% (dat_sep %>% dplyr::pull('Genus'))) {
    dat_sep <- dat_sep %>%
      tidyr::pivot_longer(
        cols = -OTU,
        names_to = "Level",
        values_to = "Classification"
      ) %>%
      # order classification level
      dplyr::mutate(Level = factor(Level, levels)) %>%
      dplyr::left_join(dplyr::group_by(., OTU) %>%
        dplyr::filter(Classification != "unclassified") %>%
        # select lowest level classification
        dplyr::filter(Level == levels[max(as.numeric(Level))]) %>%
        dplyr::select(OTU, Lowest_classified = Classification),
      by = "OTU"
      ) %>%
      dplyr::mutate(Classification = ifelse(Classification == "unclassified",
        # append unclassified with lowest classification
        paste(Lowest_classified, Classification, sep = " "),
        Classification
      )) %>%
      dplyr::select(-Lowest_classified) %>%
      tidyr::pivot_wider(
        names_from = "Level",
        values_from = "Classification"
      )
  }
  # create label options for OTU and lowest taxonomic classification with the OTU
  dat_labs <- dat_sep %>%
    dplyr::mutate(
      tax_otu_label = paste0(Genus, " (", gsub("tu0*", "TU ", OTU), ")"),
      tax_otu_label = gsub(" unclassified", "", tax_otu_label),
      otu_label = paste0(gsub("tu0*", "TU ", OTU)),
      label_html = stringr::str_replace(
        tax_otu_label,
        "([a-zA-Z]+) (.*)",
        glue::glue("<i>\\1</i> \\2")
      )
    )
  colnames(dat_labs) <- tolower(colnames(dat_labs))
  dat_reordered <- dat_labs %>%
    dplyr::relocate(c("otu_label", "tax_otu_label", "label_html"),
      .after = "otu"
    )
  return(dat_reordered)
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
