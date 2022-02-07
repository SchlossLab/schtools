#' Convert taxonomy strings into dataframe of labels based on taxnomic classification
#'
#' @param dat datframe from mothur taxonomy file with columns `OTU`, `Size`, and `Taxonomy`
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
parse_tax <- function(dat, sep = '\t') {
  levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
  dat <- dat %>%
    dplyr::mutate(Taxonomy = gsub("_", " ", .data[["Taxonomy"]])) %>%
    tidyr::separate(.data[["Taxonomy"]], levels, sep = "\\(\\d{2,3}\\);", extra = "drop") %>%
    dplyr::select(-.data[["Size"]])
  # in older version of mothur unclassified are listed as unclassified
  # without information from higher level classification
  # for those cases, append with lowest identified classification
  if (any(dat$Genus == "unclassified")) {
    dat <- dat %>%
      tidyr::pivot_longer(
        cols = -.data[["OTU"]],
        names_to = "Level",
        values_to = "Classification"
      ) %>%
      # order classification level
      dplyr::mutate(Level = factor(.data[["Level"]], levels)) %>%
      dplyr::left_join(dplyr::group_by(., .data[["OTU"]]) %>%
        dplyr::filter(.data[["Classification"]] != "unclassified") %>%
        # select lowest level classification
        dplyr::filter(.data[["Level"]] == levels[max(as.numeric(.data[["Level"]]))]) %>%
        dplyr::select(.data[["OTU"]], Lowest_classified = .data[["Classification"]]),
      by = "OTU"
      ) %>%
      dplyr::mutate(Classification = ifelse(.data[["Classification"]] == "unclassified",
        # append unclassified with lowest classification
        paste(.data[["Lowest_classified"]], .data[["Classification"]], sep = " "),
        .data[["Classification"]]
      )) %>%
      dplyr::select(-.data[["Lowest_classified"]]) %>%
      tidyr::pivot_wider(
        names_from = "Level",
        values_from = "Classification"
      )
  }
  # create label options for OTU and lowest taxonomic classification with the OTU
  dat <- dat %>%
    dplyr::mutate(
      tax_otu_label = paste0(.data[["Genus"]], " (", gsub("tu0*", "TU ", .data[["OTU"]]), ")"),
      tax_otu_label = gsub(" unclassified", "", .data[["tax_otu_label"]]),
      otu_label = paste0(gsub("tu0*", "TU ", .data[["OTU"]]))
    )
  return(dat)
}

#' Read in a taxonomy file and parse it to a wide dataframe
#'
#' @param taxonomy_filename filename of taxonomy file
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
read_tax <- function(taxonomy_filename, sep = '\t') {
  if (endsWith(taxonomy_filename, '.csv')) sep <- ','
  tax_df <- utils::read.table(taxonomy_filename,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
      parse_tax()
  return(tax_df)
}
