#' Convert taxonomy strings into dataframe of labels based on taxnomic classification
#'
#' @param taxonomy_filename filename of taxonomy file
#'
#' @return dataframe of taxonomic labels
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' taxonomy_filepath <- system.file("extdata",
#'   "test.taxonomy",
#'   package = "mothuR"
#' )
#' taxonomy_tbl <- read_tax(taxonomy_filepath)
#' head(taxonomy_tbl)
read_tax <- function(taxonomy_file){
  levels <- c('Kingdom','Phylum','Class','Order','Family','Genus')
  taxonomy_df <- utils::read.table(taxonomy_file, 
    sep = '\t', 
    header = T, 
    stringsAsFactors = F
    ) %>% 
    dplyr::mutate(Taxonomy = gsub('_', ' ', Taxonomy)) %>% 
    tidyr::separate(Taxonomy, levels, sep = '\\(\\d{2,3}\\);', extra = 'drop') %>% 
    dplyr::select(-Size) 
  # in older version of mothur unclassified are listed as unclassified
  # without information from higher level classification
  # for those cases, append with lowest identified classification
  if(any(taxonomy_df$Genus == 'unclassified')){ 
    taxonomy_df <- taxonomy_df %>% 
      tidyr::pivot_longer(
        cols = -.data[['OTU']],
        names_to = 'Level', 
        values_to = 'Classification'
        ) %>% 
      # order classification level
      dplyr::mutate(Level = factor(Level, levels)) %>% 
      dplyr::left_join(dplyr::group_by(., OTU) %>% 
          dplyr::filter(Classification != 'unclassified') %>% 
          # select lowest level classification
          dplyr::filter(Level == levels[max(as.numeric(Level))]) %>% 
          dplyr::select(OTU, Lowest_classified = Classification), 
        by = 'OTU') %>% 
      dplyr::mutate(Classification = ifelse(Classification == 'unclassified', 
          # append unclassified with lowest classification
          paste(Lowest_classified, Classification, sep= ' '), 
          Classification)) %>% 
      dplyr::select(-Lowest_classified) %>% 
      tidyr::pivot_wider(
        names_from = 'Level', 
        values_from = 'Classification')
  }
  # create label options for OTU and lowest taxonomic classification with the OTU
  taxonomy_df <- taxonomy_df %>%  
      dplyr::mutate(
      	tax_otu_label = paste0(Genus, ' (', gsub('tu0*', 'TU ', OTU),')'),
        tax_otu_label = gsub(' unclassified', '', tax_otu_label),
        otu_label = paste0(gsub('tu0*', 'TU ', OTU))
        )
  return(taxonomy_df)
}