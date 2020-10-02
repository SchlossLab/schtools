#' Convert taxonomy strings into dataframe of labels based on taxnomic classification
#'
#' @importFrom magrittr %>%
#' @param taxonomy_filename filename of taxonomy file
#'
#' @return dataframe of taxonomic labels
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' taxonomy_filepath <- system.file("extdata",
#'   "final.taxonomy",
#'   package = "mothuR"
#' )
#' taxonomy_tbl <- convert_taxonomy_to_df(taxonomy_filepath)
#' head(taxonomy_tbl)
convert_taxonomy_to_df <- function(taxonomy_file){
  levels <- c('Kingdom','Phylum','Class','Order','Family','Genus')
  taxonomy_df <- utils::read.table(taxonomy_file, 
    sep = '\t', 
    header = T, 
    stringsAsFactors = F
    ) %>% 
  # convert mothur taxonomy string to columns
    tidyr::separate(Taxonomy, levels, sep = '\\(\\d{2,3}\\);', extra = 'drop') %>% 
    dplyr::select(-Size) 
  # in older version of mothur unclassified are listed as unclassified
  # without information from higher level classification
  # for those cases, append with lowest identified classification
  if(any(taxonomy_df$Genus == 'unclassified')){ 
    taxonomy_df <- taxonomy_df %>% 
      # convert to long form to group classification by OTU 
      tidyr::gather(Level, Classification, -OTU) %>% 
      # order classification level
      dplyr::mutate(Level = factor(Level, levels))  %>% 
      # create dataframe with OTU and lowest level classification
      dplyr::left_join(dplyr::group_by(., OTU) %>% 
          # remove unclassifieds
          filter(Classification != 'unclassified') %>% 
          # select lowest level classification
          filter(Level == levels[max(as.numeric(Level))]) %>% 
          select(OTU, Lowest_classified = Classification), 
        by = 'OTU') %>% 
      dplyr::mutate(Classification = ifelse(Classification == 'unclassified', 
          # append unclassified with lowest classification
          paste(Lowest_classified, Classification, sep= '_'), 
          Classification)) %>% 
      dplyr::select(-Lowest_classified) %>% 
      tidyr::spread(Level, Classification)
  }
  # create label options for OTU and lowest taxonomic classification with the OTU
  taxonomy_df <- taxonomy_df %>%  
      dplyr::mutate(
      	tax_otu_label = paste0(Genus, ' (', gsub('tu0*', 'TU ', OTU),')'),
        otu_label = paste0(gsub('tu0*', 'TU ', OTU))
        )
  return(taxonomy_df)
}