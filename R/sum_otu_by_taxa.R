###################
#
# sum_otu_by_taxa.R
#
# created by Nicholas Lesniak in 2016
# modified by Nicholas Lesniak in 2019
#    
# function outputs a dataframe with OTUs summed by taxa group per sample
# 
# taxonomy_df - dataframe formatted with convert_OTU_labels.R
# otu_df - dataframe with samples as column 'Group' and otus in columns
# taxa_level - genus, family, order, class, phylum, kingdom
# 
#
###################

levels <- c('Kingdom','Phylum','Class','Order','Family','Genus', 'OTU', 'tax_otu_label', 'otu_label')

sum_otu_by_taxa <- function(taxonomy_df, otu_df, taxa_level = 'NA', top_n = 0, silent = T){
  
  if(!all(is.data.frame(taxonomy_df), is.data.frame(otu_df),
    taxa_level %in% levels)) {stop(paste0(
      'Check to make sure you have entered a data frame for taxonomy and shared 
      and you have selected a classification level - ',
      paste0(levels, collapse = ', ')))}
  if(silent == F){
    print(paste0('Summing shared by ', taxa_level))
  }

  output_dataframe <- otu_df %>% 
    gather(OTU, abundance, starts_with('Otu')) %>% 
    inner_join(select(taxonomy_df, OTU, taxa = one_of(taxa_level)), by = 'OTU') %>% 
    group_by(Group, taxa) %>%
    summarise(abundance = sum(abundance)) %>% 
    ungroup

  if(top_n > 0){
    if(silent == F){
      print(paste0('Returning the top ', top_n, ' groups, all others are summed in Other'))
    }

    top_taxa <- output_dataframe %>% 
      group_by(taxa) %>% 
      summarise(median_abundance = mean(abundance, na.rm = T)) %>% 
      top_n(top_n, median_abundance) %>% 
      select(taxa) %>% 
      mutate(top_taxa = taxa)

    output_dataframe <- output_dataframe %>%
      full_join(top_taxa, by = 'taxa') %>%  
      mutate(taxa = ifelse(is.na(top_taxa), 'Other', taxa)) %>% 
      group_by(Group, taxa) %>% 
      summarise(abundance = sum(abundance)) %>% 
      mutate(dataset = paste0('top_', top_n, '_by_', taxa_level)) %>% 
      ungroup 

    } else {
      if(silent == F){
        print('Returning all groups')
      }
    }

  return(output_dataframe)
  }