###################
#
# convert_OTU_labels.R
#
# creates table of tax labels for OTUs
# created by Nicholas Lesniak in 2016
# updated by Nicholas Lesniak in 2019
#
# convert mothur output to a dataframe 
# 
# taxa_file - taxonomy file location
#
###################

library(dplyr)
library(tidyr)
library(readr)

taxonomy_file <- 'data/process/final.taxonomy'
taxonomy_df <- read.table(taxonomy_file, sep = '\t', header = T)

levels <- c('Kingdom','Phylum','Class','Order','Family','Genus')

# convert mothur taxonomy string to columns
taxonomy_df_edit <- taxonomy_df %>% 
	separate(Taxonomy, levels, sep = '\\(\\d{2,3}\\);', extra = 'drop') %>% 
	select(-Size) 
# in older version of mothur unclassified are listed as unclassified
# without information from higher level classification
# for those cases, append with lowest identified classification
if(any(taxonomy_df_edit$Genus == 'unclassified')){ 
	taxonomy_df_edit <- taxonomy_df_edit %>% 
		gather(Level, Classification, -OTU) %>% # convert to long form to group classification by OTU 
		mutate(Level = factor(Level, levels))  %>% # order classification level
		left_join(group_by(., OTU) %>% # create dataframe with OTU and lowest level classification
				filter(Classification != 'unclassified') %>% # remove unclassifieds
				filter(Level == levels[max(as.numeric(Level))]) %>% # select lowest level classification
				select(OTU, Lowest_classified = Classification), 
			by = 'OTU') %>% 
		mutate(Classification = ifelse(Classification == 'unclassified', 
				paste(Lowest_classified, Classification, sep= '_'), # append unclassified with lowest classification
				Classification)) %>% 
		select(-Lowest_classified) %>% 
		spread(Level, Classification)
}

# edit output to have cleaned up OTU
taxonomy_df_edit <- taxonomy_df_edit %>%  
    mutate(tax_otu_label = paste0(Genus, ' (',  
        gsub('tu0*', 'TU ', OTU),')'), # create labels 
      otu_label = paste0(gsub('tu0*', 'TU ', OTU))) # create labels  

write_tsv(taxonomy_df_edit, 'data/process/abx_cdiff_taxonomy_clean.tsv')