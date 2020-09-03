###################
#
# read_dist.R
#
# reads triangle matrices into R   
#
###################

library(tidyverse)

read_dist <- function(dist_file_name){
  # read in the first row to determine the matrix dimensions
  matrix_dim <- as.numeric(read.table(dist_file_name, nrow = 1, as.is = TRUE))
  # read in all the data from the lower triangle (exlcude the first which is the matrix dim)
  distance_matrix <- read.table(dist_file_name, fill = TRUE, skip = 1, 
    col.names = c(as.character(1:matrix_dim)),
    stringsAsFactor = F)
  # add column names based on row names
  colnames(distance_matrix) <- c('rows', distance_matrix$X1[-matrix_dim])
  # convert to long form and eliminate NAs (upper right of triangle)
  distance_matrix %>% 
    pivot_longer(col = -rows, values_to = 'distances', names_to = 'columns') %>% 
    filter(!is.na(distances))
}


