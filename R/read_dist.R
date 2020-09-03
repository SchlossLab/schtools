#' Read in lower left triangular matrix from file
#'
#' @param dist_file_name file name of lower left triangular matrix
#'
#' @return distance matrix as a tibble
#' @export
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#'
#' @examples
#' # TODO: add dist file to inst/ext-data and create a real example of using this
read_dist <- function(dist_file_name) {
  # TODO: input validation - make sure file has expected format & throw errors if it doesn't
  # read in the first row to determine the matrix dimensions
  matrix_dim <-
    as.numeric(utils::read.table(dist_file_name, nrow = 1, as.is = TRUE))
  # read in all the data from the lower triangle (exlcude the first which is the matrix dim)
  distance_matrix <-
    utils::read.table(
      dist_file_name,
      fill = TRUE,
      skip = 1,
      col.names = c(as.character(1:matrix_dim)),
      stringsAsFactor = F
    )
  # add column names based on row names
  colnames(distance_matrix) <-
    c('rows', distance_matrix$X1[-matrix_dim])
  # convert to long form and eliminate NAs (upper right of triangle)
  return(
    distance_matrix %>%
      dplyr::pivot_longer(
        col = -rows,
        values_to = 'distances',
        names_to = 'columns'
      ) %>%
      dplyr::filter(!is.na(distances))
  )
}
