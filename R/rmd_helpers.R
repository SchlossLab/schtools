
#' Create a prose string from a list or vector
#'
#' The word 'and' is inserted before the last element and an Oxford comma is used.
#'
#' @param x a list or vector
#'
#' @return a string where each element in `x` is separated by a comma
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' paste_oxford_list(1:3)
#' paste_oxford_list(c("cats", "dogs", "turtles"))
paste_oxford_list <- function(x) {
  if (length(x) < 2) {
    prose <- as.character(x)
  } else if (length(x) == 2) {
    prose <- paste(x, collapse = " and ")
  } else {
    length_x <- length(x)
    first_elements <- paste0(x[1:length_x - 1], collapse = ", ")
    prose <- paste0(first_elements, ", and ", x[[length_x]])
  }
  return(prose)
}
