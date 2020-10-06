
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

#' Inline hook for knitr to paste human-readable numbers
#'
#' Pastes rounded `x` if numeric, otherwise `x` unmodified.
#'
#' @param x inline code
#'
#' @return rounded `x` if numeric, otherwise `x` unmodified.
#' @export
#' @author Pat Schloss \email{pschloss@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' inline_hook(0.02)
#' inline_hook(.Machine$double.eps^0.5)
#' inline_hook('this is a string')
inline_hook <- function(x){
    print(x)
    if(is.numeric(x)) {
        if(abs(x - round(x)) < .Machine$double.eps^0.5){
            paste(format(x,big.mark=',', digits=0, scientific=FALSE))
        } else {
            paste(format(x,big.mark=',', digits=2, nsmall=2, scientific=FALSE))
        }
    } else {
        paste(x)
    }
}
