
#' Create a prose string from a list or vector
#'
#' The word 'and' is inserted before the last element and an Oxford comma is used.
#'
#' @param x a list or vector
#'
#' @return a string where each element in `x` is separated by a comma
#' @export
#' @author Pat Schloss \email{pschloss@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' paste_oxford_list(1:3)
#' paste_oxford_list(c("cats", "dogs", "turtles"))
paste_oxford_list <- function(x) {
  x <- purrr::map_chr(x, inline_hook)
  if (length(x) < 2) {
    prose <- as.character(x)
  } else if (length(x) == 2) {
    prose <- paste0(x, collapse = " and ")
  } else {
    length_x <- length(x)
    first_elements <- paste0(x[1:length_x - 1], collapse = ", ")
    prose <- paste0(first_elements, ", and ", x[[length_x]])
  }
  return(prose)
}

#' Inline hook for knitr to paste human-readable numbers.
#'
#' Pastes formatted `x` if numeric, otherwise `x` unmodified.
#' Circumvents R's automatic scientific notation.
#'
#' @param x inline code
#'
#' @return formatted `x` if numeric, otherwise `x` unmodified.
#' @export
#' @author Pat Schloss \email{pschloss@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' inline_hook(0.02)
#' inline_hook(.Machine$double.eps^0.5)
#' inline_hook("this is a string")
inline_hook <- function(x) {
  if (is.list(x)) {
    x <- unlist(x)
  }
  if (is.numeric(x)) {
    if (abs(x - round(x)) < .Machine$double.eps^0.5) {
      x_str <- paste(format(x, big.mark = ",", digits = 0, scientific = FALSE))
    } else {
      x_str <- paste(format(x, big.mark = ",", digits = 2, nsmall = 2, scientific = FALSE))
    }
  } else {
    x_str <- paste(x)
  }
  return(x_str)
}

#' Set knitr chunk options & inline hook
#'
#'  Call this function in the setup chunk of your R Markdown files.
#'
#' @export
#' @author Pat Schloss \email{pschloss@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
set_knitr_opts <- function() {
  knitr::opts_chunk$set(
    tidy = TRUE,
    echo = FALSE,
    eval = TRUE,
    warning = FALSE,
    cache = FALSE
  )
  knitr::knit_hooks$set(inline = inline_hook)
}
