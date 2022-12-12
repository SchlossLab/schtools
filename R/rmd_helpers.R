#' Inline hook for knitr to paste human-readable numbers and nice lists.
#'
#' @param x just about anything
#'
#' @return a string where each element in `x` is separated by a comma and numbers
#'   are in a human-readable format.
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#' @author Pat Schloss \email{pschloss@@umich.edu}
#'
#' @examples
#' inline_hook(c(1.2993992, 0.03, 1000))
#' inline_hook(c("cats", "dogs"))
inline_hook <- function(x) {
  x_formatted <- purrr::map_chr(x, format_number)
  return(paste_oxford_list(x_formatted))
}

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

#' Format human-readable numbers.
#'
#' Pastes formatted `x` if numeric, otherwise `x` unmodified.
#' Circumvents R's automatic scientific notation.
#' If a number is nearly whole (see `is_nearly_whole()`), it is rounded to have
#' zero decimal places. Otherwise, numbers >= 1 are rounded to 1 decimal place;
#' numbers < 1 are rounded to have 2 significant digits.
#'
#' @param x inline code
#' @param nsmall number of digits after the decimal point to round to when `x`
#'   is not nearly whole but `x >= 1`.
#' @param signif_precise number of significant digits to use when `x` is not
#'   nearly whole
#'
#'
#' @return formatted `x` if numeric, otherwise `x` unmodified.
#' @export
#' @author Pat Schloss \email{pschloss@@umich.edu}
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' format_number(0.0256)
#' format_number(.Machine$double.eps^0.5)
#' format_number(100000.08)
#' format_number(1.00000000000000000001)
#' format_number("this is a string")
format_number <- function(x, nsmall = 1, signif_precise = 2) {
  if (is.list(x)) {
    x <- unlist(x)
  }
  if (is.numeric(x)) {
    if (isTRUE(is_nearly_whole(x))) {
      signif_digits <- NULL # drop the decimal digits entirely
      nsmall <- 0
    } else { # need more precision
      signif_digits <- signif_precise
      if (isTRUE(x >= 1)) { # only round to `nsmall` if it's greater than or equal to 1
        x <- round(x, nsmall)
      } else {
        nsmall <- signif_precise
      }
    }
    x_str <- paste(format(x,
      digits = signif_digits, nsmall = nsmall,
      big.mark = ",", scientific = FALSE
    ))
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
    message = FALSE,
    warning = FALSE,
    cache = FALSE
  )
  knitr::knit_hooks$set(inline = inline_hook)
}
