#' dplyr pipe
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' rlang data pronoun
#' @importFrom rlang .data
#' @export
rlang::.data

## make R CMD CHECK shut up about the dot `.``
## See: \url{https://github.com/tidyverse/magrittr/issues/29}
utils::globalVariables(c("."))

## Suppress R CMD check note 'All declared Imports should be used'.
## The formatR package is needed on Ubuntu when tidy=TRUE in knitr opts.
#' @importFrom formatR tidy_source
NULL
