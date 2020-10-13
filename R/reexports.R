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
