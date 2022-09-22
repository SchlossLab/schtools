
#' Calculate OTU relative abundances from a shared file
#'
#' @param abs_abun_dat a data frame from reading in a [shared file](https://mothur.org/wiki/shared_file).
#'   Should contain a `Group` column for sample names,
#'   `Otu` columns for absolute counts of each OTU,
#'   and rows as each sample.
#'
#' @return a new data frame with OTU relative abundances in long format.
#' @export
#' @author Kelly Sovacool \email{sovacool@@umich.edu}
#'
#' @examples
#' shared_dat <- readr::read_tsv(system.file("extdata", "test.shared",
#'                                           package = "schtools"))
#' shared_dat %>% calc_relabun()
calc_relabun <- function(abs_abun_dat) {
    abs_abun_dat$total_counts <- rowSums(abs_abun_dat %>% select(starts_with('Otu')))
    rel_abun_dat <- abs_abun_dat %>%
        rename(sample = Group) %>%
        pivot_longer(starts_with("Otu"), names_to = 'otu', values_to = 'count') %>%
        mutate(rel_abun = count / total_counts) %>%
        select(sample, otu, rel_abun)
    return(rel_abun_dat)
}
