
calc_rel_abun <- function(abs_abun_dat) {
    abs_abun_dat$total_counts <- rowSums(abs_abun_dat %>% select(starts_with('Otu')))
    rel_abun_dat <- abs_abun_dat %>% rename(sample = Group) %>%
        pivot_longer(starts_with("Otu"), names_to = 'otu', values_to = 'count') %>%
        mutate(rel_abun = count / total_counts) %>%
        select(sample, otu, rel_abun)
}
