library(dplyr)
library(tidyr)
shared_dat <- data.frame(
    label = rep.int(0.03, 3),
    Group = glue::glue("p{seq.int(1, 3, 1)}") %>% as.character(),
    numOtus = 3,
    Otu00001 = c(10, 0, 0), # 10
    Otu00002 = c(2, 10, 2), # 14
    Otu00003 = c(2, 1, 10), # 13
    Otu00004 = c(2, 2, 0),  # 04
    Otu00005 = c(2, 0, 10), # 12
    Otu00006 = c(0, 11, 12) # 23
)
test_that("calc_rel_abun works", {
    expect_equal( # rows sum to 1
        calc_relabun(shared_dat) %>%
            pivot_wider(names_from = "otu", values_from = "rel_abun") %>%
            select(starts_with('Otu')) %>%
            rowSums(),
        rep(1, 3)
    )
    expect_equal(calc_relabun(shared_dat),
                 structure(
                     list(
                         sample = c(
                             "p1",
                             "p1",
                             "p1",
                             "p1",
                             "p1",
                             "p1",
                             "p2",
                             "p2",
                             "p2",
                             "p2",
                             "p2",
                             "p2",
                             "p3",
                             "p3",
                             "p3",
                             "p3",
                             "p3",
                             "p3"
                         ),
                         otu = c(
                             "Otu00001",
                             "Otu00002",
                             "Otu00003",
                             "Otu00004",
                             "Otu00005",
                             "Otu00006",
                             "Otu00001",
                             "Otu00002",
                             "Otu00003",
                             "Otu00004",
                             "Otu00005",
                             "Otu00006",
                             "Otu00001",
                             "Otu00002",
                             "Otu00003",
                             "Otu00004",
                             "Otu00005",
                             "Otu00006"
                         ),
                         rel_abun = c(
                             0.555555555555556,
                             0.111111111111111,
                             0.111111111111111,
                             0.111111111111111,
                             0.111111111111111,
                             0,
                             0,
                             0.416666666666667,
                             0.0416666666666667,
                             0.0833333333333333,
                             0,
                             0.458333333333333,
                             0,
                             0.0588235294117647,
                             0.294117647058824,
                             0,
                             0.294117647058824,
                             0.352941176470588
                         )
                     ),
                     row.names = c(NA,-18L),
                     class = c("tbl_df", "tbl", "data.frame")
                 ))
})
