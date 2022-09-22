tax_dat <- data.frame(
  otu = c("Otu00001", "Otu00002", "Otu00003", "Otu00004", "Otu00005", "Otu00006"),
  kingdom = c("Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria"),
  phylum = c("Bacteroidetes", "Proteobacteria", "Proteobacteria", "Bacteroidetes", "Firmicutes", "Bacteroidetes"),
  class = c("Bacteroidia", "Gammaproteobacteria", "Gammaproteobacteria", "Bacteroidia", "Negativicutes", "Bacteroidia"),
  order = c("Bacteroidales", "Enterobacteriales", "Enterobacteriales", "Bacteroidales", "Selenomonadales", "Bacteroidales"),
  family = c("Bacteroidaceae", "Enterobacteriaceae", "Enterobacteriaceae", "Bacteroidaceae", "Veillonellaceae", "Bacteroidaceae"),
  genus = c("Bacteroides", "Enterobacteriaceae unclassified", "Enterobacteriaceae unclassified", "Bacteroides", "Veillonella", "Bacteroides")
)
shared_dat <- data.frame(
  label = rep.int(0.03, 6),
  Group = glue::glue("p{seq.int(1, 6, 1)}") %>% as.character(),
  numOtus = 6,
  Otu00001 = c(10, 0, 0, 0, 0, 1),
  Otu00002 = c(0, 10, 0, 0, 1, 0),
  Otu00003 = c(0, 0, 10, 1, 0, 0),
  Otu00004 = c(0, 0, 1, 10, 0, 0),
  Otu00005 = c(0, 1, 0, 0, 10, 0),
  Otu00006 = c(1, 0, 0, 0, 0, 10)
)

test_that("pool_taxon_counts works", {
  expect_equal(
    pool_taxon_counts(shared_dat, tax_dat, genus),
    list(
      shared = structure(
        list(
          label = c(
            "genus", "genus", "genus",
            "genus", "genus", "genus"
          ),
          Group = c(
            "p1", "p2", "p3", "p4",
            "p5", "p6"
          ),
          numOtus = c(3, 3, 3, 3, 3, 3),
          Otu1 = c(
            11, 0, 1,
            10, 0, 11
          ),
          Otu2 = c(0, 10, 10, 1, 1, 0),
          Otu3 = c(
            0, 1, 0, 0,
            10, 0
          )
        ),
        row.names = c(NA, -6L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      tax = structure(
        list(
          otu = c("Otu1", "Otu2", "Otu3"),
          size = c(
            33,
            22, 11
          ),
          genus = c(
            "Bacteroides",
            "Enterobacteriaceae unclassified",
            "Veillonella"
          )
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -3L)
      )
    )
  )
  expect_equal(
    pool_taxon_counts(shared_dat, tax_dat, kingdom),
    list(
      shared = structure(
        list(
          label = c(
            "kingdom",
            "kingdom",
            "kingdom",
            "kingdom",
            "kingdom",
            "kingdom"
          ),
          Group = c(
            "p1",
            "p2", "p3", "p4", "p5", "p6"
          ),
          numOtus = c(1, 1, 1, 1, 1, 1),
          Otu1 = c(11, 11, 11, 11, 11, 11)
        ),
        row.names = c(NA, -6L),
        class = c(
          "tbl_df",
          "tbl", "data.frame"
        )
      ),
      tax = structure(
        list(
          otu = "Otu1",
          size = 66,
          kingdom = "Bacteria"
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -1L)
      )
    )
  )
  expect_error(pool_taxon_counts(shared_dat, tax_dat, not_a_taxon))
})
