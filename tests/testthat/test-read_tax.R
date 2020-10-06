test_that("convert_taxonomy_to_df works on example file", {
  taxonomy_out <- read_tax(system.file("extdata",
    OTU = c("Otu0001", "Otu0002", "Otu0003", "Otu0004", "Otu0005", "Otu0006"),
    Kingdom = c("Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria"),
    Phylum = c("Bacteroidetes", "Verrucomicrobia", "Bacteroidetes", "Bacteroidetes", "Firmicutes", "Bacteroidetes"),
    Class = c("Bacteroidia", "Verrucomicrobiae", "Bacteroidia", "Bacteroidia", "Bacilli", "Bacteroidia"),
    Order = c("Bacteroidales", "Verrucomicrobiales", "Bacteroidales", "Bacteroidales", "Lactobacillales", "Bacteroidales"),
    Family = c("Bacteroidaceae", "Verrucomicrobiaceae", "Porphyromonadaceae", "Porphyromonadaceae", "Lactobacillaceae", "Porphyromonadaceae"),
    Genus = c("Bacteroides", "Akkermansia", "Porphyromonadaceae_unclassified", "Porphyromonadaceae_unclassified", "Lactobacillus", "Porphyromonadaceae_unclassified"),
    tax_otu_label = c("Bacteroides (OTU 1)", "Akkermansia (OTU 2)", "Porphyromonadaceae_unclassified (OTU 3)", "Porphyromonadaceae_unclassified (OTU 4)", "Lactobacillus (OTU 5)", "Porphyromonadaceae_unclassified (OTU 6)"),
    otu_label = c("OTU 1", "OTU 2", "OTU 3", "OTU 4", "OTU 5", "OTU 6")
  ),
  row.names = c(1:6),
  class = c("data.frame")
  )
  tbl_tail <- structure(list(
    OTU = c("Otu5581", "Otu5582", "Otu5583", "Otu5584", "Otu5585", "Otu5586"),
    Kingdom = c("Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria", "Bacteria"),
    Phylum = c("Bacteroidetes", "Bacteroidetes", "Bacteroidetes", "Bacteroidetes", "Bacteroidetes", "Firmicutes"),
    Class = c("Bacteroidia", "Bacteroidia", "Bacteroidia", "Bacteroidia", "Bacteroidia", "Clostridia"),
    Order = c("Bacteroidales", "Bacteroidales", "Bacteroidales", "Bacteroidales", "Bacteroidales", "Clostridiales"),
    Family = c("Bacteroidaceae", "Porphyromonadaceae", "Porphyromonadaceae", "Porphyromonadaceae", "Porphyromonadaceae", "Lachnospiraceae"),
    Genus = c("Bacteroides", "Porphyromonadaceae_unclassified", "Porphyromonadaceae_unclassified", "Porphyromonadaceae_unclassified", "Porphyromonadaceae_unclassified", "Lachnospiraceae_unclassified"),
    tax_otu_label = c("Bacteroides (OTU 5581)", "Porphyromonadaceae_unclassified (OTU 5582)", "Porphyromonadaceae_unclassified (OTU 5583)", "Porphyromonadaceae_unclassified (OTU 5584)", "Porphyromonadaceae_unclassified (OTU 5585)", "Lachnospiraceae_unclassified (OTU 5586)"),
    otu_label = c("OTU 5581", "OTU 5582", "OTU 5583", "OTU 5584", "OTU 5585", "OTU 5586")
  ),
  row.names = c(5581:5586),
  class = c("data.frame")
  )
  taxonomy_out <- convert_taxonomy_to_df(system.file("extdata",
    "final.taxonomy",
    package = "mothuR"
  ))
  expect_equal(ncol(taxonomy_out), 9)
  expect_equal(head(taxonomy_tbl), tbl_head)
  expect_equal(tail(taxonomy_tbl), tbl_tail)
})
