test_that("read_tax works on example file", {
  taxonomy_out <- read_tax(system.file("extdata",
    "test.taxonomy",
    package = "schtools"
  ))
  expect_equal(
    colnames(taxonomy_out),
    c("OTU", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "tax_otu_label", "otu_label")
  )
  expect_equal(any(is.na(taxonomy_out)), FALSE)
  expect_match(taxonomy_out$OTU, "Otu")
  expect_match(taxonomy_out$tax_otu_label, " \\(OTU ")
  expect_match(taxonomy_out$otu_label, "OTU ")
})
