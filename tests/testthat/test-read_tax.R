tax_infilename <- system.file("extdata",
  "test.taxonomy",
  package = "schtools"
)
tax_raw_tbl <- readr::read_tsv(tax_infilename)

test_that("parse_tax works on example file", {
  taxonomy_out <- parse_tax(tax_raw_tbl)
  expect_equal(
    colnames(taxonomy_out),
    c(
      "otu", "otu_label", "tax_otu_label", "label_html", "kingdom",
      "phylum", "class", "order", "family", "genus"
    )
  )
  expect_equal(any(is.na(taxonomy_out)), FALSE)
  expect_match(taxonomy_out$otu, "Otu")
  expect_match(taxonomy_out$tax_otu_label, " \\(OTU ")
  expect_match(taxonomy_out$otu_label, "OTU ")
  expect_equal(
    taxonomy_out$label_html,
    c(
      "<i>Bacteroides</i> (OTU 1)",
      "<i>Porphyromonadaceae</i> (OTU 3)",
      "<i>Porphyromonadaceae</i> (OTU 4)",
      "<i>Enterobacteriaceae</i> (OTU 8)",
      "<i>Bacteria</i> (OTU 44)",
      "<i>Bacteria</i> (OTU 56)",
      "<i>Acinetobacter</i> (OTU 57)",
      "<i>Clostridium</i> XlVa (OTU 58)",
      "<i>Betaproteobacteria</i> (OTU 159)",
      "<i>Clostridium</i> XVIII (OTU 208)",
      "<i>Candidatus</i> Saccharibacteria (OTU 328)",
      "<i>Clostridiales</i> Incertae Sedis XIII (OTU 329)"
    )
  )
})
test_that("read_tax works as well as `read_tsv %>% parse_tax`", {
  expect_equal(
    read_tax(tax_infilename),
    parse_tax(tax_raw_tbl)
  )
})
