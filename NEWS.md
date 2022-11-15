# schtools 0.3.1

- Fixes for API changes in ggplot2 v3.4.0 and tidyselect v1.2.0 (#42, #44, @kelly-sovacool).
- `pool_taxon_counts()` now takes the `taxon_level` argument as a character 
  rather than a data variable (#43, @kelly-sovacool).

# schtools 0.3.0

- New function `log_snakemake()` redirects all output to a rule-specific log
  file when called by a Snakemake rule (#35, @kelly-sovacool).
- New function `pool_taxon_counts()` pools OTU counts at a particular taxonomic 
  level (e.g. genus, phylum, etc.) to enable comparing analyses at different 
  taxonomic resolutions, as seen in <https://doi.org/10.1128/mbio.03161-21> 
  (#37, @kelly-sovacool).
- New function `calc_relabun()` calculates OTU relative abundances (#38, @kelly-sovacool).
- New custom themes for ggplot2:
    - `theme_sovacool()` (#39, @kelly-sovacool)
    - `theme_lucas()` (#40, @sklucas)

# schtools 0.2.1

- Improvements to `parse_tax()` (#32, @kelly-sovacool).
    - New column `label_html` provides HTML to correctly italicize genus names but not OTU labels.
    - Added an example to the intro vignette to demonstrate using this feature with `ggtext`.
    - Changed taxonomy data frame column names to lowercase and reordered them.
- Minor documentation improvements (#33, @kelly-sovacool).

# schtools 0.1.1

- Refactored `read_tax()` (#30, @kelly-sovacool).
    - New function `parse_tax()` to separate the logic of reading and parsing taxonomy files.
    - `read_tax()` now accepts a `sep` parameter to handle different delim file types (e.g. csv or tsv).
- schtools now has a logo! (#31, @kelly-sovacool)

# schtools 0.1.0

This is the first release version of schtools! 🎉

- You can now install schtools from [CRAN](https://cran.r-project.org/package=schtools)!
- Added a `NEWS.md` file to track changes to the package.
- Major new functions:
  - `read_dist()` (#10, @NLesniak)
  - `read_tax()` (#22, @NLesniak)
  - `set_knitr_opts()` (#21, @pschloss, @kelly-sovacool)
  - `inline_hook()` (#21, @pschloss, @kelly-sovacool)
  - `paste_oxford_list()` (#21, @pschloss, @kelly-sovacool)
  - `format_number()` (#24, @pschloss, @kelly-sovacool)
  - `is_nearly_whole()` (#24, @pschloss, @kelly-sovacool)
  - `close_enough()` (#26, @kelly-sovacool)
  - `is_nondesc()` (#28, @kelly-sovacool)
- New vignettes:
  - `introduction`
