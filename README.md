
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spex

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of spex is to provide an online tool for simple exploration of
biological datasets.

It is currently running
[here](https://www.bioinformatics.babraham.ac.uk/shiny/spex/spex/) on
the Babraham shiny server with an example dataset.

The initial page currently looks like this:
![](man/figures/README-start_page_screenshot.png)

The tabs allow the user to explore a dataset, metadata information and
an example plot are shown below.  
![](man/figures/README-metadata_screenshot.png)

![](man/figures/README-density_screenshot.png)

## Preparing data

### Main data file

A data file where each row contains a different genes (or other
markers), and each column contains a different sample e.g.

### Metadata file

sample names in the first column that must match the column names in the
main dataset, then conditions in subsequent columns e.g.

| sample\_name | class | time\_series |
|--------------|-------|--------------|
| DAO06        | DA    | 1440         |
| DAO09        | DA    | 1440         |
| OEA098       | OEA   | 240          |
| OEA068       | OEA   | 960          |
| OEA101       | OEA   | 960          |

Keep the name of the first column as “sample\_name” - it’s simpler for
processing.

## Installation

This is currently under initial development and has not been released.
