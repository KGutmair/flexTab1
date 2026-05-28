
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexTab1

<!-- badges: start -->

[![R-CMD-check](https://github.com/KGutmair/flexTab1/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KGutmair/flexTab1/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KGutmair/flexTab1/graph/badge.svg)](https://app.codecov.io/gh/KGutmair/flexTab1)
<!-- badges: end -->

An R package for creating flexible, publication-ready summary tables (Table 1s) for descriptive statistics. Customize both statistical measures and table layout to meet your exact reporting needs.

## Overview

This function creates a highly flexible summary measure table for
descriptive statistics ("Table 1") with options to customize both the
statistical options and the output layout. It supports:

- **Statistical Flexibility**: Various summary measures can be selected
  for categorical (*absolute*, *relative*) and numerical variables
  (*mean*, *median*, *sd*, *min*, *max*, *quartile1*, *quartile3*),
  arranged in any combination. *Missing values* frequencies for each
  variable can be optionally displayed. For group comparisons, options
  include displaying *p-values* and *standardized mean differences*
  (SMD).

- **Custom Layout**: The following layout options are available: *row
  sorting* and *column sorting*, the option to display *summary measure
  units* (e.g., n (%), median (SD)), and an empty line after each
  variable for improved readability. The output can be generated as
  either a *publication-ready* output table (created with the
  `flextable` package) or as a `data.frame`, allowing for further
  modifications or integration with other table formatting packages.

- **Group Analysis**: Supports summary measures for a *single group* and
  stratified by *two or more groups*. Additionally, a nested group
  structure is supported, allowing for the comparison of subgroups
  within main groups.

## Installation

You can install the development version of flexTab1 from
[GitHub](https://github.com/KGutmair/flexTab1):

``` r
# install.packages("devtools")
devtools::install_github("KGutmair/flexTab1")
```

**Requirements**: R (>= 3.6.0)

## Quick Start

Get started in just a few lines:

``` r
library(flexTab1)

# Create a simple Table 1
tab1 <- Table1_flex(
  data = your_data,
  variables = c("age", "sex", "treatment"),
  measures_num = c("mean", "sd"),
  measures_cat = c("absolute", "relative")
)

tab1
```

## Usage

### Main Function: `Table1_flex()`

**Key Parameters:**
- `data`: Your dataset
- `variables`: Vector of variable names to include in the table
- `group_var`: Optional grouping variable for stratified comparisons
- `treatment_arm`: Optional nested grouping variable (subgroups within groups)
- `measures_num`: Summary measures for numeric variables (default: `c("mean", "sd")`)
- `measures_cat`: Summary measures for categorical variables (default: `c("absolute", "relative")`)
- `add_measure_ident`: If `TRUE`, displays summary measure units (e.g., "n (%)", "mean (SD)")
- `display_pvalue`: If `TRUE`, includes p-values for group comparisons
- `display_smd`: If `TRUE`, includes standardized mean differences
- `flextable_output`: If `TRUE`, returns a `flextable` object; if `FALSE`, returns a `data.frame`
- `sort_rows`: Custom row sorting order
- `group_order`: Custom group ordering

### Loading data + preparation

``` r
library(dplyr)
library(flexTab1)
library(flextable)



# Load pbc data from the survival package
 pbc <- survival::pbc

   baseline_var <- c("age", "chol", "sex", "stage", "platelet")

   pbc[c("stage", "trt", "edema", "hepato")] <-
     lapply(pbc[c("stage", "trt", "edema", "hepato")], as.factor)

   pbc <- pbc[!is.na(pbc$trt), ]
 pbc$trt <- ifelse(pbc$trt == "1",
 "D-penicillamine",
 "Placebo")
```

### Table 1 for one group/treatment arm

``` r

tab1_example <- Table1_flex(
     data = pbc,
     variables = baseline_var,
     measures_num = c("mean", "sd"),
     measures_cat = c("absolute", "relative")
   )

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="40%" />

### Table 1 for two treatment arms

``` r
tab1_example <- Table1_flex(
  data = pbc,
  variables = baseline_var,
  group_var = "trt")


tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="62%" />

``` r
tab1_example <- Table1_flex(
  data = pbc,
  variables = baseline_var,
  group_var = "trt",
  add_measure_ident = FALSE,
  display_pvalue = TRUE, 
  display_smd = TRUE,
  sort_rows = c("age", "sex", "stage", "chol", "platelet"),
  measures_cat = c("absolute", "relative"),
  measures_num = c("median", "min", "max"),
  group_order = c("Placebo", "D-penicillamine")
)

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="62%" />

### Table 1 for nested groups

The `Table1_flex()` function also supports a nested group structure, meaning having
subgroups within main groups:

``` r

baseline_var <- c("age", "chol", "platelet", "stage")
tab1_example <- Table1_flex(
  data = pbc,
  variables = baseline_var,
  group_var = "sex",
  treatment_arm = "trt",
  add_measure_ident = TRUE,
  display_pvalue = TRUE,
  measures_cat = c("relative"),
  measures_num = c("mean","sd"),
  flextable_output = TRUE
)

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="70%" />

## Citation

If you use `flexTab1` in your research, please cite it as:

```
# Citation to be added
```

## License

[Add your license information here, e.g., MIT, GPL-3.0, etc.]

## Contributing

We welcome contributions! Please:

- Report bugs and request features via [GitHub Issues](https://github.com/KGutmair/flexTab1/issues)
- Submit pull requests with improvements
- Follow standard R package conventions

## Getting Help

For questions and discussions:
- Check existing [GitHub Issues](https://github.com/KGutmair/flexTab1/issues)
- Create a new issue for bug reports or feature requests
