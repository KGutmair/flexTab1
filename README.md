
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexTab1

<!-- badges: start -->
<!-- badges: end -->

## Overview

This function creates a highly flexible Table 1 for descriptive
statistics with options to customize both the statistical options and
layout. It supports:

- **Statistical Flexibility**: Various summary measures can be selected
  for categorical (*absolute*, *relative*) and numerical variables
  (*mean*, *median*, *sd*, *min*, *max*, *quartile1*, *quartile3*),
  arranged in any combination. *Missing values* frequencies for each
  variable can be optionally displayed. For group comparisons, options
  include displaying *p-values* and *standardized mean differences*
  (SMD).

- **Custom Layout**:The following layout options are available: *row
  sorting*, the option to display *summary measure units* (e.g., n (%),
  median (SD)), and an empty line after each variable for improved
  readability. The output can be generated as either a
  *publication-ready* `"flextable()"` *object* or as a `"data.frame"`,
  allowing for further modifications or integration with other table
  formatting packages.

- **Group Analysis**: Supports *single and grouped* summaries for
  comparing treatment arms or other groups. Additionally, a nested group
  structure is supported, allowing for the comparison of superior groups
  (A, B, and C), each with two subgroups (x and y).

This package is still an experimental version.

## Installation

You can install the development version of flexTab1 from
[GitHub](https://github.com/KGutmair/flexTab1)

``` r
# install.packages("devtools")
devtools::install_github("KGutmair/flexTab1")
```

## Usage

### Table 1 for one group/treatment arm

``` r
library(dplyr)
library(flexTab1)
library(flextable)



# Load pbc data from the survival package
pbc <- survival::pbc

baseline_var <- c("age", "chol", "sex", "stage", "platelet")

pbc <- pbc %>%
     mutate_at(c("stage", "trt", "edema", "hepato"), function(x) as.factor(x)) %>%
     filter(!is.na(trt)) %>%
     mutate(trt = ifelse(trt == "1", "D-penicillmain", "Placebo"))

 tab1_example <- Table1_flex(
     data = pbc,
     variables = baseline_var 
   )

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="47%" />

### Table 1 for two treatment arms

``` r
tab1_example <- Table1_flex(
  data = pbc,
  variables = baseline_var,
  group_var = "trt",
  add_measure_ident = TRUE,
  display_pvalue = TRUE, 
  sort_rows = c("age", "sex", "stage", "chol", "platelet"),
  measures_cat = c("absolute", "relative"),
  measures_num = c("median", "min", "max")
)

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="62%" />

### Table 1 for nested groups

The Table1_flex also supports a nested group structure, meaning having
subgroups within superior groups

``` r

baseline_var <- c("age", "chol", "platelet", "stage")
tab1_example <- Table1_flex(
  data = pbc,
  variables = baseline_var,
  group_var = "sex",
  treatment_arm = "trt",
  add_measure_ident = TRUE,
  measures_cat = c("relative"),
  measures_num = c("mean","sd"),
  flextable_output = TRUE
)

tab1_example %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" />
