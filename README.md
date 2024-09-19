
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexTab1

<!-- badges: start -->
<!-- badges: end -->

The goal of flexTab1 is to create a highly flexible Table 1 for
descriptive statistics, allowing users to customize the output format,
choose appropriate summary measures, and optionally display missing
values, p-values (testing for difference between two groups), and
standardized mean differences (SMD). It is designed to adapt to various
reporting needs and ensures comprehensive data summaries in a
user-friendly format.

This package is still an experimental version.

## Installation

You can install the development version of flexTab1 from
[GitHub](https://github.com/KGutmair/flexTab1)

``` r
# install.packages("devtools")
devtools::install_github("KGutmair/flexTab1")
```

## Example

Table 1 for one group/treatment arm

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

 tab1_ex <- Table1_flex(
     data = pbc,
     variables = baseline_var 
   )

tab1_ex %>%
  fontsize(size = 11, part = "all") %>%
   line_spacing(space = 1.7, part = "all") %>%
   autofit() 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="50%" />
