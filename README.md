
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PrePost

<!-- badges: start -->
<!-- badges: end -->

The goal of PrePost is to easily assess healthcare events occurring
before and after a defined timepoint.

## Installation

You can install the development version of PrePost from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nicksunderland/PrePost")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PrePost)

x <- PrePost(nhs_number       = c("441", "720", "962"),
             index_event_time = structure(c(164, 164, 165, class = c("POSIXct",  "POSIXt"), tzone = "GMT")),
             window_pre       = 31,
             window_post      = 30, 
             window_units     = "days")

run_descriptives(x)
```