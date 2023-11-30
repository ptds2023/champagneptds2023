# champagneptds2023

<!-- badges: start -->
[![R-CMD-check](https://github.com/ptds2023/champagneptds2023/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ptds2023/champagneptds2023/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`champagneptds2023` is a package that models the champagne glass.

## Installation

``` r
# If you don't have 'devtools' installed
install.packages("devtools")

# Install 'champagneptds2023'
devtools::install_github("ptds2023/champagneptds2023")
```

## Usage

```r
library(champagneptds2023)

g(c(1,2))

# Computing the volume of Champagne glass
I_approx <- integrate(f, lower = 10, upper = 20)$value
I <-  4 * (-10 + 22 * log(6) + log(32)) / log(2)
abs(I - I_approx)
```
