
# ASCATCATA

<!-- badges: start -->
![](https://img.shields.io/badge/Preliminary%20Version-Test-blue.svg)
<!-- badges: end -->

The goal of ASCATCATA is to allow to apply a multivariate ASCA (ANOVA-Simultaneous Component Analysis) on Temporal-Check-All-That-Apply data.

## Installation

You can install the development version of ASCATCATA like so:

``` r
library(devtools)
devtools::install_github("riccim94/ASCATCATA")
```

## Example

The ASCATCATA package offers a main function to apply an ANOVA decomposition
across a TCATA dataset.
The decomposition applied consists in assuming a gaussian distribution after applying an union scale normalization to the interval considered.

``` r
library(ASCATCATA)
library(tempR)
## basic example code
data <- tempr::ojtcata

```

