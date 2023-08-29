# ASCATCATA package

<!-- badges: start -->

![](https://img.shields.io/badge/Preliminary%20Version-Test-red.svg)

<!-- badges: end -->

The package ASCATCATA offers a set of functions to apply, interpret, and report multivariate ASCA (ANOVA-Simultaneous Component Analysis) analysis on Dynamic Sensory Analysis data.

## Installation

You can install the development version of ASCATCATA like so:

``` r
library(devtools)
devtools::install_github("riccim94/ASCATCATA")
```

## The functions available

The ASCATCATA package offers a main function to apply an ASCA decomposition on datasets from dynamic sensory analysis and offers a set of functions to plot, interpret, and validate the results of the analysis. The decomposition applied consists of assuming a Gaussian distribution after applying a unit scale normalization to the interval considered. The decomposition is applied for each combination of unit of time and, when available, sensory attributes asked. 

The package offers a set of function to analyze three different kinds of dynamic sensory analysis:
* asca_ti: To analyze data from Time Intensity (TI) analysis
* asca_tds: To analyze data from Temporal Dominant Sensation (TDS) analysis.
* asca_tcata: To analyze data from Temporal Check All That Apply (TCATA) data.

  ***

### asca_ti
This function applies ASCA decomposition to a Time-intensity (TDS) dataset. It is required that the dataset is put in long format. The decomposition applied is based on the assumption of a normal distribution of the data. Each decomposition is applied to each units of time.  




### asca_tds
This function applies ASCA decomposition to a Temporal Dominant Sensation (TDS) dataset. It is required that the dataset is put in long format. The decomposition applied is based on the assumption of a normal distribution of the data. Each decomposition is applied to each combination of units of time and sensory descriptors.  


``` r
# The first step consists of wrangling the dataset to put it in a long format
# and to mutate in factors the columns "cons" and "samp", and in numeric the column time
data <- tempR::bars
data.long <- data %>% gather( time, CATA, 5:455) %>% mutate(time = str_remove(time, "time_") %>% str_remove(., "s$"))

```
Once the dataset is arranged in long format with a column for the time values and a column for the attribute values, then we apply time-resolved ASCA decomposition on the dataset using the function asca_tds().

``` r
test_tds <- asca_tds(CATA~(sample+assessor)^2, data = data.long, timecol = "time", attributes = "attribute")
```

The results can be reported using the plot_ASCA function.

``` r
plot_ASCA(test_tds)
```
![](Images/plot_tds_1.png)

![](Images/plot_tds_2.png)

![](Images/plot_tds_3.png)

And it is possible to select different arrangements for the depiction of loading values.

### asca_tcata
``` r
library(ASCATCATA)
library(tempR)
library(tidyverse)
## basic example code
```

``` r
# The first step consists of wrangling the dataset to put it in a long format
# and to mutate in factors the columns "cons" and "samp", and in numeric the column time
data <- tempR::ojtcata
data.long <- data %>% gather(time, CATA, 5:25) %>%
mutate(cons = as.factor(cons), samp = as.factor(samp),
time = as.numeric(str_extract(time, "\\d+")))

# then we apply time-resolved ASCA decomposition on the dataset.
```

``` r
ASCA_T1 <- ASCATCATA::asca_tcata(CATA ~ cons+samp, data = data.long, timecol = "time", attributes = "attribute")
```

``` r
# The results can be represented using biplots adopting the plot_ASCA() function.

ASCATCATA::plot_ASCA(ASCA_T1)
```

![](Images/plot_ASCA_1.png)

![](Images/plot_ASCA_2.png)


``` r
# There are multiple display options available to show the loading values

ASCATCATA::plot_ASCA(ASCA_T1, density = T, path = F,, path.smooth = F)
```

![](Images/plot_ASCA_3.png)

![](Images/plot_ASCA_4.png)

``` r
# To estimate the variability along time of the attributes we can use the function plot_time_loadings.

#In its standard formulation, this function plots the contribution during time of each attribut organized by factor or organized by individual attribute.

ASCATCATA::plot_time_loadings(ASCA_T1)
```

![](Images/plot_time_loading_ex_1.png)

![](Images/plot_time_loadings_ex_2.png)

``` r
#The same function can also plot the loading values risolved by time for one axes at time
``` 

![](Images/plot_time_loading_ex_3.png)

![](Images/plot_time_loading_ex_4.png)


``` r
# The function plot_ASCA allows also to apply a hierarchical clustering for the results of the hierarchical clustering and reports the results.

ASCATCATA::plot_ASCA(ASCA_T1, h_clus = 2)
```
![](Images/plot_asca_clus_1.png)

![](Images/plot_asca_clus_2.png)


#### loadings.time.structure
The parameter loadings.time.structure in the function asca_tcata() is set by default to "long". if the input is switched to "short" the multivariate analysis of the factors levels of ASCA will be done with a different arrangement. The loading values estimated will be a single value, and the scores will be estimated for each combination of time units and levels of the factors considered.

``` r
ASCA_T2 <- ASCATCATA::asca_tcata(CATA ~ cons+samp, data = data.long, timecol = "time", attributes = "attribute", loadings.time.structure = "short")
```

The function plot_ASCA() will plot the results adopting a different plot structure to represent properly the results.

``` r
plot_ASCA(ASCA_T2)
```
![](Images/plot_line_1.png)

![](Images/plot_line_2.png)

![](Images/plot_line_3.png)

#### time.quantization


#### residuals versus fitted


#### Sum of squares estimation





## Author

Michele Ricci, Ph.D.
ricci.michele94@gmail.com


