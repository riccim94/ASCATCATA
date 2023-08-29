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
data <- tempR::ojtds
data.long <- data %>% gather( time, CATA, 5:25) %>% mutate(time = str_remove(time, "time_") %>% str_remove(., "s$"))

# then we apply time-resolved ASCA decomposition on the dataset.
```


``` r

```




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
# The results can be represented using biplots

ASCATCATA::plot_ASCA(ASCA_T1)
```

![](Images/plot_ASCA_1.png)

![](Images/plot_ASCA_2.png)


``` r
# There are multiple display options available

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
# The function plot_ASCA allows also to apply a hierarchical clustering for the results of the hierarchical clustering and report the results.

ASCATCATA::plot_ASCA(ASCA_T1, h_clus = 2)
```
![](Images/plot_asca_clus_1.png)

![](Images/plot_asca_clus_2.png)


#### loadings.time.structure


#### time.quantization





## Author

Michele Ricci, Ph.D.
ricci.michele94@gmail.com


