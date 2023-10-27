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

## The package

The ASCATCATA package offers a main function to apply an ASCA decomposition on datasets from dynamic sensory analysis. It offers a set of functions to plot, interpret, and validate the analysis results.

ASCA analysis (Smilde et al. 2012) consists of applying the same general linear model (GLM) decomposition for each variable of the dataset at the time. In the present case, to deal properly with time-resolved datasets, the decomposition is applied for each combination of time unit and parameter available.
The glm decomposition is applied without the application of a link function and assuming a Gaussian distribution of the residuals. The decomposition is applied to data after a unit scale normalization step.
From the decomposition applied to each sub-unit of the dataset, it is possible to estimate a coefficient for each level of all the factors included in the glm model.

The second step of the ASCA framework consists of the construction of *n* matrices of dimension [*k*,*m*], where *n* is the number of factors included in the model, *k* is the number of levels of each factor respectively and *m* is the total number of the glm decomposition applied.
Each matrix is treated adopting a matrix decomposition based on SVD and the final values are analyzed as a Principal Component Analysis (PCA), assigning score values to the levels of the factor considered, loading values to the combination between time units and sensory descriptors. 

The information obtained from the ASCA framework consists of a multivariate comparison between the levels of a single factor, highlighting similarities and differences between multiple measurements of an experiment using the scores values. Furthermore, the analysis estimates also how each variable varies in relation to each factor taking into account possible interaction effects.

In the context of dynamic sensory data, the ASCA framework proposed can be used for:
* The comparison of different conditions (i.e. two different foods, the behavior of the judges during the whole test, the presence or the absence of an ingredient in the same product), obtaining an individual score value estimated considering the whole time-series datasets available. The analysis reports also how the parameters (in the case of dynamic sensory analysis the sensory descriptors) vary over time according to the factors considered.
* The comparison of different conditions during time at multivariate levels, obtaining a time-resolved score value for each level of the factor considered. The analysis report also the loadings values that define how sensory descriptors vary.
* After a bootstrap test, the estimation of confidence intervals allows the estimation of significant differences between conditions at the multivariate level from ASCA models, and to estimate which loading values are significantly different from zero, to define the most important parameters that cause those differences.
* The multivariate distribution of residuals is an indicator of the significance of the model applied.
* The aggregate distribution of Sum of Squares values in ASCA models is an indicator of the explained variance by each factor included in the model and can be associated with the power of the effect analyzed.

In the present work, the ASCA framework is proposed as a reliable tool to infer practical information from dynamic sensory datasets allowing the sensory researcher to extract summarizing information from structured datasets considering the issues related to the autocorrelated structure of time series and the effects of confounding factors.

 ***

## Functions available

The package offers a set of functions to analyze three different kinds of dynamic sensory analysis:
* asca_ti: To analyze data from Time Intensity (TI) analysis
* asca_tds: To analyze data from Temporal Dominant Sensation (TDS) analysis.
* asca_tcata: To analyze data from Temporal Check All That Apply (TCATA) data.


### asca_ti
This function applies ASCA decomposition to a Time-Intensity (TI) dataset. It is required that the dataset is put in a long format. The decomposition applied is based on the assumption of a normal data distribution. Each decomposition is applied to each unit of time.  

``` r
# An open Time Intensity dataset is taken from the website https://help.xlstat.com/dataset/time-intensity-data_0.xlsm
library(readxl)
library(httr)
library(tidyverse)
url1 <- "https://help.xlstat.com/dataset/time-intensity-data_0.xlsm"

GET(url1, write_disk(tf <- tempfile(fileext = ".xlsm")))
tf <- str_replace_all(tf, "\\\\", "//")
data <- read_excel(tf)
data.long <- data %>% gather(time, intensity, 6:36) %>% filter(time != "0") %>% droplevels()
#the function is then applied on the dataset, defining the formula applied by the ASCA decomposition and the column containing the time column
test_ti <- asca_ti(intensity ~ PANELIST+PRODUCT, data = data.long, timecol = "time")
```
The package also offers two functions that report the reesults of ASCA analysis using visual graphical devices. The first is plot_ASCA() function

``` r
plot_ASCA(test_ti)
```
![](Images/plot_ti_1.png)



The second is plot_time_loadings().
``` r
plot_time_loadings(test_ti)
```
![](Images/plot_ti_2.png)


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

It is possible to select different arrangements for the depiction of loading values specifying the different parameters of the plot_ASCA() function. By defining a number or the name of the factor in **object** it is possible to select which plot will be printed. Specifying the logical value of **density**, **path**, and **path.smooth** is possible to modify the aestethic of the plot.

``` r
plot_ASCA(test_tds, object = 1, density = T, path = F, path.smooth = F)

```
![](Images/ASCA_plot_tds_density.png)

``` r
test_tds_short <- asca_tds(CATA~(sample+assessor)^2, data = data.long, timecol = "time", attributes = "attribute", loadings.time.structure = "short")

plot_ASCA(test_tds_short)

```

![](Images/ASCA_plot_tds_s_1.png)

![](Images/ASCA_plot_tds_s_2.png)

![](Images/ASCA_plot_tds_s_3.png)

It is possible to plot the contribution of each attribute on the overall variability for each factor adopting the function plot_time_loadings()

``` r
plot_time_loadings(test_tds)
```

![](Images/tds_time_1.png)

![](Images/tds_time_2.png)

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
#The same function can also plot the loading values risolved by time for one axes at time.
ASCATCATA::plot_time_loadings(ASCA_T1, choice = "loadings")

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
The time.quantization parameter allows the quantization of the time units. The number reported consists in defying the number of time units which are required to be incorporated in singular time units. All the other operations would be the same, and the functions plot_ASCA() and plot_time_loadings() would function in the same way, reporting values for aggregated estimation of ASCA decomposition

#### residuals versus fitted
In the object is reported data.frame containing all the residuals and fitted values estimated by each ANOVA decomposition applied and the correspondent time unit and attribute.

#### Sum of squares estimation
In the object is reported data.frame containing the percentage of the Sum of Squares attributed to each factor and to residuals at aggregate levels.

``` r
ASCA_T1$SS_decomposition
```

Factor | Sum_Sq 
--- | ---
Residuals | 0.687 
cons | 0.239 
samp | 0.0740 



## Author

Michele Ricci, Ph.D.
ricci.michele94@gmail.com

## Bibliography

A.K. Smilde, M.E. Timmerman, M.M.W.B. Hendriks, J.J. Jansen, H.C.J. Hoefsloot. 2012, Generic framework for high-dimensional fixed-effects ANOVA Briefings in Bioinformatics, 13 (5), pp. 524-535, 10.1093/bib/bbr071
