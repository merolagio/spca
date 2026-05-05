
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src = "man/figures/spca_Logo.png" align = "left" height = "139" alt = "spca logo" />

# Package spca

<!-- badges: start -->

[![R-CMD-check](https://github.com/merolagio/spca/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/merolagio/spca/actions/workflows/R-CMD-check.yaml)
<!-- [![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18829364.svg)](https://doi.org/10.5281/zenodo.18829364) -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status:
Active](https://img.shields.io/badge/status-active-success.svg)]()
<!-- badges: end -->

This package contains functions to compute, print and plot Least Squares
Sparse Principal Components Analysis (LS-SPCA)

The main function *spca()* computes the sparse loadings and various
statistics, such as the variance explained by each sparse component
(sPC). print, summery and plot methods are available.

Functions are new.spca (to create an \`spca’ object from a set of
loadings and aggregate_by_scale to visualize the contribution by scale.

## Installation

You can install the development version of spca from
[GitHub](https://github.com/) with:

``` r
# `remotes' is the lightest alternative
install.packages("remotes")
remotes::install_github("merolagio/spca")
#or
#install.packages("devtools")
devtools::install_github("merolagio/spca")
# or
# install.packages("pak")
pak::pak("merolagio/spca")
```

## Example

### load data

`holzinger` is the small classic Holzinger-Swineford dataset with 145
cases on 12 variables.

``` r
library(spca)
## basic example code
data(holzinger)
```

### Decide the number of components

``` r
ho_r = cor(holzinger)
ho_ee = eigen(ho_r)
spca_screeplot(ho_ee$value)
```

![](man/figures/README-pca_checks-1.png)<!-- -->

``` r
wachter_qqplot(ho_ee$values, p = ncol(holzinger), n = nrow(holzinger), nfit_line = -4)
```

![](man/figures/README-pca_checks-2.png)<!-- -->

### Compute PCA

``` r
ho_pca = pca(holzinger, ncomp = 4, screeplot = FALSE, wachter = FALSE)
summary(ho_pca)
#>          sPC1   sPC2   sPC3   sPC4
#> Vexp    40.2%  13.7%  10.6%   6.4%
#> Cvexp   40.2%  53.9%  64.5%  70.9%
#> Rvexp  100.0% 100.0% 100.0% 100.0%
#> Rcvexp 100.0% 100.0% 100.0% 100.0%
#> Card       12     12     12     12
```

### Compute the sparse loadings

Important parameters in the `spca` function are: $`\alpha`$ the minimum
$`R^2`$ or the minimum proportion of cumulative vexp of the PCs
reproduced by the sPCs; *ncomp* the number of components to compute;
*method* the LS-SPCA method to use (“u” for uncorrelated, “c” for
correlated and “p” for projection \[default\]); and *var_selection*
(“stepwise” \[default\], “backward”, or “forward”). see the help for
these parameters and more.

The following command computes four sPCs using forward variable
selection with the cSPCA method so that each sPC as at least 0.95
squared correlation with the corresponding PC ($`r > 0.9`$).

``` r
myspca = spca(holzinger, alpha = 0.95, ncomps = 4)
```

### Inspect the lsspca results

Methods are *print*, *plot* (several options available) and *summary*

``` r
# fig.width = 5, fig.height = 3}
myspca 
#>         sPC1   sPC2   sPC3   sPC4
#> V1     11.9%         13.2%  24.2%
#> V2                   22.1% -20.8%
#> V3     14.2%         17.0%       
#> V4            22.6% -11.9%       
#> V5     19.6%        -17.2%       
#> V6            21.9%              
#> V7     12.2% -21.2% -18.5%  -9.6%
#> V8           -20.3%              
#> V9     12.3% -14.0%         18.6%
#> V10    13.7%                -9.3%
#> V11                        -17.5%
#> V12    16.1%                     
#>        -----  -----  -----  -----
#> Cvexp  38.6%  51.9%  62.3%  68.8%
#> 

summary(myspca)
#>          sPC1   sPC2   sPC3   sPC4
#> Vexp    38.6%  13.3%  10.4%   6.4%
#> Cvexp   38.6%  51.9%  62.3%  68.8%
#> Rvexp   96.0%  97.3%  98.3% 100.6%
#> Rcvexp  96.0%  96.3%  96.7%  97.0%
#> Card        7      5      6      6

plot(myspca, plot_type = "bar")
```

<img src="man/figures/README-methods-1.png" width="60%" /> Other types
of plots are available, circular,

``` r
plot(myspca, plot_type = "circular") # "c" is enough to call "heatmap" type
```

![](man/figures/README-circular-1.png)<!-- -->

Heatmap

``` r
plot(myspca, plot_type = "h") # "h" is enough to call "heatmap" type
```

![](man/figures/README-heatmap-1.png)<!-- -->

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
