
# vacuum <a href='https://github.com/sielinski/vacuum'><img src='man/figures/vacuum_hex.png' align="right" height="139" /></a>

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vacuum)](https://cran.r-project.org/package=vacuum)
[![CRAN checks](https://cranchecks.info/badges/worst/vacuum)](https://cran.r-project.org/web/checks/check_results_vacuum.html)
<!-- badges: end -->

Vacuum is an implementation in R of three procedures developed by 
John Tukey: FUNOP (FUll NOrmal Plot), FUNOR-FUNOM 
(FUll NOrmal Rejection-FUll NOrmal Modification), and vacuum cleaner. 
Combined, they provide a way to identify, treat, and analyze outliers 
in two-way (i.e., contingency) tables, as described in his 
landmark paper "The Future of Data Analysis", Tukey, John W. (1962) 
<https://www.jstor.org/stable/2237638>.

## Installation

You can install the released version of vacuum from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("vacuum")
````

You can install the development version, which currently contains only  documentation changes, from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sielinski/vacuum")
```

## Example

funop identifies outliers in a numeric vector:

``` r
library(vacuum)

# example data
dat <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

# outliers flagged as TRUE in the "special" column
funop(dat)

```
