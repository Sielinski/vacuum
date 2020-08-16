
# vacuum

<!-- badges: start -->
<!-- badges: end -->

Vacuum is an implementation in R of three procedures developed by 
John Tukey: FUNOP, FUNOR-FUNOM, and vacuum cleaner. Combined, they 
provide a way to identify, treat, and analyze outliers in two-way 
(i.e., contingency) tables, as described in Tukey's landmark paper 
"The Future of Data Analytics". 

## Installation

You can install the released version of vacuum from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("vacuum")
```

## Example

funop identifies outliers in a numeric vector:

``` r
library(vacuum)

# example data
dat <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

# index positions of outliers 
funop(dat)

# values of outliers 
dat[funop(dat)]

```
