
# vacuum

<!-- badges: start -->
<!-- badges: end -->

Vacuum is an implementation of three procedures developed by John Tukey,  FUNOP, FUNOR-FUNOM, and vacuum cleaner, originally published in his paper "The Future of Data Analytics". Combined, the procedures provide a complete solution to identify, treat, and analyze outliers. 

## Installation

You can install the released version of vacuum from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("vacuum")
```

## Example

FUNOP identifies outliers in a numeric vector:

``` r
library(vacuum)

# example data
as_received <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

# index positions of outliers 
FUNOP(as_received)

# values of outliers 
as_received[FUNOP(as_received)]

```

