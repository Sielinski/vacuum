
# vacuum

<!-- badges: start -->
<!-- badges: end -->

Vacuum is an implementation in R of three procedures developed by 
John Tukey: FUNOP (FUll NOrmal Plot), FUNOR-FUNOM 
(FUll NOrmal Rejection-FUll NOrmal Modification), and vacuum cleaner. 
Combined, they provide a way to identify, treat, and analyze outliers 
in two-way (i.e., contingency) tables, as described in his 
landmark paper "The Future of Data Analysis", Tukey, John W. (1962) 
<https://www.jstor.org/stable/2237638>.

## Installation

I'll release this on [CRAN](https://CRAN.R-project.org) once they begin 
accepting packages again (after their summer hiatus). In the meantime, you can try:

``` r
devtools::install_github("sielinski/vacuum")
```

## Example

funop identifies outliers in a numeric vector:

``` r
library(vacuum)

# example data
dat <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

# outliers flagged as TRUE in the special column
funop(dat)

```
