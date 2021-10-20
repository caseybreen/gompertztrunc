
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gompertztrunc

## About the package – in progress

`gompertztrunc` is an R package for estimating Gompertz distributions
from doubly-truncated data using maximum likelihood estimation.

To install the package, use:

``` r
install.packages("devtools")
devtools::install_github("caseybreen/gompertztrunc")
```

## TODO

1.  Apply to real data (e.g., BUNMD) or one of Censoc datasets. The
    `gompertz_mle` function seems to work okay on a simple DMF dataset —
    but BUNMD or Numident is a better test.

2.  Get `gompertz_mle` function to work with weights.

3.  More flexible functions for estimating e65 (using MLE gompertz
    parameters OR baseline )

4.  Double-check that code works with both discrete (integer) and
    continuous death ages.

5.  Check if CI is correct. Casey thinks the confidence intervals are
    probably anti-conservative, e.g. coverage ratio of the “95%
    intervals” is probably closer to 70%.

6.  Include way to specify structure of “beta,” e.g. different for men
    and women, or smooth function of cohort or … (Later)

7.  Fixed effects (Later)
