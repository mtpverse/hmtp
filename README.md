
<!-- README.md is generated from README.Rmd. Please edit that file -->

## hmtp

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/hmtp)](https://CRAN.R-project.org/package=hmtp)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

> Targeted Minimum-Loss Based Estimation for Semi-Continuous Outcomes
> (Hurdle Models).

Nick Williams, Iván Díaz, and Kara Rudolph

------------------------------------------------------------------------

## Installation

The stable, development version can be installed from GitHub with:

``` r
devtools::install_github("mtpverse/hmtp")
```

## Example

``` r
library(hmtp)

hmtp_tmle(hmtp_simdata, "trt", "y", paste0("x.", 1:3), shift = static_binary_on)
                                                                                                       
#> HMTP Estimator: TMLE
#>    Trt. Policy: (static_binary_on)
#> 
#> Population intervention estimate
#>       Estimate: 0.6104
#>     Std. error: 0.0241
#>         95% CI: (0.5631, 0.6577)
```

### Features

| Feature                         | Status |
|---------------------------------|:------:|
| Point treatment                 |   ✓    |
| Modified treatment intervention |   ✓    |
| Static intervention             |   ✓    |
| Dynamic intervention            |   ✓    |
| Continuous treatment            |   ✓    |
| Binary treatment                |   ✓    |
| Categorical treatment           |   ✓    |
| Missingness in treatment        |        |
| Censored outcome                |   ✓    |
| Mediation                       |        |
| Survey weights                  |   ✓    |
| Super learner                   |   ✓    |
| Clustered data                  |   ✓    |
| Parallel processing             |   ✓    |
| Progress bars                   |   ✓    |

## Citation

## References
