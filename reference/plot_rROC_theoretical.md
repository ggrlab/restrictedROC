# Plot (restricted) ROC curves for theoretic distributions

Plot (restricted) ROC curves for theoretic distributions

## Usage

``` r
plot_rROC_theoretical(
  qnorm_positive,
  qnorm_negative,
  length.out_densityplot = 500,
  n_positive = 500,
  n_negative = 500,
  return_all = FALSE,
  ...
)
```

## Arguments

- qnorm_positive:

  Quantile distribution function of positive class

- qnorm_negative:

  Quantile distribution function of negative class

- length.out_densityplot:

  Number of points to draw the density plot per curve

- n_positive:

  Number of positive samples.

- n_negative:

  Number of negative samples.

- return_all:

  If true, also return the rROC result, not only the plots

- ...:

  Further arguments to
  [`plot_density_rROC_empirical()`](https://ggrlab.github.io/restrictedROC/reference/plot_density_rROC_empirical.md)

## Value

If return_all: List of "data" and "rroc" result Else: Only the "plots"
from "rroc" result

## Examples

``` r
plot_rROC_theoretical(
    qnorm_positive = function(x) qnorm(x, mean = 1, sd = 1),
    qnorm_negative = function(x) qnorm(x, mean = 0, sd = 1)
)
#> Ignoring unknown labels:
#> • colour : ""
```
