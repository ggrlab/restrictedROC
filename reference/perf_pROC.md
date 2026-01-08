# Performance ROC curves (pROC)

Performance ROC curves (pROC)

## Usage

``` r
perf_pROC(
  response,
  predictor,
  quiet = FALSE,
  coords_ret = c("tp", "fp", "tpr", "fpr", "threshold"),
  ...
)
```

## Arguments

- response:

  A vector containing the true class labels. Care that it is VERY
  important which class is the positive class because the *predictions*
  are ordered according to `restriction`

- predictor:

  A vector containing the predictions.

- quiet:

  Parameter of [pROC::roc](https://rdrr.io/pkg/pROC/man/roc.html)

- coords_ret:

  Coordinates from
  [`pROC::coords()`](https://rdrr.io/pkg/pROC/man/coords.html) which
  should be returned in the "perf"-listelement

- ...:

  Further parameters to
  [pROC::roc](https://rdrr.io/pkg/pROC/man/roc.html)

## Value

List of perf: Result of `perf_pROC()`, comes from
[`pROC::coords()`](https://rdrr.io/pkg/pROC/man/coords.html) auc: Area
under the ROC curve, comes from
[`pROC::auc()`](https://rdrr.io/pkg/pROC/man/auc.html) positive_label:
Label of the positive class

## Examples

``` r
data(aSAH, package = "pROC")
restrictedROC:::perf_pROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka
)
#> Setting levels: control = Good, case = Poor
#> Setting direction: controls < cases
#> $perf
#> # A tibble: 110 × 5
#>       tp    fp   tpr   fpr threshold
#>    <dbl> <dbl> <dbl> <dbl>     <dbl>
#>  1    41    72 1     1       -Inf   
#>  2    41    71 1     0.986      3.44
#>  3    40    71 0.976 0.986      4.24
#>  4    40    70 0.976 0.972      4.82
#>  5    40    69 0.976 0.958      5.11
#>  6    39    69 0.951 0.958      5.18
#>  7    38    69 0.927 0.958      5.28
#>  8    37    69 0.902 0.958      5.68
#>  9    37    68 0.902 0.944      6.00
#> 10    37    67 0.902 0.931      6.15
#> # ℹ 100 more rows
#> 
#> $auc
#> [1] 0.611958
#> 
#> $positive_label
#> [1] "Poor"
#> 
restrictedROC:::perf_pROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    levels = c("Poor", "Good"), # response-values for NEGATIVE and POSITIVE
    direction = "<" # predictions of positives are higher than negatives
)
#> $perf
#> # A tibble: 110 × 5
#>       tp    fp   tpr   fpr threshold
#>    <dbl> <dbl> <dbl> <dbl>     <dbl>
#>  1    72    41 1     1       -Inf   
#>  2    71    41 0.986 1          3.44
#>  3    71    40 0.986 0.976      4.24
#>  4    70    40 0.972 0.976      4.82
#>  5    69    40 0.958 0.976      5.11
#>  6    69    39 0.958 0.951      5.18
#>  7    69    38 0.958 0.927      5.28
#>  8    69    37 0.958 0.902      5.68
#>  9    68    37 0.944 0.902      6.00
#> 10    67    37 0.931 0.902      6.15
#> # ℹ 100 more rows
#> 
#> $auc
#> [1] 0.388042
#> 
#> $positive_label
#> [1] "Good"
#> 
restrictedROC:::perf_pROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    levels = c("Poor", "Good"), # response-values for NEGATIVE and POSITIVE
    direction = ">" # predictions of positives are higher than negatives
)
#> $perf
#> # A tibble: 110 × 5
#>       tp    fp   tpr   fpr threshold
#>    <dbl> <dbl> <dbl> <dbl>     <dbl>
#>  1    72    41 1     1         Inf  
#>  2    72    40 1     0.976     250. 
#>  3    71    40 0.986 0.976      76.4
#>  4    71    39 0.986 0.951      65.7
#>  5    70    39 0.972 0.951      56.8
#>  6    70    38 0.972 0.927      52.4
#>  7    69    38 0.958 0.927      48.8
#>  8    69    37 0.958 0.902      47.2
#>  9    68    37 0.944 0.902      44.1
#> 10    67    37 0.931 0.902      40.9
#> # ℹ 100 more rows
#> 
#> $auc
#> [1] 0.611958
#> 
#> $positive_label
#> [1] "Good"
#> 
restrictedROC:::perf_pROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    levels = c("Good", "Poor"), # response-values for NEGATIVE and POSITIVE
    direction = "<" # predictions of positives are higher than negatives
)
#> $perf
#> # A tibble: 110 × 5
#>       tp    fp   tpr   fpr threshold
#>    <dbl> <dbl> <dbl> <dbl>     <dbl>
#>  1    41    72 1     1       -Inf   
#>  2    41    71 1     0.986      3.44
#>  3    40    71 0.976 0.986      4.24
#>  4    40    70 0.976 0.972      4.82
#>  5    40    69 0.976 0.958      5.11
#>  6    39    69 0.951 0.958      5.18
#>  7    38    69 0.927 0.958      5.28
#>  8    37    69 0.902 0.958      5.68
#>  9    37    68 0.902 0.944      6.00
#> 10    37    67 0.902 0.931      6.15
#> # ℹ 100 more rows
#> 
#> $auc
#> [1] 0.611958
#> 
#> $positive_label
#> [1] "Poor"
#> 
```
