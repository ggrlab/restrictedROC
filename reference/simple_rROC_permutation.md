# restricted ROC

Calculate the restricted ROC curves.

## Usage

``` r
simple_rROC_permutation(
  response,
  predictor,
  positive_label = NULL,
  direction = "<",
  n_permutations = 100,
  fix_seed = 0,
  parallel_permutations = FALSE,
  return_proc = FALSE,
  verbose = FALSE
)
```

## Arguments

- response:

  A vector containing the true class labels. Care that it is VERY
  important which class is the positive class because the *predictions*
  are ordered according to `restriction`

- predictor:

  A vector containing the predictions.

- positive_label:

  Label for the positive class. All other values of `response` are
  regarded as negative cases.

- direction:

  See [`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html), but only
  "\<" is implemented right now. Maybe changing the positive_label
  already solves your problem.

- n_permutations:

  How many permutations should be done

- fix_seed:

  boolean: If not FALSE, the seed for each permutation will be set by
  set.seed(fix_seed + permutation_i)

- parallel_permutations:

  boolean: If TRUE, the permutation will be done via
  [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html),
  otherwise by [`base::lapply`](https://rdrr.io/r/base/lapply.html)

- return_proc:

  1.  Should pROC::roc() be returned for the full dataset? 2) Should
      pROC::roc() be returned on each of the part datasets? Only works
      with `get_all_aucs_fun=get_all_aucs` after
      `get_all_aucs_norecalculation()` does not calculate the ROC curves
      for each restriction separately.

- verbose:

  Print in which permutation we are

## Value

List of: - Results of
[`simple_rROC_interpret()`](https://ggrlab.github.io/restrictedROC/reference/simple_rROC_interpret.md):
performances, global, keep_highs, keep_lows, max_total, positive_label
"pROC_full": Result of
[`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html) on the full
dataset, calculated once "permutation_pval": Permutation p-values
globally and with optimized ("max") restriction

                pval.twoside.max pval.twoside.global      n_permutations
                            0.16                0.01              100.00
        "perm_max_bound":
            Bound table of the optimal restriction results for all permutations

             # A tibble: 100 × 6
             auc auc_var_H0 rzAUC pval_asym threshold part
             <dbl>      <dbl> <dbl>     <dbl>     <dbl> <chr>
                1 0.161     0.0134  -2.92   0.00345     17.4  high
             2 0.631     0.00635  1.64   0.101       11.7  high
             3 0.332     0.00826 -1.85   0.0644      13.2  high

        "perm_global_bound":
            Bound table of the global AUC (= use all samples) for all permutations

             # A tibble: 100 × 4
                  auc auc_var_H0   rzAUC pval_asym
                <dbl>      <dbl>   <dbl>     <dbl>
              1 0.453      0.453 -0.827      0.408
              2 0.504      0.504  0.0627     0.950

## Examples

``` r
data(aSAH, package = "pROC")
a <- simple_rROC_permutation(
    response = aSAH$outcome,
    predictor = aSAH$ndka
)
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
#> Positive label not given, setting to last level of factor: Poor
```
