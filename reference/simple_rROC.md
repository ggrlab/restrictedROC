# restricted ROC

Calculate the restricted ROC curves.

## Usage

``` r
simple_rROC(
  response,
  predictor,
  direction = "<",
  positive_label = NULL,
  get_all_aucs_fun = get_all_aucs_norecalculation,
  return_proc = FALSE,
  do_parallel = FALSE,
  check_positive_negative_count = FALSE
)
```

## Arguments

- response:

  A vector containing the true class labels. Care that it is VERY
  important which class is the positive class because the *predictions*
  are ordered according to `restriction`

- predictor:

  A vector containing the predictions.

- direction:

  See [`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html), but only
  "\<" is implemented right now. Maybe changing the positive_label
  already solves your problem.

- positive_label:

  Label for the positive class. All other values of `response` are
  regarded as negative cases.

- get_all_aucs_fun:

  How to calculate the AUCs. You would usually now want to set that.
  Implemented are `get_all_aucs()`: Calculates the AUCs by actively
  splitting the data into markerHIGH and markerLOW parts. Then
  calculates a usual AUC on the parts.

          `get_all_aucs_norecalculation()`:
              Calculates the AUCs based on the scaling factor described in the
              publication. Much faster after the ROC curve does not have to
              be recalculated over and over again.
              Todo: Could potentially be improved by not recalculating the partial
              AUCs with pROC over and over but by just adding parts.

- return_proc:

  1.  Should pROC::roc() be returned for the full dataset? 2) Should
      pROC::roc() be returned on each of the part datasets? Only works
      with `get_all_aucs_fun=get_all_aucs` after
      `get_all_aucs_norecalculation()` does not calculate the ROC curves
      for each restriction separately.

- do_parallel:

  `get_all_aucs()` has parallelization enabled, but for some reason it
  seemed to not improve the speed of the calculation. Therefore throws
  an error. `get_all_aucs_norecalculation()` does not use it at all.

- check_positive_negative_count:

  Pure checking/testing parameter, you would not set that TRUE anytime.
  Just enables checks if the number of positives/negatives was extracted
  correctly for the restrictions

## Value

List of two elements:

    "positive_label": Label of the positive class
    "joined_aucs": Table with the following columns:
     threshold
         The threshold which was used as restriction value.
         "high"-part is always >= threshold
         "low"-part is always   < threshold
     auc_high
         restricted AUC for the high part, including the scaling factor
     positives_high
         How many positives are in the restricted range of high values
     negatives_high
         How many negatives are in the restricted range of high values
     scaling_high
         Scaling factor which is multiplied with the actual partial
         area under the curve to obtain the "recalculated" area under
         the curve if it was RE-calculated on the samples being in the
         restricted range of high values
     auc_var_H0_high
         Estimated variance under the nullhypothesis using
         \deqn{\frac{n_{positives} + n_{negatives} + 1}{12\cdot n_{positives} \cdot n_{negatives}}}
     rzAUC_high
         restricted standardized AUC, obtained via
         \deqn{\frac{auc_high - .5}{\sqrt{auc_var_H0_high}}}
     pval_asym_onesided_high
         Asymptotic, onesided (is AUC bigger) p-value of the restricted standardized AUC,
         obtained via:
         \deqn{1 - pnorm(full_df[["rzAUC"]])}
         Here the requirements are not fullfilled, use with utmost caution!
     pval_asym_high
         Asymptotic, twosided (is AUC different) p-value of the restricted standardized AUC,
         obtained via:
         \deqn{(1 - pnorm(abs(full_df[["rzAUC"]]))) * 2}
         Here the requirements are not fullfilled, use with utmost caution!
     auc_low
         restricted AUC for the low part, including the scaling factor
     positives_low
         How many positives are in the restricted range of low values
     negatives_low
         How many negatives are in the restricted range of low values
     scaling_low
         Scaling factor which is multiplied with the actual partial
         area under the curve to obtain the "recalculated" area under
         the curve if it was RE-calculated on the samples being in the
         restricted range of low values
     auc_var_H0_low
         Estimated variance under the nullhypothesis using
         \deqn{\frac{n_{positives} + n_{negatives} + 1}{12\cdot n_{positives} \cdot n_{negatives}}}
     rzAUC_low
         restricted standardized AUC, obtained via
         \deqn{\frac{auc_low - .5}{\sqrt{auc_var_H0_low}}}
     pval_asym_onesided_low
         Asymptotic, onesided (is AUC bigger) p-value of the restricted standardized AUC,
         obtained via:
         \deqn{1 - pnorm(full_df[["rzAUC"]])}
         Here the requirements are not fullfilled, use with utmost caution!
     pval_asym_low
         Asymptotic, twosided (is AUC different) p-value of the restricted standardized AUC,
         obtained via:
         \deqn{(1 - pnorm(abs(full_df[["rzAUC"]]))) * 2}
         Here the requirements are not fullfilled, use with utmost caution!
     tp
         Number of true positives at that threshold including all samples
     fp
         Number of false positives at that threshold including all samples
     tpr_global
         True positive rate at that threshold including all ("global") samples
     fpr_global
         False positive rate at that threshold including all ("global") samples

## Examples

``` r
data(aSAH, package = "pROC")
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.612             41             72         1            0.00322
#>  2      3.44    0.606             41             71         1.01         0.00323
#>  3      4.24    0.622             40             71         1.04         0.00329
#>  4      4.82    0.616             40             70         1.05         0.00330
#>  5      5.11    0.611             40             69         1.07         0.00332
#>  6      5.18    0.626             39             69         1.10         0.00338
#>  7      5.28    0.643             38             69         1.13         0.00343
#>  8      5.68    0.660             37             69         1.16         0.00349
#>  9      6.00    0.655             37             68         1.17         0.00351
#> 10      6.15    0.650             37             67         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    positive_label = "Poor"
)
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.612             41             72         1            0.00322
#>  2      3.44    0.606             41             71         1.01         0.00323
#>  3      4.24    0.622             40             71         1.04         0.00329
#>  4      4.82    0.616             40             70         1.05         0.00330
#>  5      5.11    0.611             40             69         1.07         0.00332
#>  6      5.18    0.626             39             69         1.10         0.00338
#>  7      5.28    0.643             38             69         1.13         0.00343
#>  8      5.68    0.660             37             69         1.16         0.00349
#>  9      6.00    0.655             37             68         1.17         0.00351
#> 10      6.15    0.650             37             67         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    positive_label = "Good"
)
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.388             72             41         1            0.00322
#>  2      3.44    0.394             71             41         1.01         0.00323
#>  3      4.24    0.378             71             40         1.04         0.00329
#>  4      4.82    0.384             70             40         1.05         0.00330
#>  5      5.11    0.389             69             40         1.07         0.00332
#>  6      5.18    0.374             69             39         1.10         0.00338
#>  7      5.28    0.357             69             38         1.13         0.00343
#>  8      5.68    0.340             69             37         1.16         0.00349
#>  9      6.00    0.345             68             37         1.17         0.00351
#> 10      6.15    0.350             67             37         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Good"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    get_all_aucs_fun = restrictedROC:::get_all_aucs_norecalculation
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.612             41             72         1            0.00322
#>  2      3.44    0.606             41             71         1.01         0.00323
#>  3      4.24    0.622             40             71         1.04         0.00329
#>  4      4.82    0.616             40             70         1.05         0.00330
#>  5      5.11    0.611             40             69         1.07         0.00332
#>  6      5.18    0.626             39             69         1.10         0.00338
#>  7      5.28    0.643             38             69         1.13         0.00343
#>  8      5.68    0.660             37             69         1.16         0.00349
#>  9      6.00    0.655             37             68         1.17         0.00351
#> 10      6.15    0.650             37             67         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    get_all_aucs_fun = restrictedROC:::get_all_aucs
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 19
#>    threshold auc_high positives_high negatives_high auc_var_H0_high rzAUC_high
#>        <dbl>    <dbl>          <int>          <int>           <dbl>      <dbl>
#>  1   -Inf       0.612             41             72         0.00322       1.97
#>  2      3.44    0.606             41             71         0.00323       1.87
#>  3      4.24    0.622             40             71         0.00329       2.12
#>  4      4.82    0.616             40             70         0.00330       2.02
#>  5      5.11    0.611             40             69         0.00332       1.92
#>  6      5.18    0.626             39             69         0.00338       2.17
#>  7      5.28    0.643             38             69         0.00343       2.44
#>  8      5.68    0.660             37             69         0.00349       2.71
#>  9      6.00    0.655             37             68         0.00351       2.62
#> 10      6.15    0.650             37             67         0.00353       2.53
#> # ℹ 100 more rows
#> # ℹ 13 more variables: pval_asym_onesided_high <dbl>, pval_asym_high <dbl>,
#> #   auc_low <dbl>, positives_low <int>, negatives_low <int>,
#> #   auc_var_H0_low <dbl>, rzAUC_low <dbl>, pval_asym_onesided_low <dbl>,
#> #   pval_asym_low <dbl>, tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    get_all_aucs_fun = restrictedROC:::get_all_aucs,
    return_proc = TRUE
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 19
#>    threshold auc_high positives_high negatives_high auc_var_H0_high rzAUC_high
#>        <dbl>    <dbl>          <int>          <int>           <dbl>      <dbl>
#>  1   -Inf       0.612             41             72         0.00322       1.97
#>  2      3.44    0.606             41             71         0.00323       1.87
#>  3      4.24    0.622             40             71         0.00329       2.12
#>  4      4.82    0.616             40             70         0.00330       2.02
#>  5      5.11    0.611             40             69         0.00332       1.92
#>  6      5.18    0.626             39             69         0.00338       2.17
#>  7      5.28    0.643             38             69         0.00343       2.44
#>  8      5.68    0.660             37             69         0.00349       2.71
#>  9      6.00    0.655             37             68         0.00351       2.62
#> 10      6.15    0.650             37             67         0.00353       2.53
#> # ℹ 100 more rows
#> # ℹ 13 more variables: pval_asym_onesided_high <dbl>, pval_asym_high <dbl>,
#> #   auc_low <dbl>, positives_low <int>, negatives_low <int>,
#> #   auc_var_H0_low <dbl>, rzAUC_low <dbl>, pval_asym_onesided_low <dbl>,
#> #   pval_asym_low <dbl>, tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> $pROC_lowpart
#> $pROC_lowpart$`-Inf`
#> NULL
#> 
#> $pROC_lowpart$`3.44`
#> NULL
#> 
#> $pROC_lowpart$`4.24`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 1 controls (part_df[["true"]] FALSE) < 1 cases (part_df[["true"]] TRUE).
#> Area under the curve: 1
#> 
#> $pROC_lowpart$`4.82`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 2 controls (part_df[["true"]] FALSE) < 1 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5
#> 
#> $pROC_lowpart$`5.105`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 1 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3333
#> 
#> $pROC_lowpart$`5.185`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 2 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6667
#> 
#> $pROC_lowpart$`5.28`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 3 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.7778
#> 
#> $pROC_lowpart$`5.685`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.8333
#> 
#> $pROC_lowpart$`6.005`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 4 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.625
#> 
#> $pROC_lowpart$`6.15`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5
#> 
#> $pROC_lowpart$`6.295`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 6 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4167
#> 
#> $pROC_lowpart$`6.345`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 7 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3571
#> 
#> $pROC_lowpart$`6.465`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 8 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3125
#> 
#> $pROC_lowpart$`6.565`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 9 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2778
#> 
#> $pROC_lowpart$`6.69`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.25
#> 
#> $pROC_lowpart$`6.925`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 11 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2273
#> 
#> $pROC_lowpart$`7.24`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 12 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2083
#> 
#> $pROC_lowpart$`7.525`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 12 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3667
#> 
#> $pROC_lowpart$`7.645`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 13 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3385
#> 
#> $pROC_lowpart$`7.705`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 14 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3143
#> 
#> $pROC_lowpart$`7.855`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 15 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2933
#> 
#> $pROC_lowpart$`7.99`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 16 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.275
#> 
#> $pROC_lowpart$`8.055`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 17 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2588
#> 
#> $pROC_lowpart$`8.16`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 18 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.2444
#> 
#> $pROC_lowpart$`8.305`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 18 controls (part_df[["true"]] FALSE) < 6 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.3704
#> 
#> $pROC_lowpart$`8.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 18 controls (part_df[["true"]] FALSE) < 7 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4603
#> 
#> $pROC_lowpart$`8.535`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 19 controls (part_df[["true"]] FALSE) < 7 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4361
#> 
#> $pROC_lowpart$`8.72`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 20 controls (part_df[["true"]] FALSE) < 7 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4143
#> 
#> $pROC_lowpart$`8.955`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 20 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4875
#> 
#> $pROC_lowpart$`9.225`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 21 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4643
#> 
#> $pROC_lowpart$`9.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 23 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4239
#> 
#> $pROC_lowpart$`9.52`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 24 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4062
#> 
#> $pROC_lowpart$`9.6`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 24 controls (part_df[["true"]] FALSE) < 9 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4722
#> 
#> $pROC_lowpart$`9.665`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 25 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.506
#> 
#> $pROC_lowpart$`9.75`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 26 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4865
#> 
#> $pROC_lowpart$`9.805`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 27 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4685
#> 
#> $pROC_lowpart$`9.82`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 28 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4518
#> 
#> $pROC_lowpart$`9.84`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 29 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4362
#> 
#> $pROC_lowpart$`9.9`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 29 controls (part_df[["true"]] FALSE) < 11 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4875
#> 
#> $pROC_lowpart$`10.14`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 30 controls (part_df[["true"]] FALSE) < 11 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4712
#> 
#> $pROC_lowpart$`10.365`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 31 controls (part_df[["true"]] FALSE) < 11 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.456
#> 
#> $pROC_lowpart$`10.41`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 31 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5013
#> 
#> $pROC_lowpart$`10.465`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 32 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4857
#> 
#> $pROC_lowpart$`10.53`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 33 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.471
#> 
#> $pROC_lowpart$`10.575`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 34 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4571
#> 
#> $pROC_lowpart$`10.715`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 35 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.444
#> 
#> $pROC_lowpart$`10.95`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 36 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4317
#> 
#> $pROC_lowpart$`11.08`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 37 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.42
#> 
#> $pROC_lowpart$`11.345`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 37 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4647
#> 
#> $pROC_lowpart$`11.635`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 37 controls (part_df[["true"]] FALSE) < 14 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5029
#> 
#> $pROC_lowpart$`11.675`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 37 controls (part_df[["true"]] FALSE) < 15 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.536
#> 
#> $pROC_lowpart$`11.7`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 38 controls (part_df[["true"]] FALSE) < 15 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5219
#> 
#> $pROC_lowpart$`11.725`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 38 controls (part_df[["true"]] FALSE) < 16 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5518
#> 
#> $pROC_lowpart$`11.85`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 39 controls (part_df[["true"]] FALSE) < 16 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5377
#> 
#> $pROC_lowpart$`12.095`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 40 controls (part_df[["true"]] FALSE) < 16 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5242
#> 
#> $pROC_lowpart$`12.375`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 40 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5522
#> 
#> $pROC_lowpart$`12.55`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 41 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5387
#> 
#> $pROC_lowpart$`12.58`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 42 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5259
#> 
#> $pROC_lowpart$`12.63`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 43 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5137
#> 
#> $pROC_lowpart$`12.69`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 44 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.502
#> 
#> $pROC_lowpart$`12.73`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 45 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4908
#> 
#> $pROC_lowpart$`12.775`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 45 controls (part_df[["true"]] FALSE) < 18 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5191
#> 
#> $pROC_lowpart$`12.85`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 46 controls (part_df[["true"]] FALSE) < 18 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5079
#> 
#> $pROC_lowpart$`12.94`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 47 controls (part_df[["true"]] FALSE) < 19 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.523
#> 
#> $pROC_lowpart$`13.05`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 48 controls (part_df[["true"]] FALSE) < 19 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5121
#> 
#> $pROC_lowpart$`13.16`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 49 controls (part_df[["true"]] FALSE) < 19 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5016
#> 
#> $pROC_lowpart$`13.305`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 49 controls (part_df[["true"]] FALSE) < 20 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5265
#> 
#> $pROC_lowpart$`13.43`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 50 controls (part_df[["true"]] FALSE) < 20 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.516
#> 
#> $pROC_lowpart$`13.505`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 51 controls (part_df[["true"]] FALSE) < 20 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5059
#> 
#> $pROC_lowpart$`13.615`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 51 controls (part_df[["true"]] FALSE) < 21 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5294
#> 
#> $pROC_lowpart$`13.77`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 51 controls (part_df[["true"]] FALSE) < 22 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5508
#> 
#> $pROC_lowpart$`13.955`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 52 controls (part_df[["true"]] FALSE) < 22 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5402
#> 
#> $pROC_lowpart$`14.15`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 52 controls (part_df[["true"]] FALSE) < 23 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5602
#> 
#> $pROC_lowpart$`14.3`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 52 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5785
#> 
#> $pROC_lowpart$`14.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 53 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5676
#> 
#> $pROC_lowpart$`15.055`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 54 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5571
#> 
#> $pROC_lowpart$`15.715`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 55 controls (part_df[["true"]] FALSE) < 25 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5647
#> 
#> $pROC_lowpart$`15.925`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 55 controls (part_df[["true"]] FALSE) < 26 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5815
#> 
#> $pROC_lowpart$`16.035`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 55 controls (part_df[["true"]] FALSE) < 27 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.597
#> 
#> $pROC_lowpart$`16.66`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 56 controls (part_df[["true"]] FALSE) < 27 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5863
#> 
#> $pROC_lowpart$`17.255`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 57 controls (part_df[["true"]] FALSE) < 27 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.576
#> 
#> $pROC_lowpart$`17.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 58 controls (part_df[["true"]] FALSE) < 27 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5661
#> 
#> $pROC_lowpart$`17.63`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 58 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5816
#> 
#> $pROC_lowpart$`18.035`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 59 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5717
#> 
#> $pROC_lowpart$`18.835`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 60 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5622
#> 
#> $pROC_lowpart$`20.105`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 61 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.553
#> 
#> $pROC_lowpart$`20.985`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5441
#> 
#> $pROC_lowpart$`21.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5598
#> 
#> $pROC_lowpart$`21.525`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 30 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5745
#> 
#> $pROC_lowpart$`21.75`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5882
#> 
#> $pROC_lowpart$`22.1`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 32 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6011
#> 
#> $pROC_lowpart$`22.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 63 controls (part_df[["true"]] FALSE) < 32 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5915
#> 
#> $pROC_lowpart$`22.53`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 64 controls (part_df[["true"]] FALSE) < 32 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5823
#> 
#> $pROC_lowpart$`23.605`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 64 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5949
#> 
#> $pROC_lowpart$`25.885`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 65 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5858
#> 
#> $pROC_lowpart$`27.84`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 66 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5769
#> 
#> $pROC_lowpart$`30.43`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5683
#> 
#> $pROC_lowpart$`32.39`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 34 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.581
#> 
#> $pROC_lowpart$`33.235`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 35 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.593
#> 
#> $pROC_lowpart$`37.2`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6043
#> 
#> $pROC_lowpart$`40.885`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.615
#> 
#> $pROC_lowpart$`44.13`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 68 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6059
#> 
#> $pROC_lowpart$`47.22`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5971
#> 
#> $pROC_lowpart$`48.775`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 38 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6077
#> 
#> $pROC_lowpart$`52.38`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 70 controls (part_df[["true"]] FALSE) < 38 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5991
#> 
#> $pROC_lowpart$`56.825`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 70 controls (part_df[["true"]] FALSE) < 39 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6093
#> 
#> $pROC_lowpart$`65.7`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 71 controls (part_df[["true"]] FALSE) < 39 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6008
#> 
#> $pROC_lowpart$`76.435`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 71 controls (part_df[["true"]] FALSE) < 40 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6107
#> 
#> $pROC_lowpart$`249.745`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 72 controls (part_df[["true"]] FALSE) < 40 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6023
#> 
#> $pROC_lowpart$`Inf`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 72 controls (part_df[["true"]] FALSE) < 41 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.612
#> 
#> 
#> $pROC_highpart
#> $pROC_highpart$`-Inf`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 72 controls (part_df[["true"]] FALSE) < 41 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.612
#> 
#> $pROC_highpart$`3.44`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 71 controls (part_df[["true"]] FALSE) < 41 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6065
#> 
#> $pROC_highpart$`4.24`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 71 controls (part_df[["true"]] FALSE) < 40 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6217
#> 
#> $pROC_highpart$`4.82`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 70 controls (part_df[["true"]] FALSE) < 40 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6162
#> 
#> $pROC_highpart$`5.105`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 40 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6107
#> 
#> $pROC_highpart$`5.185`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 39 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6263
#> 
#> $pROC_highpart$`5.28`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 38 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6428
#> 
#> $pROC_highpart$`5.685`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 69 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6602
#> 
#> $pROC_highpart$`6.005`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 68 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6552
#> 
#> $pROC_highpart$`6.15`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 67 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6501
#> 
#> $pROC_highpart$`6.295`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 66 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6448
#> 
#> $pROC_highpart$`6.345`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 65 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6393
#> 
#> $pROC_highpart$`6.465`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 64 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6337
#> 
#> $pROC_highpart$`6.565`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 63 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6278
#> 
#> $pROC_highpart$`6.69`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 62 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6218
#> 
#> $pROC_highpart$`6.925`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 61 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6156
#> 
#> $pROC_highpart$`7.24`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 60 controls (part_df[["true"]] FALSE) < 37 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6092
#> 
#> $pROC_highpart$`7.525`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 60 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6262
#> 
#> $pROC_highpart$`7.645`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 59 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6198
#> 
#> $pROC_highpart$`7.705`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 58 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6133
#> 
#> $pROC_highpart$`7.855`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 57 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6065
#> 
#> $pROC_highpart$`7.99`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 56 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5995
#> 
#> $pROC_highpart$`8.055`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 55 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5922
#> 
#> $pROC_highpart$`8.16`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 54 controls (part_df[["true"]] FALSE) < 36 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5846
#> 
#> $pROC_highpart$`8.305`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 54 controls (part_df[["true"]] FALSE) < 35 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6013
#> 
#> $pROC_highpart$`8.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 54 controls (part_df[["true"]] FALSE) < 34 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.619
#> 
#> $pROC_highpart$`8.535`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 53 controls (part_df[["true"]] FALSE) < 34 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6118
#> 
#> $pROC_highpart$`8.72`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 52 controls (part_df[["true"]] FALSE) < 34 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6044
#> 
#> $pROC_highpart$`8.955`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 52 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6227
#> 
#> $pROC_highpart$`9.225`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 51 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6153
#> 
#> $pROC_highpart$`9.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 49 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5996
#> 
#> $pROC_highpart$`9.52`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 48 controls (part_df[["true"]] FALSE) < 33 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5912
#> 
#> $pROC_highpart$`9.6`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 48 controls (part_df[["true"]] FALSE) < 32 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6097
#> 
#> $pROC_highpart$`9.665`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 47 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6211
#> 
#> $pROC_highpart$`9.75`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 46 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6129
#> 
#> $pROC_highpart$`9.805`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 45 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6043
#> 
#> $pROC_highpart$`9.82`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 44 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5953
#> 
#> $pROC_highpart$`9.84`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 43 controls (part_df[["true"]] FALSE) < 31 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5859
#> 
#> $pROC_highpart$`9.9`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 43 controls (part_df[["true"]] FALSE) < 30 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6054
#> 
#> $pROC_highpart$`10.14`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 42 controls (part_df[["true"]] FALSE) < 30 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.596
#> 
#> $pROC_highpart$`10.365`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 41 controls (part_df[["true"]] FALSE) < 30 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5862
#> 
#> $pROC_highpart$`10.41`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 41 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6064
#> 
#> $pROC_highpart$`10.465`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 40 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5966
#> 
#> $pROC_highpart$`10.53`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 39 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5862
#> 
#> $pROC_highpart$`10.575`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 38 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5753
#> 
#> $pROC_highpart$`10.715`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 37 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5638
#> 
#> $pROC_highpart$`10.95`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 36 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5517
#> 
#> $pROC_highpart$`11.08`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 35 controls (part_df[["true"]] FALSE) < 29 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5389
#> 
#> $pROC_highpart$`11.345`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 35 controls (part_df[["true"]] FALSE) < 28 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5582
#> 
#> $pROC_highpart$`11.635`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 35 controls (part_df[["true"]] FALSE) < 27 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5788
#> 
#> $pROC_highpart$`11.675`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 35 controls (part_df[["true"]] FALSE) < 26 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6011
#> 
#> $pROC_highpart$`11.7`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 34 controls (part_df[["true"]] FALSE) < 26 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5894
#> 
#> $pROC_highpart$`11.725`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 34 controls (part_df[["true"]] FALSE) < 25 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6129
#> 
#> $pROC_highpart$`11.85`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 33 controls (part_df[["true"]] FALSE) < 25 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6012
#> 
#> $pROC_highpart$`12.095`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 32 controls (part_df[["true"]] FALSE) < 25 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5888
#> 
#> $pROC_highpart$`12.375`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 32 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6133
#> 
#> $pROC_highpart$`12.55`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 31 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6008
#> 
#> $pROC_highpart$`12.58`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 30 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5875
#> 
#> $pROC_highpart$`12.63`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 29 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5733
#> 
#> $pROC_highpart$`12.69`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 28 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.558
#> 
#> $pROC_highpart$`12.73`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 27 controls (part_df[["true"]] FALSE) < 24 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5417
#> 
#> $pROC_highpart$`12.775`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 27 controls (part_df[["true"]] FALSE) < 23 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5652
#> 
#> $pROC_highpart$`12.85`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 26 controls (part_df[["true"]] FALSE) < 23 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5485
#> 
#> $pROC_highpart$`12.94`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 25 controls (part_df[["true"]] FALSE) < 22 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5555
#> 
#> $pROC_highpart$`13.05`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 24 controls (part_df[["true"]] FALSE) < 22 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5369
#> 
#> $pROC_highpart$`13.16`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 23 controls (part_df[["true"]] FALSE) < 22 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5168
#> 
#> $pROC_highpart$`13.305`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 23 controls (part_df[["true"]] FALSE) < 21 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5414
#> 
#> $pROC_highpart$`13.43`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 22 controls (part_df[["true"]] FALSE) < 21 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5206
#> 
#> $pROC_highpart$`13.505`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 21 controls (part_df[["true"]] FALSE) < 21 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4977
#> 
#> $pROC_highpart$`13.615`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 21 controls (part_df[["true"]] FALSE) < 20 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5226
#> 
#> $pROC_highpart$`13.77`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 21 controls (part_df[["true"]] FALSE) < 19 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5501
#> 
#> $pROC_highpart$`13.955`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 20 controls (part_df[["true"]] FALSE) < 19 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5276
#> 
#> $pROC_highpart$`14.15`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 20 controls (part_df[["true"]] FALSE) < 18 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5569
#> 
#> $pROC_highpart$`14.3`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 20 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5897
#> 
#> $pROC_highpart$`14.455`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 19 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5681
#> 
#> $pROC_highpart$`15.055`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 18 controls (part_df[["true"]] FALSE) < 17 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5441
#> 
#> $pROC_highpart$`15.715`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 17 controls (part_df[["true"]] FALSE) < 16 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5515
#> 
#> $pROC_highpart$`15.925`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 17 controls (part_df[["true"]] FALSE) < 15 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5882
#> 
#> $pROC_highpart$`16.035`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 17 controls (part_df[["true"]] FALSE) < 14 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6303
#> 
#> $pROC_highpart$`16.66`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 16 controls (part_df[["true"]] FALSE) < 14 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6071
#> 
#> $pROC_highpart$`17.255`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 15 controls (part_df[["true"]] FALSE) < 14 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.581
#> 
#> $pROC_highpart$`17.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 14 controls (part_df[["true"]] FALSE) < 14 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.551
#> 
#> $pROC_highpart$`17.63`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 14 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5934
#> 
#> $pROC_highpart$`18.035`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 13 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5621
#> 
#> $pROC_highpart$`18.835`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 12 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5256
#> 
#> $pROC_highpart$`20.105`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 11 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4825
#> 
#> $pROC_highpart$`20.985`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 13 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4308
#> 
#> $pROC_highpart$`21.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 12 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4667
#> 
#> $pROC_highpart$`21.525`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 11 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5091
#> 
#> $pROC_highpart$`21.75`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 10 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.56
#> 
#> $pROC_highpart$`22.1`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 10 controls (part_df[["true"]] FALSE) < 9 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6222
#> 
#> $pROC_highpart$`22.35`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 9 controls (part_df[["true"]] FALSE) < 9 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5802
#> 
#> $pROC_highpart$`22.53`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 8 controls (part_df[["true"]] FALSE) < 9 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5278
#> 
#> $pROC_highpart$`23.605`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 8 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5938
#> 
#> $pROC_highpart$`25.885`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 7 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5357
#> 
#> $pROC_highpart$`27.84`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 6 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4583
#> 
#> $pROC_highpart$`30.43`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 8 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.35
#> 
#> $pROC_highpart$`32.39`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 7 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4
#> 
#> $pROC_highpart$`33.235`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 6 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.4667
#> 
#> $pROC_highpart$`37.2`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 5 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.56
#> 
#> $pROC_highpart$`40.885`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 5 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.7
#> 
#> $pROC_highpart$`44.13`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 4 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.625
#> 
#> $pROC_highpart$`47.22`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 4 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5
#> 
#> $pROC_highpart$`48.775`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 3 controls (part_df[["true"]] FALSE) < 3 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.6667
#> 
#> $pROC_highpart$`52.38`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 2 controls (part_df[["true"]] FALSE) < 3 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5
#> 
#> $pROC_highpart$`56.825`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 2 controls (part_df[["true"]] FALSE) < 2 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.75
#> 
#> $pROC_highpart$`65.7`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 1 controls (part_df[["true"]] FALSE) < 2 cases (part_df[["true"]] TRUE).
#> Area under the curve: 0.5
#> 
#> $pROC_highpart$`76.435`
#> 
#> Call:
#> roc.default(response = part_df[["true"]], predictor = part_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: part_df[["pred"]] in 1 controls (part_df[["true"]] FALSE) < 1 cases (part_df[["true"]] TRUE).
#> Area under the curve: 1
#> 
#> $pROC_highpart$`249.745`
#> NULL
#> 
#> $pROC_highpart$`Inf`
#> NULL
#> 
#> 
#> $pROC_full
#> 
#> Call:
#> roc.default(response = true_pred_df[["true"]], predictor = true_pred_df[["pred"]],     levels = c(FALSE, TRUE), direction = direction)
#> 
#> Data: true_pred_df[["pred"]] in 72 controls (true_pred_df[["true"]] FALSE) < 41 cases (true_pred_df[["true"]] TRUE).
#> Area under the curve: 0.612
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    check_positive_negative_count = TRUE
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.612             41             72         1            0.00322
#>  2      3.44    0.606             41             71         1.01         0.00323
#>  3      4.24    0.622             40             71         1.04         0.00329
#>  4      4.82    0.616             40             70         1.05         0.00330
#>  5      5.11    0.611             40             69         1.07         0.00332
#>  6      5.18    0.626             39             69         1.10         0.00338
#>  7      5.28    0.643             38             69         1.13         0.00343
#>  8      5.68    0.660             37             69         1.16         0.00349
#>  9      6.00    0.655             37             68         1.17         0.00351
#> 10      6.15    0.650             37             67         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    do_parallel = TRUE
)
#> Positive label not given, setting to last level of factor: Poor
#> $joined_aucs
#> # A tibble: 110 × 21
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.612             41             72         1            0.00322
#>  2      3.44    0.606             41             71         1.01         0.00323
#>  3      4.24    0.622             40             71         1.04         0.00329
#>  4      4.82    0.616             40             70         1.05         0.00330
#>  5      5.11    0.611             40             69         1.07         0.00332
#>  6      5.18    0.626             39             69         1.10         0.00338
#>  7      5.28    0.643             38             69         1.13         0.00343
#>  8      5.68    0.660             37             69         1.16         0.00349
#>  9      6.00    0.655             37             68         1.17         0.00351
#> 10      6.15    0.650             37             67         1.19         0.00353
#> # ℹ 100 more rows
#> # ℹ 15 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   pval_asym_high <dbl>, auc_low <dbl>, positives_low <dbl>,
#> #   negatives_low <dbl>, scaling_low <dbl>, auc_var_H0_low <dbl>,
#> #   rzAUC_low <dbl>, pval_asym_onesided_low <dbl>, pval_asym_low <dbl>,
#> #   tp <dbl>, fp <dbl>, tpr_global <dbl>, fpr_global <dbl>
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "simple_rROC" "list"       
if (FALSE) { # \dontrun{
simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    get_all_aucs_fun = restrictedROC:::get_all_aucs,
    do_parallel = TRUE
)
} # }
```
