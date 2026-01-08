# restricted ROC interpretation

Interpret the results of restriction, e.g. make them nicer to read and
further process.

## Usage

``` r
simple_rROC_interpret(perf_rroc, use_abs_rzAUC = TRUE)
```

## Arguments

- perf_rroc:

  Result from simple_rROC

- use_abs_rzAUC:

  One of the results is the restriction with the maximum rzAUC value. If
  TRUE, report the absolute highest, otherwise the general highest
  including the sign.

## Value

    S3-class restrictedROC of
     - performances
         previously reported perf_rroc result, previously seen as "joined_aucs"
     - global
         AUC, AUC variance under H0, rzAUC and asymptotic pvalue

         when including all samples. This pvalue is now appropriately usable according to
     - keep_highs
         AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold

         Threshold is the restriction value, all other values the corresponding
         performances for the highest (potentially absolute) rzAUC when keeping high
         values (markerHIGH)
     - keep_lows
         AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold

         Threshold is the restriction value, all other values the corresponding
         performances for the highest (potentially absolute) rzAUC when keeping low
         values (markerLOW)
     - max_total
         AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold, part

         Threshold is the restriction value, all other values the corresponding
         performances for the highest (potentially absolute) rzAUC overall.
         This compares "global", "keep_highs" and "keep_lows", therefore `part`
         denotes where the result was coming from.
     - positive_label
         What is the positive label
     - pROC_full
         [pROC::roc()] result of the full dataset, only if `return_pROC` was TRUE

## Examples

``` r
data(aSAH, package = "pROC")
tmp <- simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka
)
#> Positive label not given, setting to last level of factor: Poor
simple_rROC_interpret(tmp)
#> $performances
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
#> $global
#>        auc auc_var_H0    rzAUC pval_asym
#> 1 0.611958   0.611958 1.973565 0.0484312
#> 
#> $keep_highs
#>         auc  auc_var_H0    rzAUC   pval_asym threshold
#> 1 0.6602037 0.003492623 2.710795 0.006712216     5.685
#> 
#> $keep_lows
#>        auc  auc_var_H0    rzAUC pval_asym threshold
#> 1 0.611958 0.003218157 1.973565 0.0484312       Inf
#> 
#> $max_total
#>         auc  auc_var_H0    rzAUC   pval_asym threshold part
#> 1 0.6602037 0.003492623 2.710795 0.006712216     5.685 high
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "restrictedROC" "list"         
simple_rROC_interpret(tmp, use_abs_rzAUC = FALSE)
#> $performances
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
#> $global
#>        auc auc_var_H0    rzAUC pval_asym
#> 1 0.611958   0.611958 1.973565 0.0484312
#> 
#> $keep_highs
#>         auc  auc_var_H0    rzAUC   pval_asym threshold
#> 1 0.6602037 0.003492623 2.710795 0.006712216     5.685
#> 
#> $keep_lows
#>        auc  auc_var_H0    rzAUC pval_asym threshold
#> 1 0.611958 0.003218157 1.973565 0.0484312       Inf
#> 
#> $max_total
#>         auc  auc_var_H0    rzAUC   pval_asym threshold part
#> 1 0.6602037 0.003492623 2.710795 0.006712216     5.685 high
#> 
#> $positive_label
#> [1] "Poor"
#> 
#> attr(,"class")
#> [1] "restrictedROC" "list"         
```
