# Plot ROC curve with rzAUC values inside

Plot ROC curve with rzAUC values inside

## Usage

``` r
plot_rROC_joint(roc_part, df, part, col_rzAUC)
```

## Arguments

- roc_part:

  data.frame with true positive rate (tpr) and false positive rate (fpr)
  to plot the ROC curve

- df:

  data.frame from single_rROC\$perf where

          single_rROC <- simple_rROC_interpret(simple_rROC(
              response = aSAH$outcome,
              predictor = aSAH$ndka
          ))

- part:

  - "high" (for markerHIGH part, bottom-left part of ROC curve)

  - "low" (for markerLOW part, top-right part of ROC curve)

- col_rzAUC:

  Color of the rzAUC points

## Value

ggplot object with two axes: 1. Usual ROC curve 2. The rzAUC in the same
plot on the second axis

## Examples

``` r
# See plot_rROC()
```
