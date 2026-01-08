# Merge (cbind) applied rROC results

Merge (cbind) applied rROC results

## Usage

``` r
merge_applied(
  rroc_applied,
  which_preds = c("all", "all_split", "predictor", "bounded", "keep")
)
```

## Arguments

- rroc_applied:

  A *single outcome* result of apply_restriction()

- which_preds:

  Which predictions should be returned? Default is all. You can also
  specify "all_split" to return a list of predictions for each type of
  prediction (e.g. "predictor", "bounded", "keep"). Or you can specify a
  subset of these types (e.g. c("predictor", "bounded")), then the bound
  predictions including the predictions of only these types will be
  returned.
