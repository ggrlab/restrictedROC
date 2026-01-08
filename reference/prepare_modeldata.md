# Prepare model data

This function prepares the data for the model. It takes the rROC result
and applies the restrictions to the data. It then merges the predictions
into a single data.frame which can be used as input for model building.

## Usage

``` r
prepare_modeldata(x, ...)
```

## Arguments

- x:

  Data which should be prepared. Can be a data.frame or matrix, but must
  have column names. Rows are samples, columns are features.

- ...:

  Arguments passed on to
  [`prepare_modeldata.data.frame`](https://ggrlab.github.io/restrictedROC/reference/prepare_modeldata.data.frame.md)

  `y`

  :   The outcome vector. If NULL, rroc_result must be supplied. If
      given, only rROC() uses it.

  `rroc_result`

  :   The result of rROC(). If NULL, rROC() is called with x, y and
      `...`.

  `rroc_savefile`

  :   If not NULL, the rroc_result is saved to this file.

  `which_preds`

  :   Which predictions should be returned? Default is "bounded". See
      [`merge_applied`](https://ggrlab.github.io/restrictedROC/reference/merge_applied.md)
      for details.

  `removed_impute`

  :   The value to impute for samples outside the informative range.
