# Train a predictive model using (rROC-preprocessed) data

A convenience function to train a model using the data x_prepared to
predict the outcome y. Per default, the function is a wrapper around the
h2o.randomForest function.

## Usage

``` r
train_rROC_h2o(
  x_prepared,
  y,
  init_h2o = TRUE,
  h2o_trainfun = function(df, col_y, cols_x, ...) {
    
    h2o::h2o.randomForest(training_frame = df, y = col_y, x = cols_x, ntrees = 1000,
    max_depth = 20, min_rows = 1, nbins = 20, seed = 4242, ...)
 },
  ...
)
```

## Arguments

- x_prepared:

  A data frame containing the preprocessed data to use in the model. The
  data frame should not contain the outcome variable. Usually the result
  of a call to
  [`prepare_modeldata`](https://ggrlab.github.io/restrictedROC/reference/prepare_modeldata.md).

- y:

  The outcome variable. Usually a factor.

- init_h2o:

  Whether to initialize h2o. Defaults to TRUE.

- h2o_trainfun:

  The function to use to train the model. Defaults to `h2o.randomForest`
  with some default parameters. Needs to have `df`, `col_y` and `cols_x`
  as arguments where `df` is the data frame to use, `col_y` is the
  column index of the outcome variable and `cols_x` is a vector of
  column indices of the features to use to predict the outcome.

- ...:

  Arguments passed to h2o_trainfun.
