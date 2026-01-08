# Predict new data with a trained model

A convenience function to predict new data with a trained model. The
function is a wrapper around the h2o.predict function. It returns a list
with the predictions and optionally the metrics.

## Usage

``` r
predict_rROC_h2o(
  h2o_model,
  x_prepared,
  y,
  init_h2o = FALSE,
  calculate_metrics = TRUE,
  sample_split = NULL
)
```

## Arguments

- h2o_model:

  The model to use for prediction.

- x_prepared:

  A data frame containing the preprocessed data to use in the model. The
  data frame can contain more variables than necessary within h2o_model

- y:

  The true outcome variable. Usually a factor.

- init_h2o:

  Whether to initialize h2o. Defaults to FALSE as I expect it was
  initialized during training already.

- calculate_metrics:

  Whether to calculate metrics. Defaults to TRUE.

- sample_split:

  A vector of length(y) containing the "split" of the data. The metrices
  are calculated for each split separately. Defaults to NULL, which
  means that the whole data is used.
