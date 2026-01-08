# Apply a (nested list) of rROC results to new data

Restriction returns an informative range which tells that some samples
should be discarded according to their predictor value. This function
applies the restriction to a new dataset. Predictor values outside the
informative range are imputed with `removed_impute`.

## Usage

``` r
apply_restriction(object, newdata, feature = NA, removed_impute = -1)
```

## Arguments

- object:

  A (nested list) of rROC results. See
  [`rROC`](https://ggrlab.github.io/restrictedROC/reference/rROC.md) for
  details.

- newdata:

  A data frame with all predictor variables.

- feature:

  The current feature. If `object` is a single restrictedROC object,
  this is the name of the feature. If `object` is a (nested list), this
  is the name of the current list element. Example:
  `list("outcome_1"=list("feature_1"=rroc_result))` `feature` is
  `"feature_1"` when `rroc_result` is processed when rroc_result is
  detected.

- removed_impute:

  The value to impute for samples outside the informative range.

## Value

A list with three elements:

- `predictions`: A data frame with the original predictor values and the
  imputed values. It has three columns:

  - `predictor`:

    is the original predictor value.

  - `keep`:

    is a logical vector that indicates whether the sample is inside the
    informative range.

  - `bounded`:

    is the imputed value.

  - `bounded_twoside`:

    is the imputed value for two-sided restriction: RestrictionValue +
    or - `removed_impute`

- `thresholds`: A data frame with the feature, threshold, the part of
  the predictor that was used and the imputed value for predictor values
  outside the informative range. The informative range is then:

  - `low`:

    `(-Inf, threshold)`

  - `high`:

    `(threshold, Inf)`
