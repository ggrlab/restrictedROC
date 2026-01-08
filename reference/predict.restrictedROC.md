# S3 class predict for class "restrictedROC"

Predict new samples for a given restrictedROC result

## Usage

``` r
# S3 method for class 'restrictedROC'
predict(
  object,
  newdata,
  newdata_predictor_column = 1,
  newdata_response_column = 2,
  pred_high_label = 1,
  pred_low_label = 0,
  original.response = NA,
  original.predictor = NA,
  ...
)
```

## Arguments

- object:

  Result from
  [`simple_rROC_interpret()`](https://ggrlab.github.io/restrictedROC/reference/simple_rROC_interpret.md),
  easiest calculated from simple_rROC(..., return_proc=TRUE)

- newdata:

  The new dataframe which should be predicted

- newdata_predictor_column:

  Column name or number of the predictor (What you use to predict the
  response)

- newdata_response_column:

  Column name or number of the true response (/outcome)

- pred_high_label:

  Label of predictions HIGHER than the cutoff (Youden-index)

- pred_low_label:

  Label of predictions LOWER OR EQUAL than the cutoff (Youden-index)

- original.response:

  If the original response cannot be loaded from `object`, you have to
  give the original response.

- original.predictor:

  If the original predictor cannot be loaded from `object`, you have to
  give the predictor

- ...:

  Not used but keeps devtools::check() happy

## Value

List of table_full Table of full predictions, classified using the
Youden-Index. This is the classical way of predicting. table_restricted
Table of restricted predictions, classified using the Youden-Index.
Samples which were identified as "unclassifiable" with our restriction
method were removed. pred tibble with "response": True response
"predictor": Values to use as predictors "keep": Should they be kept
according to restriction "prediction": Youden-based predictions,
regardless if kept or not threshold_and_restriction Vector with
"restriction": The restriction value, decides together with
`restriction_part` which samples are kept. "classification_threshold":
Values above will be classified as `pred_high_label` restriction_part
Should "high", "low" or "global" (=all) samples be kept, therefore
samples with values higher or lower (or all) with respect to the
restriction are kept.

    tab_classifiable
        Restricted table including the unclassifiable predictions, e.g.

                            response
            prediction          Poor Good
                Poor              29   35
                Good               8   34
                unclassifiable     4    3

## Examples

``` r
data(aSAH, package = "pROC")
tmp <- simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka,
    return_proc = TRUE
)
#> Positive label not given, setting to last level of factor: Poor
single_rROC <- simple_rROC_interpret(tmp)

predict(
    single_rROC,
    newdata = aSAH,
    newdata_predictor_column = "ndka",
    newdata_response_column = "outcome",
    pred_high_label = "Poor",
    pred_low_label = "Good"
)
#> $table_full
#>          response
#> pred_full Poor Good
#>      Poor   29   35
#>      Good   12   37
#> 
#> $table_restricted
#>          response
#> pred_kept Poor Good
#>      Poor   29   35
#>      Good    8   34
#> 
#> $pred
#> # A tibble: 113 × 5
#>    response predictor keep  pred_full pred_kept
#>    <fct>        <dbl> <lgl> <fct>     <fct>    
#>  1 Good          3.01 FALSE Good      Good     
#>  2 Good          8.54 TRUE  Good      Good     
#>  3 Good          8.09 TRUE  Good      Good     
#>  4 Good         10.4  TRUE  Good      Good     
#>  5 Poor         17.4  TRUE  Poor      Poor     
#>  6 Poor         12.8  TRUE  Poor      Poor     
#>  7 Good          6    TRUE  Good      Good     
#>  8 Poor         13.2  TRUE  Poor      Poor     
#>  9 Good         15.5  TRUE  Poor      Poor     
#> 10 Good          6.01 TRUE  Good      Good     
#> # ℹ 103 more rows
#> 
#> $threshold_and_restriction
#>   restriction classification_threshold_restricted
#> 1       5.685                               11.08
#>   classification_direction_restricted_control_X_case
#> 1                                                  <
#>   classification_threshold_full classification_direction_full_control_X_case
#> 1                         11.08                                            <
#> 
#> $restriction_part
#> [1] "high"
#> 
#> $table_classifiable
#>                 response
#> pred_kept        Poor Good
#>   Poor             29   35
#>   Good              8   34
#>   unclassifiable    4    3
#> 

tmp <- simple_rROC(
    response = aSAH$outcome,
    predictor = aSAH$ndka
)
#> Positive label not given, setting to last level of factor: Poor
single_rROC_noFullROC <- simple_rROC_interpret(tmp)
if (FALSE) { # \dontrun{
predict(
    single_rROC_noFullROC,
    newdata = aSAH,
    newdata_predictor_column = "ndka",
    newdata_response_column = "outcome",
    pred_high_label = "Poor",
    pred_low_label = "Good"
)
} # }
predict(
    single_rROC_noFullROC,
    newdata = aSAH,
    newdata_predictor_column = "ndka",
    newdata_response_column = "outcome",
    pred_high_label = "Poor",
    pred_low_label = "Good",
    original.response = aSAH$outcome,
    original.predictor = aSAH$ndka
)
#> $table_full
#>          response
#> pred_full Poor Good
#>      Poor   29   35
#>      Good   12   37
#> 
#> $table_restricted
#>          response
#> pred_kept Poor Good
#>      Poor   29   35
#>      Good    8   34
#> 
#> $pred
#> # A tibble: 113 × 5
#>    response predictor keep  pred_full pred_kept
#>    <fct>        <dbl> <lgl> <fct>     <fct>    
#>  1 Good          3.01 FALSE Good      Good     
#>  2 Good          8.54 TRUE  Good      Good     
#>  3 Good          8.09 TRUE  Good      Good     
#>  4 Good         10.4  TRUE  Good      Good     
#>  5 Poor         17.4  TRUE  Poor      Poor     
#>  6 Poor         12.8  TRUE  Poor      Poor     
#>  7 Good          6    TRUE  Good      Good     
#>  8 Poor         13.2  TRUE  Poor      Poor     
#>  9 Good         15.5  TRUE  Poor      Poor     
#> 10 Good          6.01 TRUE  Good      Good     
#> # ℹ 103 more rows
#> 
#> $threshold_and_restriction
#>   restriction classification_threshold_restricted
#> 1       5.685                               11.08
#>   classification_direction_restricted_control_X_case
#> 1                                                  <
#>   classification_threshold_full classification_direction_full_control_X_case
#> 1                         11.08                                            <
#> 
#> $restriction_part
#> [1] "high"
#> 
#> $table_classifiable
#>                 response
#> pred_kept        Poor Good
#>   Poor             29   35
#>   Good              8   34
#>   unclassifiable    4    3
#> 
```
