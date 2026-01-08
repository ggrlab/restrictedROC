# Plot rzAUC vs FPR

    Plot rzAUCs for all calculated parts

## Usage

``` r
plot_rzAUCs(
  rROC_result,
  part = c("high", "low"),
  part_colors = default_part_colors
)
```

## Arguments

- rROC_result:

  rROC result from:

          single_rROC <- simple_rROC_interpret(simple_rROC(
          response = aSAH$outcome,
          predictor = aSAH$ndka
          ))

- part:

  vector which parts should be shown, can be multiple

- part_colors:

  Named vector which part should receive which color

## Value

ggplot object with rzAUCs shown for all false positive rates. For one
FPR multiple values can exist.

## Examples

``` r
# See plot_rROC()
```
