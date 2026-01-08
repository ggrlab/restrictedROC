# Generate values for given densities

Generate values for a given list of density functions in the range from
`xmin` to `xmax` with `length.out` elements. Usually not used directly
but by
[`plot_density_ROC()`](https://ggrlab.github.io/restrictedROC/reference/plot_density_ROC.md)

## Usage

``` r
generate_density_values(
  ...,
  xmin = -1,
  xmax = 1,
  length.out = 100,
  do_melt = TRUE
)
```

## Arguments

- ...:

  List or multiple arguments of density functions in the following form:
  positive = function(x) dnorm(x, mean = 0, sd = 1), negative =
  function(x) dnorm(x, mean = -1, sd = 1), negative2 = function(x)
  dnorm(x, mean = -3, sd = 1)

- xmin:

  Minimum value where the given function(s) are calculated

- xmax:

  Maximum value where the given function(s) are calculated

- length.out:

  How many values should be generated between xmin and xmax? Used in
  [`seq()`](https://rdrr.io/r/base/seq.html)

- do_melt:

  If TRUE, apply
  [`melt_gendata()`](https://ggrlab.github.io/restrictedROC/reference/melt_gendata.md)

## Value

    list() or tibble::tibble()

## Examples

``` r
generate_density_values(
    positive = function(x) dnorm(x, mean = 0, sd = 1),
    negative = function(x) dnorm(x, mean = -1, sd = 1),
    negative2 = function(x) dnorm(x, mean = -3, sd = 1)
)
#> # A tibble: 400 × 2
#>     Value Distribution
#>     <dbl> <chr>       
#>  1 -1     x           
#>  2 -0.980 x           
#>  3 -0.960 x           
#>  4 -0.939 x           
#>  5 -0.919 x           
#>  6 -0.899 x           
#>  7 -0.879 x           
#>  8 -0.859 x           
#>  9 -0.838 x           
#> 10 -0.818 x           
#> # ℹ 390 more rows
generate_density_values(
    positive = function(x) dnorm(x, mean = 0, sd = 1),
    xmin = 0, xmax = 1
)
#> # A tibble: 200 × 2
#>     Value Distribution
#>     <dbl> <chr>       
#>  1 0      x           
#>  2 0.0101 x           
#>  3 0.0202 x           
#>  4 0.0303 x           
#>  5 0.0404 x           
#>  6 0.0505 x           
#>  7 0.0606 x           
#>  8 0.0707 x           
#>  9 0.0808 x           
#> 10 0.0909 x           
#> # ℹ 190 more rows
generate_density_values(
    positive = function(x) dnorm(x, mean = 0, sd = 1),
    do_melt = FALSE
)
#> # A tibble: 100 × 2
#>         x positive
#>     <dbl>    <dbl>
#>  1 -1        0.242
#>  2 -0.980    0.247
#>  3 -0.960    0.252
#>  4 -0.939    0.257
#>  5 -0.919    0.261
#>  6 -0.899    0.266
#>  7 -0.879    0.271
#>  8 -0.859    0.276
#>  9 -0.838    0.281
#> 10 -0.818    0.285
#> # ℹ 90 more rows
generate_density_values(
    positive = function(x) dnorm(x, mean = 0, sd = 1),
    do_melt = TRUE
)
#> # A tibble: 200 × 2
#>     Value Distribution
#>     <dbl> <chr>       
#>  1 -1     x           
#>  2 -0.980 x           
#>  3 -0.960 x           
#>  4 -0.939 x           
#>  5 -0.919 x           
#>  6 -0.899 x           
#>  7 -0.879 x           
#>  8 -0.859 x           
#>  9 -0.838 x           
#> 10 -0.818 x           
#> # ℹ 190 more rows
```
