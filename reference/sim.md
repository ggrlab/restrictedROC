# Simulate values from distributions

Simulate values from distributions

## Usage

``` r
sim(..., length.out = 100, do_melt = TRUE)
```

## Arguments

- ...:

  Every argument must be a function which takes the number of values
  which should be generated, e.g.:

          `function(n) random_n_values_from_distribution(n)`

- length.out:

  How many samples should be drawn from each population

- do_melt:

  TRUE: \# A tibble: 200 × 2 Distribution Value 1 negative 0.805 2
  positive 2.62 3 negative 1.19 4 positive 1.49

  FALSE: \# A tibble: 100 × 2 negative positive 1 0.631 2.81 2 0.687
  1.87 3 0.0347 1.61 4 -1.15 0.529

## Value

    See do_melt parameter

## Examples

``` r
sim(
    negative = function(x) rnorm(x, mean = 0, sd = 1),
    positive = function(x) rnorm(x, mean = 1, sd = 1)
)
#> # A tibble: 200 × 2
#>       Value Distribution
#>       <dbl> <chr>       
#>  1 -0.545   negative    
#>  2 -0.0205  negative    
#>  3 -0.339   negative    
#>  4  0.186   negative    
#>  5  0.346   negative    
#>  6 -0.793   negative    
#>  7 -0.749   negative    
#>  8 -1.85    negative    
#>  9 -1.56    negative    
#> 10  0.00235 negative    
#> # ℹ 190 more rows
sim(
    list(
        negative = function(x) rnorm(x, mean = 0, sd = 1),
        positive = function(x) rnorm(x, mean = 1, sd = 1)
    )
)
#> # A tibble: 200 × 2
#>     Value Distribution
#>     <dbl> <chr>       
#>  1 -1.37  negative    
#>  2  0.190 negative    
#>  3  1.96  negative    
#>  4 -0.806 negative    
#>  5  0.247 negative    
#>  6  0.508 negative    
#>  7 -0.496 negative    
#>  8 -0.902 negative    
#>  9  1.01  negative    
#> 10  1.64  negative    
#> # ℹ 190 more rows
sim(
    negative = function(x) rnorm(x, mean = 0, sd = 1),
    positive = function(x) rnorm(x, mean = 1, sd = 1),
    do_melt = FALSE
)
#> # A tibble: 100 × 2
#>    negative positive
#>       <dbl>    <dbl>
#>  1  -0.163   -0.419 
#>  2   0.0209   0.486 
#>  3   0.551    1.77  
#>  4   0.755    2.40  
#>  5  -1.02     0.984 
#>  6  -0.321   -0.0233
#>  7  -0.457   -1.12  
#>  8  -2.17     1.15  
#>  9   0.0664   0.479 
#> 10   1.51     0.0945
#> # ℹ 90 more rows
sim(
    negative = function(x) rnorm(x, mean = 0, sd = 1),
    positive = function(x) rnorm(x, mean = 1, sd = 1),
    do_melt = TRUE
)
#> # A tibble: 200 × 2
#>      Value Distribution
#>      <dbl> <chr>       
#>  1 -0.125  negative    
#>  2  0.248  negative    
#>  3 -0.627  negative    
#>  4  0.683  negative    
#>  5  0.590  negative    
#>  6 -0.815  negative    
#>  7 -0.346  negative    
#>  8  0.0565 negative    
#>  9 -0.567  negative    
#> 10 -0.0428 negative    
#> # ℹ 190 more rows

sim(
    negative = function(x) rnorm(x, mean = 0, sd = 1),
    positive = function(x) rnorm(x, mean = 1, sd = 1),
    length.out = 10
)
#> # A tibble: 20 × 2
#>      Value Distribution
#>      <dbl> <chr>       
#>  1 -1.25   negative    
#>  2 -0.925  negative    
#>  3 -0.352  negative    
#>  4 -0.0416 negative    
#>  5  0.729  negative    
#>  6  0.293  negative    
#>  7  3.59   negative    
#>  8  0.756  negative    
#>  9  0.400  negative    
#> 10  0.829  negative    
#> 11  0.695  positive    
#> 12  1.71   positive    
#> 13  1.44   positive    
#> 14  1.36   positive    
#> 15  0.876  positive    
#> 16  0.307  positive    
#> 17  1.57   positive    
#> 18  0.783  positive    
#> 19  4.04   positive    
#> 20 -0.592  positive    
```
