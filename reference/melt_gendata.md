# Melt generated data

Melt data generated in the form of lists. Given a list or data.frame of
elements, melt it from columnA columnB columnC into Distribution Value
columnA xA_1 columnA xA_2 ... ... columnB xB_2 ... ... columnC xC_2 ...
...

## Usage

``` r
melt_gendata(df)
```

## Arguments

- df:

  List or data.frame of values.

## Value

    data.frame with two columns:
        "Distribution"  Contains the name/colname of the respective value
        "Value"         The value itself

## Examples

``` r
a <- list("A" = 1:3, "B" = 5:19)
melt_gendata(a)
#> # A tibble: 18 × 2
#>    Value Distribution
#>    <int> <chr>       
#>  1     1 A           
#>  2     2 A           
#>  3     3 A           
#>  4     5 B           
#>  5     6 B           
#>  6     7 B           
#>  7     8 B           
#>  8     9 B           
#>  9    10 B           
#> 10    11 B           
#> 11    12 B           
#> 12    13 B           
#> 13    14 B           
#> 14    15 B           
#> 15    16 B           
#> 16    17 B           
#> 17    18 B           
#> 18    19 B           

a <- data.frame("a" = 1:10, "b" = 6:15)
melt_gendata(a)
#> # A tibble: 20 × 2
#>    Value Distribution
#>    <int> <chr>       
#>  1     1 a           
#>  2     2 a           
#>  3     3 a           
#>  4     4 a           
#>  5     5 a           
#>  6     6 a           
#>  7     7 a           
#>  8     8 a           
#>  9     9 a           
#> 10    10 a           
#> 11     6 b           
#> 12     7 b           
#> 13     8 b           
#> 14     9 b           
#> 15    10 b           
#> 16    11 b           
#> 17    12 b           
#> 18    13 b           
#> 19    14 b           
#> 20    15 b           
```
