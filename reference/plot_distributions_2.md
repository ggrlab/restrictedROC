# Plot exactly 2 distributions

Plot exactly 2 distributions

## Usage

``` r
plot_distributions_2(
  df,
  colors_pos_neg_both = colors_pos_neg_both_default,
  name_dist_1 = "Positive",
  name_dist_2 = "Negative"
)
```

## Arguments

- df:

  Table with the distribution values. E.g.: \# A tibble: 1,000 × 2
  Positive Negative 1 0.974 0.257 2 0.196 -0.780 3 -0.125 -0.264 4 0.701
  0.260

- colors_pos_neg_both:

  Colors of positive (+), negative (-) and overlapping (+/-)
  density-areas

- name_dist_1:

  Name of the first distribution (first column)

- name_dist_2:

  Name of the second distribution (second column)

## Value

ggplot object

## Examples

``` r
# See e.g. plot_density_ROC_empirical()
```
