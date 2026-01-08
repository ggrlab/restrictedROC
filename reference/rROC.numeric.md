# rROC on a single numeric vector

rROC on a single numeric vector

## Usage

``` r
# S3 method for class 'numeric'
rROC(x, y, ...)
```

## Arguments

- x:

  Numeric vector, values of the independent variable for every
  element/sample.

- y:

  Factor vector, values of the dependent variable for every
  element/sample.

- ...:

  Arguments passed on to
  [`rROC.data.frame`](https://ggrlab.github.io/restrictedROC/reference/rROC.data.frame.md)

  `save_path`

  :   Path to save the results to. Intermediate results are saved into
      the directory file.path(save_path, "\_partial_directory").

  `save_intermediate`

  :   Should intermediate results be saved to disk? If TRUE, every
      combination by itself is saved into file.path(save_path,
      "\_partial_directory").

  `load_existing_intermediate`

  :   Should the earlier saved intermediate results in the folder
      file.path(save_path, "\_partial_directory") be loaded?

  `do_plots`

  :   Should the plot_density_rROC_empirical be calculated and returned?

  `verbose`

  :   Should progress be printed?

  `n_permutations`

  :   How many permutations should be done

  `fix_seed`

  :   boolean: If not FALSE, the seed for each permutation will be set
      by set.seed(fix_seed + permutation_i)

  `parallel_permutations`

  :   boolean: If TRUE, the permutation will be done via
      [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html),
      otherwise by [`base::lapply`](https://rdrr.io/r/base/lapply.html)

  `positive_label`

  :   Label for the positive class. All other values of `response` are
      regarded as negative cases.

## Value

A list of lists of simple_rROC_permutation and plot results. It is
structured as follows:

- dependent variable:

  - independent variable:

    - "plots":

      [`plot_density_rROC_empirical`](https://ggrlab.github.io/restrictedROC/reference/plot_density_rROC_empirical.md)
      result

    - "permutation":

      [`simple_rROC_permutation`](https://ggrlab.github.io/restrictedROC/reference/simple_rROC_permutation.md)
      result
