# Restriction for multiple dependent and independent variables

Restriction for multiple dependent and independent variables. Traverses
all dependent variables and within all independent variables. Then
calculates the rROC in the sense of dependent ~ independent.

Can save intermediate results to disk, to avoid re-calculating for
crash-reasons or to save time when re-running the same analysis.

Can return plot_density_rROC_empirical for every combination.

## Usage

``` r
# S3 method for class 'data.frame'
rROC(
  x,
  independent_vars,
  dependent_vars = NULL,
  y = NULL,
  save_path = NULL,
  parallel_permutations = TRUE,
  n_permutations = 10000,
  save_intermediate = TRUE,
  load_existing_intermediate = TRUE,
  positive_label = 1,
  verbose = TRUE,
  do_plots = FALSE,
  fix_seed = 0,
  ...
)
```

## Arguments

- x:

  A data.frame containing all dependent and independent variables as
  columns.

- independent_vars:

  A character vector of independent variable column names. If NULL, all
  columns except dependent_vars are used.

- dependent_vars:

  A character vector of dependent variable column names. If NULL, `y`
  must be given.

- y:

  Either a vector of dependent variable values or a list of length 1 of
  a vector of dependent variable values. If NULL, dependent_vars must be
  given.

- save_path:

  Path to save the results to. Intermediate results are saved into the
  directory file.path(save_path, "\_partial_directory").

- parallel_permutations:

  boolean: If TRUE, the permutation will be done via
  [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html),
  otherwise by [`base::lapply`](https://rdrr.io/r/base/lapply.html)

- n_permutations:

  How many permutations should be done

- save_intermediate:

  Should intermediate results be saved to disk? If TRUE, every
  combination by itself is saved into file.path(save_path,
  "\_partial_directory").

- load_existing_intermediate:

  Should the earlier saved intermediate results in the folder
  file.path(save_path, "\_partial_directory") be loaded?

- positive_label:

  Label for the positive class. All other values of `response` are
  regarded as negative cases.

- verbose:

  Should progress be printed?

- do_plots:

  Should the plot_density_rROC_empirical be calculated and returned?

- fix_seed:

  boolean: If not FALSE, the seed for each permutation will be set by
  set.seed(fix_seed + permutation_i)

- ...:

  Arguments passed on to
  [`simple_rROC_permutation`](https://ggrlab.github.io/restrictedROC/reference/simple_rROC_permutation.md)

  `return_proc`

  :   1.  Should pROC::roc() be returned for the full dataset? 2) Should
          pROC::roc() be returned on each of the part datasets? Only
          works with `get_all_aucs_fun=get_all_aucs` after
          `get_all_aucs_norecalculation()` does not calculate the ROC
          curves for each restriction separately.

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
