# Restriction for multiple dependent and independent variables

Restriction for multiple dependent and independent variables

## Usage

``` r
prune(x, ...)
```

## Arguments

- x:

  data.frame

  :   See
      [`rROC.data.frame`](https://ggrlab.github.io/restrictedROC/reference/rROC.data.frame.md).
      data.frame containing all dependent and independent variables as
      columns. Dependent/independent variable column names must be given
      as "dependent_vars"/"independent_vars" arguments.

  matrix

  :   See
      [`rROC.matrix`](https://ggrlab.github.io/restrictedROC/reference/rROC.matrix.md).
      Matrix of (samples x features). Dependent variable(s) must be
      given as "y" argument.

  numeric vector

  :   See
      [`rROC.numeric`](https://ggrlab.github.io/restrictedROC/reference/rROC.numeric.md).
      Numeric vector of independent variable. Dependent variable(s) must
      be given as "y" argument.

- ...:

  Arguments passed on to
  [`rROC.data.frame`](https://ggrlab.github.io/restrictedROC/reference/rROC.data.frame.md),
  [`simple_rROC_permutation`](https://ggrlab.github.io/restrictedROC/reference/simple_rROC_permutation.md)

  `y`

  :   Either a vector of dependent variable values or a list of length 1
      of a vector of dependent variable values. If NULL, dependent_vars
      must be given.

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
