# Give a summary of a rROC() result

[`rROC`](https://ggrlab.github.io/restrictedROC/reference/rROC.md)
returns a nested list. It looks something like:
`single_restriction <- reslist[[dependent_variables]][[independent_variables]]`
where every single restriction has done permutations, a p-value and
additional info. This function gives a summary of the result.

## Usage

``` r
# S3 method for class 'rROC'
summary(
  object,
  relevant_cols_regex = c("pval.twoside.*", "n_permutations", "positive_label",
    ".*\\.auc$", "part", "restriction", "informative_range.*"),
  searchword = "max_total",
  current_level = 0,
  ...
)
```

## Arguments

- object:

  A [`rROC`](https://ggrlab.github.io/restrictedROC/reference/rROC.md)
  result

- relevant_cols_regex:

  A regex to filter the columns of a single restriction.

- searchword:

  If the searchword is not found in the names of the current element,
  the current level is increased and the function is called recursively.
  If the searchword is found, the summary is returned.

- current_level:

  The current level of the nested list. This is used internally.

- ...:

  Additional arguments passed to
  [`summary`](https://rdrr.io/r/base/summary.html).

## Value

A summary of the result:
