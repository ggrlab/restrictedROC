#' Simulate values from distributions
#'
#' @param ...
#' 	Every argument must be a function which takes the number of values which should
#' 	be generated, e.g.:
#'
#' 		`function(n) random_n_values_from_distribution(n)`
#'
#' @param length.out
#'  How many samples should be drawn from each population
#' @param do_melt
#'  TRUE:
#'      # A tibble: 200 × 2
#'         Distribution  Value
#'         <chr>         <dbl>
#'       1 negative      0.805
#'       2 positive      2.62
#'       3 negative      1.19
#'       4 positive      1.49
#'
#'  FALSE:
#'      # A tibble: 100 × 2
#'         negative positive
#'            <dbl>    <dbl>
#'       1   0.631     2.81
#'       2   0.687     1.87
#'       3   0.0347    1.61
#'       4  -1.15      0.529
#'
#' @return
#' 	See do_melt parameter
#' @export
#'
#' @examples
#'
#' sim(
#'     negative = function(x) rnorm(x, mean = 0, sd = 1),
#'     positive = function(x) rnorm(x, mean = 1, sd = 1)
#' )
#' sim(
#'     list(
#'         negative = function(x) rnorm(x, mean = 0, sd = 1),
#'         positive = function(x) rnorm(x, mean = 1, sd = 1)
#'     )
#' )
#' sim(
#'     negative = function(x) rnorm(x, mean = 0, sd = 1),
#'     positive = function(x) rnorm(x, mean = 1, sd = 1),
#'     do_melt = FALSE
#' )
#' sim(
#'     negative = function(x) rnorm(x, mean = 0, sd = 1),
#'     positive = function(x) rnorm(x, mean = 1, sd = 1),
#'     do_melt = TRUE
#' )
#'
#' sim(
#'     negative = function(x) rnorm(x, mean = 0, sd = 1),
#'     positive = function(x) rnorm(x, mean = 1, sd = 1),
#'     length.out = 10
#' )
#'
sim <- function(..., length.out = 100, do_melt = TRUE) {
    args <- list(...)
    if (is.list(args[[1]])) {
        args <- args[[1]]
    }
    if (length(names(args)) == 0) {
        names(args) <- paste0("dist_", seq_along(args))
    }
    df <- tibble::as_tibble(lapply(args, function(rfun) rfun(length.out)))
    if (do_melt) {
        df <- melt_gendata(df)
    }
    return(df)
}
