#' Generate values for given densities
#'
#' Generate values for a given list of density functions in the range from ``xmin`` to ``xmax``
#' with ``length.out`` elements. Usually not used directly but by [plot_density_ROC()]
#'
#' @param ...
#' List or multiple arguments of density functions in the following form:
#'     positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     negative = function(x) dnorm(x, mean = -1, sd = 1),
#'     negative2 = function(x) dnorm(x, mean = -3, sd = 1)
#' @param xmin
#' Minimum value where the given function(s) are calculated
#' @param xmax
#' Maximum value where the given function(s) are calculated
#' @param length.out
#' How many values should be generated between xmin and xmax? Used in [seq()]
#' @param do_melt
#' If TRUE, apply [melt_gendata()]
#'
#' @return
#' 	list() or tibble::tibble()
#' @export
#'
#' @examples
#' generate_density_values(
#'     positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     negative = function(x) dnorm(x, mean = -1, sd = 1),
#'     negative2 = function(x) dnorm(x, mean = -3, sd = 1)
#' )
#' generate_density_values(
#'     positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     xmin = 0, xmax = 1
#' )
#' generate_density_values(
#'     positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     do_melt = FALSE
#' )
#' generate_density_values(
#'     positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     do_melt = TRUE
#' )
generate_density_values <- function(..., xmin = -1, xmax = 1, length.out = 100, do_melt = TRUE) {
    args <- list(...)
    xvals <- seq(from = xmin, to = xmax, length.out = length.out)
    if (is.list(args[[1]])) {
        args <- args[[1]]
    }
    if (length(names(args)) == 0) {
        names(args) <- paste0("dist_", seq_along(args))
    }
    df <- tibble::tibble("x" = xvals, tibble::as_tibble(lapply(args, function(dfun) dfun(xvals))))
    if (do_melt) {
        df <- melt_gendata(df)
    }
    return(df)
}
