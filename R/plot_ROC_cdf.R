#' ROC curve for given cumulative distribution functions
#'
#' @param cdf_positive
#' Cumulative distribution function for the positive class
#' @param cdf_negative
#' Cumulative distribution function for the negative class
#' @inheritParams plot_density_ROC
#' @return ggplot ROC-curve
#' @export
#'
#' @examples
#' plot_ROC_cdf(
#'     cdf_positive = function(x) pnorm(x, mean = 1, sd = 1),
#'     cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
#'     xmin = -5, xmax = 15, length.out = 2500
#' )
#' plot_ROC_cdf(
#'     cdf_positive = function(x) pnorm(x, mean = 1, sd = 1.5),
#'     cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
#'     xmin = -5, xmax = 15, length.out = 2500
#' )
#' plot_ROC_cdf(
#'     cdf_positive = function(x) pnorm(x, mean = 1, sd = 2.5),
#'     cdf_negative = function(x) pnorm(x, mean = 0, sd = 1),
#'     xmin = -5, xmax = 15, length.out = 2500
#' )
plot_ROC_cdf <- function(cdf_positive,
                         cdf_negative,
                         xmin = -1,
                         xmax = 1,
                         length.out = 1000) {
    x <- seq(from = xmin, to = xmax, length.out = length.out)

    df <- data.frame(
        tpr = 1 - cdf_positive(x),
        fpr = 1 - cdf_negative(x)
    )
    return(plot_ROC(df))
}
