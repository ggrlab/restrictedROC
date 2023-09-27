#' Plot density + ROC based on stats::_<NAME>
#'
#' Given positive and negative distribution _NAMES_ and their arguments,
#' plot the corresponding densities and their ROC-curves.
#'
#' @param dist_positive_str
#' A name of any distribution of the `stats::` package. E.g. "norm", "binom", "exp".
#' This is the distribution of values coming from the "positive"-class.
#' For all possible distributions check ?stats::Distributions
#' @param dist_negative
#' A name of any distribution of the `stats::` package. E.g. "norm", "binom", "exp".
#' This is the distribution of values coming from the "negative"-class.
#' For all possible distributions check ?stats::Distributions
#' @param dist_positive_args
#' Arguments to `dist_positive_str` as named list.
#' @param dist_negative_args
#' Arguments to `dist_negative_str` as named list.
#'
#' @inheritParams plot_density_ROC
#' @inherit plot_density_ROC return
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' plot_density_ROC_str()
#' plot_density_ROC_str(
#'     dist_positive_str = "norm",
#'     dist_negative = "norm",
#'     dist_positive_args = list("mean" = 0, "sd" = 1),
#'     dist_negative_args = list("mean" = 1, "sd" = 1),
#'     xmin = -4, xmax = 7
#' )
#' # pdf("removeme.pdf", width = 14, height = 6)
#' plot_density_ROC_str(
#'     dist_positive_str = "norm",
#'     dist_negative = "norm",
#'     dist_positive_args = list("mean" = 1, "sd" = 1),
#'     dist_negative_args = list("mean" = 0, "sd" = 1),
#'     xmin = -4, xmax = 7
#' )
#' # dev.off()
#' # pdf("removeme_2.pdf", width = 14, height = 6)
#' plot_density_ROC_str(
#'     dist_positive_str = "norm",
#'     dist_negative = "norm",
#'     dist_positive_args = list("mean" = 1, "sd" = .5),
#'     dist_negative_args = list("mean" = 1, "sd" = 1),
#'     xmin = -4, xmax = 7, length.out = 2503
#' )
#' # dev.off()
#' # pdf("removeme_3.pdf", width = 14, height = 6)
#' plot_density_ROC_str(
#'     dist_positive_str = "norm",
#'     dist_negative = "norm",
#'     dist_positive_args = list("mean" = 1, "sd" = 2),
#'     dist_negative_args = list("mean" = 1, "sd" = 1),
#'     xmin = -4, xmax = 7, length.out = 2503
#' )
#' # dev.off()
#' tmp <- plot_density_ROC_str(
#'     dist_positive_str = "norm",
#'     dist_negative = "norm",
#'     dist_positive_args = list("mean" = 1, "sd" = 2),
#'     dist_negative_args = list("mean" = 1, "sd" = 1),
#'     xmin = -4, xmax = 7, length.out = 2503
#' )
#'
plot_density_ROC_str <- function(dist_positive_str = "norm",
                                 dist_negative = "norm",
                                 dist_positive_args = list("mean" = 0, "sd" = 1),
                                 dist_negative_args = list("mean" = 0, "sd" = 1),
                                 length.out = 500, xmin = -3, xmax = 3) {
    density_positive_fun <- get(paste0("d", dist_positive_str))
    density_positive <- function(x) {
        do.call(density_positive_fun, c(list("x" = x), dist_positive_args))
    }
    cdf_positive_fun <- get(paste0("p", dist_positive_str))
    cdf_positive <- function(x) {
        do.call(cdf_positive_fun, c(list("q" = x), dist_positive_args))
    }

    density_negative_fun <- get(paste0("d", dist_negative))
    density_negative <- function(x) {
        do.call(density_negative_fun, c(list("x" = x), dist_negative_args))
    }
    quantile_negative_fun <- get(paste0("q", dist_negative))
    quantile_negative <- function(x) {
        do.call(quantile_negative_fun, c(list("p" = x), dist_negative_args))
    }
    plot_density_ROC(
        density_positive = density_positive,
        cdf_positive = cdf_positive,
        density_negative = density_negative,
        quantile_negative = quantile_negative,
        length.out = length.out, xmin = xmin, xmax = xmax
    )
}

#' Plot density + ROC
#'
#' Given positive and negative distributions, plot the corresponding densities and their ROC-curves.
#'
#' @param density_positive
#' `function(x) {}`: Returns the density given a specific value x.
#' Refers to values coming from the "positive"-class.
#' @param cdf_positive
#' `function(q) {...}`: Returns the cumulative distribution value given a specific quantile `q`.
#' Refers to values coming from the "positive"-class.
#' @param density_negative
#' `function(x) {}`: Returns the density given a specific value x.
#' Refers to values coming from the "negative"-class.
#' @param quantile_negative
#' `function(x) {}`: Returns the density given a specific value x.
#' Refers to values coming from the "negative"-class.
#' @param length.out
#' The number of simulated points for the plots from the distributions.
#' `length.out` points will be generated between `[xmin, xmax]`.
#' PURELY VISUAL, ROC curves are calculated based on the actual distributions!
#' @param xmin
#' Minimum value of simulated points for the plots from the distributions.
#' `length.out` points will be generated between `[xmin, xmax]`.
#' PURELY VISUAL, ROC curves are calculated based on the actual distributions!
#' @param xmax
#' Maximum value of simulated points for the plots from the distributions.
#' `length.out` points will be generated between `[xmin, xmax]`.
#' PURELY VISUAL, ROC curves are calculated based on the actual distributions!
#'
#' @return `patchwork`'ed ggplots: left density, right ROC curve.
#' @export
#'
#' @examples
#' plot_density_ROC(
#'     density_positive = function(x) dnorm(x, mean = 0, sd = 1),
#'     cdf_positive = function(x) pnorm(x, mean = 0, sd = 1),
#'     density_negative = function(x) dnorm(x, mean = -1, sd = 1.5),
#'     quantile_negative = function(x) qnorm(x, mean = -1, sd = 1.5),
#'     xmin = -4, xmax = 7
#' )
plot_density_ROC <- function(density_positive,
                             cdf_positive,
                             density_negative,
                             quantile_negative,
                             length.out = 500, xmin = -3, xmax = 3,
                             colors_pos_neg_both = c(
                                 "+" = "#008B45FF",
                                 "+/-" = "#3B4992FF",
                                 "-" = "#EE0000FF"
                             )) {
    plotted_roc <- plot_ROC_theoretical(
        pnorm_positive = cdf_positive,
        qnorm_negative = quantile_negative,
        length.out = length.out
    )
    df_melted <- generate_density_values(
        density_positive,
        density_negative,
        xmin = xmin, xmax = xmax, length.out = length.out, do_melt = FALSE
    )

    names(colors_pos_neg_both)[c(1, 3)] <- names(df_melted)[2:3]
    plotted_density <- plot_distributions_2(
        df = df_melted,
        colors_pos_neg_both = colors_pos_neg_both,
        name_dist_1 = "dist_1",
        name_dist_2 = "dist_2"
    )
    return(list(
        "plot" = patchwork::wrap_plots(plotted_density, plotted_roc[["plot"]]),
        "auc" = plotted_roc[["auc"]]
    ))
}
