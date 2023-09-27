#' Plot empirical densities kernel estimate
#'
#' If more than 2 list elements or data.frame columns, NotImplemented error.
#'
#' 	1. Calculates density estimates per list-element/column with [density()]
#' 	2. Approximates the estimated densities with [stats::approx()] for equally spaced `length.out` elements between xmin and xmax
#' 	3. If `positive_label` is given, set this as the "+" density.
#'
#' @param df
#' data.frame or list of values. The names will be the legends, the values will
#' be used to estimate the densities.
#' @param xmin
#' If given, the densities are restricted/shown from this value on
#' @param xmax
#' If given, the densities are restricted/shown until this value
#' @param n_density
#' Parameter of [density()], from that documentation:
#' 	The number of equally spaced points at which the density is to be estimated.
#' 	When n > 512, it is rounded up to a power of 2 during the calculations
#' 	(as fft is used) and the final result is interpolated by approx.
#' 	So it almost always makes sense to specify n as a power of two.
#' @param length.out
#' After calculating the density, how many values should be used to _plot_ the density
#' @param colors_pos_neg_both
#' Colors for positive ("+"), negative ("-") and overlapping "+/-" areas of the
#' densities
#' @param positive_label
#' If `positive_label` is given, set this as the "+" density.
#' @param ...
#' Convenience, not used in this function though.
#'
#' @return
#' ggplot of the empirical densities
#' @export
#'
#' @examples
#'
#' sim_samples <- sim(
#'     list(
#'         "dist_1" = function(x) rnorm(x, mean = 1, sd = 1),
#'         "dist_2" = function(x) rnorm(x, mean = 0, sd = 1)
#'     ),
#'     do_melt = FALSE
#' )
#' plot_density_empirical(sim_samples)
#'
plot_density_empirical <- function(df,
                                   xmin = NULL,
                                   xmax = NULL,
                                   n_density = 512,
                                   length.out = 250,
                                   colors_pos_neg_both = c(
                                       "+" = "#008B45FF",
                                       "+/-" = "#3B4992FF",
                                       "-" = "#EE0000FF"
                                   ),
                                   positive_label = NULL,
                                   ...) {
    if (any(names(df) == "x")) {
        warning("Names are not allowed to be names 'x', renaming to 'value_x'")
        df[["value_x"]] <- df[["x"]]
        df[["x"]] <- NULL
    }

    density_estimates <- lapply(df, stats::density, n = n_density)
    if (is.null(xmin)) {
        xmin <- min(vapply(density_estimates, function(x) min(x[["x"]]), numeric(1)))
    }
    if (is.null(xmax)) {
        xmax <- max(vapply(density_estimates, function(x) max(x[["x"]]), numeric(1)))
    }
    eval_seq <- seq(from = xmin, to = xmax, length.out = length.out)
    density_approximations <- vapply(density_estimates, function(densX) {
        stats::approx(densX[["x"]], densX[["y"]], eval_seq)[["y"]]
    }, numeric(length(eval_seq)))
    density_approximations <- tibble::as_tibble(density_approximations)
    density_approximations[["x"]] <- eval_seq
    density_approximations[is.na(density_approximations)] <- 0

    if (!is.null(positive_label)) {
        # If a positive label is given, reorder the grouped values as plot_density_empirical
        # needs the "positive" group be the first (only when giving _two_ distributions)
        density_approximations <- density_approximations[c(positive_label, names(density_approximations)[-which(names(density_approximations) == positive_label)])]
    }

    if (length(df) == 2) {
        ### Checking names, relevant such that the colors are correctly shown
        density_names <- names(density_approximations)[names(density_approximations) != "x"]
        # suppressWarnings because of coercion warning.
        if (suppressWarnings(any(
            !is.na(as.numeric(density_names)) |
                !is.na(as.logical(density_names))
        ))) {
            # Then there are numerics as characters in the names, ggplot seems to be unable to
            # handle this. Therefore make a letter in front.
            density_names <- paste0("x_", density_names)
        }
        names(density_approximations)[names(density_approximations) != "x"] <- density_names
        names(colors_pos_neg_both)[c(1, 3)] <- names(density_approximations)[1:2]

        dplot <- plot_distributions_2(
            density_approximations,
            colors_pos_neg_both = colors_pos_neg_both,
            name_dist_1 = names(density_approximations)[1],
            name_dist_2 = names(density_approximations)[2]
        )
    } else {
        density_approximations_long <- tidyr::pivot_longer(
            density_approximations,
            cols = names(density_approximations)[names(density_approximations) != "x"],
            names_to = "Distribution",
            values_to = "Probability density"
        )
        dplot <- plot_distributions_multiple(
            density_approximations_long
        )
    }
    return(dplot)
}
