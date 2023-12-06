#' Plot multiple (usually >2) distributions
#'
#' @param df_melted
#' A melted data.frame/tibble of values (`x`) and distribution names (`Distribution`)
#'
#'  E.g.:
#'      # A tibble: 1,000 × 2
#'      	    x  Distribution
#'          <dbl>        <char>
#'       1  0.974             A
#'       2  0.196             A
#'       3 -0.125             A
#'       4  0.701             B
#'       5  1.701             B
#'       6  0.311             B
#' @param ...
#' Further UNUSED parameters to be compliant to plot_distributions_2
#'
#' @return
#' ggplot object with filled densities
#'
#' @examples
#' sim_samples <- sim(
#'     list(
#'         "dist_1" = function(x) rnorm(x, mean = 1, sd = 1),
#'         "dist_2" = function(x) rnorm(x, mean = 0, sd = 1)
#'     ),
#'     do_melt = FALSE
#' )
#'
#' density_estimates <- lapply(sim_samples, density, n = 100)
#' eval_seq <- seq(from = -5, to = 1, length.out = 250)
#' density_approximations <- tibble::as_tibble(vapply(density_estimates, function(densX) {
#'     stats::approx(densX[["x"]], densX[["y"]], eval_seq)[["y"]]
#' }, numeric(length(eval_seq))))
#' density_approximations[["x"]] <- eval_seq
#' density_approximations_long <- tidyr::pivot_longer(
#'     density_approximations,
#'     cols = names(density_approximations)[names(density_approximations) != "x"],
#'     names_to = "Distribution",
#'     values_to = "Probability density"
#' )
#'
#' restrictedROC:::plot_distributions_multiple(density_approximations_long)
plot_distributions_multiple <- function(df_melted, ...) {
    # Code hints of dataframe
    x <- Distribution <- `Probability density` <- NULL

    return(
        ggplot(df_melted, aes(x = x, col = Distribution, fill = Distribution)) +
            geom_ribbon(aes(ymin = 0, ymax = `Probability density`), alpha = .3) +
            ggpubr::theme_pubr()
    )
}


#' Plot exactly 2 distributions
#'
#' @param df
#' Table with the distribution values.
#' E.g.:
#'      # A tibble: 1,000 × 2
#'       Positive  Negative
#'          <dbl>     <dbl>
#'       1  0.974     0.257
#'       2  0.196    -0.780
#'       3 -0.125    -0.264
#'       4  0.701     0.260
#' @param colors_pos_neg_both
#' Colors of positive (+), negative (-) and overlapping (+/-) density-areas
#' @param name_dist_1
#' Name of the first distribution (first column)
#' @param name_dist_2
#' Name of the second distribution (second column)
#' @return
#' ggplot object
#'
#' @examples
#' # See e.g. plot_density_ROC_empirical()
plot_distributions_2 <- function(df,
                                 colors_pos_neg_both = colors_pos_neg_both_default,
                                 name_dist_1 = "Positive",
                                 name_dist_2 = "Negative") {
    # Code hints of dataframe
    x <- NULL
    baseplot <- ggplot(df, aes(x = x)) +
        geom_line(aes(y = .data[[name_dist_1]])) +
        geom_line(aes(y = .data[[name_dist_2]])) +
        geom_area(aes(
            y = pmin(.data[[name_dist_1]], .data[[name_dist_2]]),
            fill = "+/-"
        ), alpha = 0.3)


    only_A_df <- df[df[name_dist_1] > df[name_dist_2], ]
    only_B_df <- df[df[name_dist_2] > df[name_dist_1], ]
    if (nrow(only_A_df) > 0) {
        baseplot <- baseplot +
            geom_ribbon(
                data = df[df[name_dist_1] > df[name_dist_2], ],
                aes(
                    ymin = .data[[name_dist_2]],
                    ymax = .data[[name_dist_1]],
                    fill = name_dist_1
                ),
                alpha = 0.3
            )
    }
    if (nrow(only_B_df) > 0) {
        baseplot <- baseplot +
            geom_ribbon(
                data = df[df[name_dist_2] > df[name_dist_1], ],
                aes(
                    ymin = .data[[name_dist_1]],
                    ymax = .data[[name_dist_2]],
                    fill = name_dist_2
                ), alpha = 0.3
            )
    }
    names(colors_pos_neg_both)[names(colors_pos_neg_both) == "+/-"] <- "+/-"
    names(colors_pos_neg_both)[names(colors_pos_neg_both) == "both"] <- "+/-"
    names(colors_pos_neg_both)[names(colors_pos_neg_both) == "Both"] <- "+/-"
    baseplot <- baseplot +
        ggpubr::theme_pubr() +
        scale_fill_manual(values = colors_pos_neg_both) +
        labs(fill = "", color = "") +
        ggplot2::ylab("Density")
    return(baseplot)
}
