geom_jitter_scaling <- function(result_plot_density_empirical,
                                values_grouped,
                                current_label,
                                color = "black",
                                scale_sum_density = 0.075,
                                scale_max = 0.01,
                                size = .25,
                                n_points = Inf) {
    value <- jitter_height <- NULL # only for linting
    # plot all points in the respective color on the x-axis with some jitter
    plotdata_df_sumdensity <- result_plot_density_empirical[["data"]]["x"]
    plotdata_df_sumdensity[["density_sum"]] <- apply(
        result_plot_density_empirical[["data"]][, 1:2], 1, sum
    )
    afun <- stats::approxfun(
        x = plotdata_df_sumdensity[["x"]],
        y = plotdata_df_sumdensity[["density_sum"]]
    )
    # afun(plotdata_df_sumdensity[["x"]]) == plotdata_df_sumdensity[["density_sum"]]

    data_approxdens <- data.frame(
        value = values_grouped[[current_label]]
    )
    if (n_points < Inf) {
        data_approxdens <- data_approxdens[sample(
            nrow(data_approxdens),
            min(n_points, nrow(data_approxdens))
        ), , drop = FALSE]
    }
    data_approxdens[["approxed_dens"]] <- afun(data_approxdens[["value"]])
    extreme_vals <- sapply(data_approxdens[["approxed_dens"]], function(x) {
        max(
            x * scale_sum_density,
            max(data_approxdens[["approxed_dens"]]) * scale_max
        )
    })
    extreme_vals_sampled <- sapply(extreme_vals, function(x) {
        runif::runif(1, min = -x, max = x)
    })
    data_approxdens[["extreme_vals"]] <- extreme_vals
    data_approxdens[["jitter_height"]] <- extreme_vals_sampled
    # print(
    #     plotted_density +
    #         ggplot2::geom_point(
    #             data = data_approxdens, mapping = aes(x = value, y = extreme_vals),
    #             size = .5
    #         )
    # )
    return(
        result_plot_density_empirical +
            ggplot2::geom_point(
                data = data_approxdens, mapping = aes(x = value, y = jitter_height),
                size = size, color = color
            )
    )
}
