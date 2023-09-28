#' Plot density + ROC
#'
#' Given positive and negative distributions, plot the corresponding densities and their ROC-curves.
#'
#' @param values_grouped
#'  List-like element. (also data.frames or matrices) where the elements are the
#'  samples from the different distributions.
#'  E.g.:
#'      # A tibble: 1,000 × 2
#'         dist_1 dist_2
#'          <dbl>  <dbl>
#'       1  0.974  0.257
#'       2  0.196 -0.780
#'       3 -0.125 -0.264
#'       4  0.701  0.260
#'
#' @return `patchwork`'ed ggplots: left density, right ROC curve.
#' @param length.out
#' See [plot_density_empirical()]. Granularity of density plot (pure visualization).
#' @param xmin
#' See [plot_density_empirical()].
#' @param xmax
#' See [plot_density_empirical()].
#' @param ...
#' Further arguments to [perf_ROC()]
#' @export
#'
#' @examples
#'
#' sim_samples <- sim(
#'     list(
#'         "dist_1" = function(x) rnorm(x, mean = 1, sd = 1),
#'         "dist_2" = function(x) rnorm(x, mean = 0, sd = 1)
#'     ),
#'     do_melt = FALSE,
#'     length.out = 1000
#' )
#' plot_density_ROC_empirical(sim_samples)
plot_density_ROC_empirical <- function(values_grouped,
                                       length.out = 500, xmin = -3, xmax = 3,
                                       ...) {
    sim_samples_melted <- melt_gendata(values_grouped)
    tmp_perf <- perf_ROC(
        response = sim_samples_melted[["Distribution"]],
        predictor = sim_samples_melted[["Value"]],
        ...
    )
    plotted_roc <- plot_ROC(tpr = tmp_perf[["perf"]]$tpr, fpr = tmp_perf[["perf"]]$fpr)
    plotted_density <- plot_density_empirical(
        values_grouped,
        xmin = xmin, xmax = xmax,
        length.out = length.out,
        positive_label = tmp_perf$positive_label,
        density_values_or_function = "values"
    )
    return(patchwork::wrap_plots(plotted_density, plotted_roc))
}

#' Plot 4 rROC plots
#'
#' 	A, Top-left:		Given positive and negative distributions, plot the corresponding densities.
#' 	B, Top-right:		Corresponding ROC curve
#' 	C, Bottom-left:		False positive rate VS threshold
#' 	D, Bottom-right:	rzAUC plot for markerHIGH and markerLOW restricted parts, the rzAUC is shown
#'
#' 	For further description see our publication on restricted ROC curves.
#'
#'
#' @param single_rROC
#'  Optional, if not given, rROC will be calculated on the fly.
#' @param values_grouped
#'  List-like element. (also data.frames or matrices) where the elements are the
#'  samples from the different distributions.
#'  E.g.:
#'      # A tibble: 1,000 × 2
#'         dist_1 dist_2
#'          <dbl>  <dbl>
#'       1  0.974  0.257
#'       2  0.196 -0.780
#'       3 -0.125 -0.264
#'       4  0.701  0.260
#' @param length.out
#' See [plot_density_empirical()]. Granularity of density plot (pure visualization).
#' @param xmin
#' See [plot_density_empirical()].
#' @param xmax
#' See [plot_density_empirical()].
#' @param positive_label
#' Positive label for calculating ROC curve and the densities. If NULL, will be
#' automatically generated from the data.
#' @param direction
#' See [pROC::roc()], but only "<" implemented.
#' @param part_colors
#' Default: default_part_colors
#' A vector c("high"=COLOR_A, "low"=COLOR_B) with colors for high-value (markerHIGH)
#' and low-value (markerLOW) parts of calculating the restricted ROC.
#' @param plot_thresholds
#' Plot the optimal restrictions for high/low part and the max_total (best overall)
#' @param plot_thresholds_fpr
#' Plot the max_total threshold on plots B, C and D
#' @return `patchwork`'ed ggplots, see Description for their meaning.
#'
#' @export
#' @examples
#'
#' sim_samples <- sim(
#'     list(
#'         "dist_1" = function(x) rnorm(x, mean = 1, sd = 1),
#'         "dist_2" = function(x) rnorm(x, mean = 0, sd = 1)
#'     ),
#'     do_melt = FALSE,
#'     length.out = 50
#' )
#' plot_density_rROC_empirical(sim_samples)
#' plot_density_rROC_empirical(sim_samples, positive_label = "dist_1")
#' plot_density_rROC_empirical(sim_samples, positive_label = "dist_2")
#' plot_density_rROC_empirical(sim_samples, part_colors = c("high" = "green", "low" = "yellow"))
#' plot_density_rROC_empirical(sim_samples, plot_thresholds = FALSE)
#' plot_density_rROC_empirical(sim_samples, plot_thresholds_fpr = FALSE)
#'
#'
#'
#' tmp <- simple_rROC(
#'     response = melt_gendata(sim_samples)[["Distribution"]],
#'     predictor = melt_gendata(sim_samples)[["Value"]],
#'     direction = "<",
#'     positive_label = "dist_1",
#'     return_proc = TRUE
#' )
#' single_rROC <- simple_rROC_interpret(tmp)
#' plot_density_rROC_empirical(sim_samples, single_rROC = single_rROC)
#'
plot_density_rROC_empirical <- function(values_grouped,
                                        length.out = 500, xmin = NA, xmax = NA,
                                        single_rROC = NULL,
                                        positive_label = NULL,
                                        direction = "<",
                                        part_colors = default_part_colors,
                                        plot_thresholds = TRUE,
                                        plot_thresholds_fpr = TRUE) {
    threshold <- fpr_global <- NULL # linting
    sim_samples_melted <- melt_gendata(values_grouped)
    if (is.null(single_rROC)) {
        tmp <- simple_rROC(
            response = sim_samples_melted[["Distribution"]],
            predictor = sim_samples_melted[["Value"]],
            direction = direction,
            positive_label = positive_label,
            return_proc = TRUE
        )
        single_rROC <- simple_rROC_interpret(tmp)
    }
    plotted_roc <- plot_rROC(
        single_rROC,
        split_roc_score = TRUE, # became deprecated 2023-03-07
        part_colors = part_colors
    )

    if (is.na(xmin)) {
        xmin <- min(unlist(values_grouped))
    }
    if (is.na(xmax)) {
        xmax <- max(unlist(values_grouped))
    }
    plotted_density <- plot_density_empirical(
        values_grouped,
        xmin = xmin, xmax = xmax,
        length.out = length.out,
        positive_label = single_rROC$positive_label
    )

    max_thresholds <- lapply(
        single_rROC[-which(names(single_rROC) %in% c("performances", "positive_label"))],
        function(x) x[["threshold"]]
    )

    plotted_density_maxthresholds <- plotted_density
    if (plot_thresholds) {
        # plot vertical lines
        for (name_x in c("keep_lows", "keep_highs")) {
            if (!is.null(max_thresholds[[name_x]])) {
                plotted_density_maxthresholds <- plotted_density_maxthresholds +
                    ggplot2::geom_vline(xintercept = max_thresholds[[name_x]])
            }
        }
        plotted_density_maxthresholds <- plotted_density_maxthresholds +
            ggplot2::geom_vline(xintercept = max_thresholds[["max_total"]], col = "red")

        # plot labels
        for (name_x in c("keep_lows", "keep_highs")) {
            if (!is.null(max_thresholds[[name_x]])) {
                plotted_density_maxthresholds <- plotted_density_maxthresholds +
                    ggplot2::annotate(x = max_thresholds[[name_x]], y = +Inf, label = name_x, vjust = 1, geom = "label")
            }
        }
        # max_total is plotted a bit deeper + smaller
        plotted_density_maxthresholds <- plotted_density_maxthresholds +
            ggplot2::annotate(
                x = max_thresholds[["max_total"]], y = +Inf, label = "max_total",
                vjust = 4, geom = "text", size = 3, col = "red"
            )
    }

    plot_x_fpr <- ggplot(
        single_rROC[["performances"]],
        aes(x = threshold, y = fpr_global)
    ) +
        geom_point() +
        ylab("False positive rate") +
        xlab("x threshold") +
        ggpubr::theme_pubr()

    complete_plotlist <- c(list("density" = plotted_density_maxthresholds, "threshold_fpr" = plot_x_fpr), plotted_roc)

    if (plot_thresholds_fpr) {
        max_th <- single_rROC$max_total$threshold
        max_rzauc <- single_rROC$max_total$rzAUC
        max_fpr <- single_rROC$performances[single_rROC$performances$threshold == max_th, ][["fpr_global"]]
        max_tpr <- single_rROC$performances[single_rROC$performances$threshold == max_th, ][["tpr_global"]]

        complete_plotlist[["threshold_fpr"]] <- complete_plotlist[["threshold_fpr"]] +
            geom_segment(x = max_th, xend = max_th, y = -Inf, yend = max_fpr, col = "red") +
            geom_segment(x = -Inf, xend = max_th, y = max_fpr, yend = max_fpr, col = "red")
        complete_plotlist[["roc"]] <- complete_plotlist[["roc"]] +
            geom_segment(x = max_fpr, xend = max_fpr, y = -Inf, yend = max_tpr, col = "red")
        complete_plotlist[["rzAUC"]] <- complete_plotlist[["rzAUC"]] +
            geom_segment(x = max_fpr, xend = max_fpr, y = -Inf, yend = max_rzauc, col = "red")
    }

    if (length(complete_plotlist) == 4) {
        design <- "AC\nBD"
        names(complete_plotlist) <- c("A", "B", "C", "D")
        wrapped_plots <- patchwork::wrap_plots(complete_plotlist) + patchwork::plot_layout(design = design)
    } else {
        wrapped_plots <- patchwork::wrap_plots(complete_plotlist)
    }

    return(
        list(
            "plots" = wrapped_plots,
            "single_rROC" = single_rROC
        )
    )
}
