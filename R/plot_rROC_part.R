# All S3 methods must be exported https://github.com/r-lib/devtools/issues/2293

#' Plot (restricted) ROC curves
#'
#' Plot for every tpr/fpr combination:
#' - Full density
#' - Full ROC
#'       + selection rectangles (orange + blue)
#'       + Full AUC
#'       + orange+blue partial AUCs
#'       + scaling factor
#'           + name rectangle sides
#'       + orange+blue partial AUCs SCALED
#' - ROCs for selections
#'       + Full AUCs
#'       + Color rectangle around the plot
#' - Densities for selections
#'
#' @param x
#' rROC result including full_pROC.
#' @param p_full_density_ROC
#' Density plot from plot_density_ROC_empirical of the full data, if not given it is calculated from rROC_res
#' @param threshold
#' At which threshold should be split into high and low. Alternatively, give a false positive rate (fpr)
#' @param fpr
#' A false positive rate at which (approximately) the cutoff will be made
#' @param color_high
#' Color for high part
#' @param color_low
#' Color for low part
#' @param include_part_auc_text
#' Should the text-annotations be added or not?
#' @return
#' [patchwork]ed plots, see the description.
#' @export
#'
#' @examples
#'
#' library(restrictedROC)
#' data(aSAH, package = "pROC")
#' ret_procs <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     return_proc = TRUE
#' )
#' # pdf("removeme.pdf")
#' print(plot_rROC_part(ret_procs, fpr = .5))
#' # dev.off()
plot_rROC_part <- function(x,
                           p_full_density_ROC = NA,
                           threshold = NA,
                           fpr = NA,
                           color_high = default_part_colors["high"],
                           color_low = default_part_colors["low"],
                           include_part_auc_text = FALSE) {
    UseMethod("plot_rROC_part")
}
#' @export
plot_rROC_part.default <- function(x, ...) {
    plot_rROC_part.rROC(x, ...)
}
#' @export
plot_rROC_part.simple_rROC <- function(x, ...) {
    plot_rROC_part_single(
        x,
        ...
    )
}

#' @export
plot_rROC_part.restrictedROC <- function(x, ...) {
    plot_rROC_part_single(
        x,
        ...
    )
}

#' @export
plot_rROC_part.rROC <- function(x, current_level = 0, title = "", ...) {
    rROC_result <- x
    if (all(is.null(rROC_result)) || all(is.na(rROC_result))) {
        return(NULL)
    } else if (!"permutation" %in% names(rROC_result)) {
        current_level <- current_level + 1
        l_results <- sapply(
            names(rROC_result), function(x) {
                plot_rROC_part.rROC(
                    rROC_result[[x]],
                    current_level = current_level,
                    title = paste0(title, " --- ", x),
                    ...
                )
            },
            USE.NAMES = TRUE, simplify = FALSE
        )
        return(l_results)
    } else {
        all_returned_plots <- plot_rROC_part_single(
            rROC_result[["permutation"]],
            ...
        )
        if ("plotlist" %in% names(all_returned_plots)) {
            all_returned_plots[["plotlist"]] <- lapply(
                all_returned_plots[["plotlist"]], function(x) {
                    x + ggplot2::ggtitle(title)
                }
            )
        }
        all_returned_plots[
            !names(all_returned_plots) %in% c("plotlist")
        ] <- lapply(
            all_returned_plots[!names(all_returned_plots) %in% c("plotlist")], function(x) {
                x + ggplot2::ggtitle(title)
            }
        )
        return(all_returned_plots)
    }
}


plot_rROC_part_single <- function(x,
                                  p_full_density_ROC = NA,
                                  threshold = NA,
                                  fpr = NA,
                                  color_high = default_part_colors["high"],
                                  color_low = default_part_colors["low"],
                                  include_part_auc_text = FALSE) {
    rROC_res <- x
    if ("joined_aucs" %in% names(rROC_res)) {
        rroc_perf <- rROC_res[["joined_aucs"]]
    } else {
        rroc_perf <- rROC_res[["performances"]]
    }

    if (!"pROC_full" %in% names(rROC_res)) {
        stop("rROC_res the returned full ROC curve, usually given by 'return_proc=TRUE'.")
    }
    roc_data_df <- data.frame(
        "original.predictor" = rROC_res[["pROC_full"]][["original.predictor"]],
        "original.response" = rROC_res[["pROC_full"]][["original.response"]]
    )

    density_range <- c(
        "min" = min(roc_data_df[["original.predictor"]]),
        "max" = max(roc_data_df[["original.predictor"]])
    )
    if (all(is.na(p_full_density_ROC))) {
        p_full_density_ROC <- plot_density_ROC_empirical(
            values_grouped = split(roc_data_df[["original.predictor"]], roc_data_df[["original.response"]]),
            length.out = 1000,
            xmin = density_range["min"], xmax = density_range["max"],
            direction = "<",
            levels = c(FALSE, TRUE)
        )
    }

    if (!is.na(fpr)) {
        if (!fpr %in% rroc_perf[["fpr_global"]]) {
            # then use the closest possible fpr
            closest_i <- which.min(abs(rroc_perf[["fpr_global"]] - fpr))
            closest_fpr <- rroc_perf[["fpr_global"]][closest_i]
            warning(paste0("\nfpr \n  ", fpr, "\nnot found, using the closest instead:\n  ", closest_fpr, ""))
        } else {
            closest_i <- which(rroc_perf[["fpr_global"]] == fpr)
        }
    } else if (!is.na(threshold)) {
        rroc_ths <- rroc_perf[["threshold"]]
        if (!threshold %in% rroc_ths) {
            # then use the closest possible threshold
            closest_i <- which.min(abs(rroc_ths - threshold))
            closest_th <- rroc_ths[closest_i]
            warning(paste0(
                "\nThreshold \n  ", threshold,
                "\nnot found, using the closest instead:\n  ", closest_th, ""
            ))
        } else {
            closest_i <- which(rroc_ths == threshold)
        }
    } else {
        stop("Either threshold or fpr must be given")
    }

    # Make sure that only one row is used. Can happen if multiple thresholds have the same fpr.
    onerow <- rroc_perf[closest_i, ][1, ]
    p_full_density_restriction <- p_full_density_ROC
    p_full_density_restriction[[1]]$layers <- c(
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = -Inf,
            xmax = onerow[["threshold"]] - 0.005,
            col = default_part_colors["low"],
            alpha = 0.4, fill = NA, linewidth = 2
        ),
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = onerow[["threshold"]] + 0.005,
            xmax = Inf,
            col = default_part_colors["high"],
            alpha = 0.4, fill = NA, linewidth = 2
        ),
        p_full_density_restriction[[1]]$layers,
        geom_vline(xintercept = onerow[["threshold"]], col = "red", linewidth = 1)
    )
    p_full_density_restriction[[1]] <- p_full_density_restriction[[1]] +
        scale_y_continuous(labels = function(x) sprintf("%4.1f", x))
    p_full_density_restriction[[2]] <- p_full_density_restriction[[2]] +
        annotate("rect",
            xmin = 0, ymin = 0,
            xmax = onerow[["fpr_global"]],
            ymax = onerow[["tpr_global"]],
            fill = default_part_colors["high"],
            alpha = 0.4,
            col = default_part_colors["high"],
            linewidth = .4
        ) +
        annotate("rect",
            xmin = onerow[["fpr_global"]],
            ymin = onerow[["tpr_global"]],
            xmax = 1,
            ymax = 1,
            fill = default_part_colors["low"],
            alpha = 0.4,
            col = default_part_colors["low"],
            linewidth = .4
        ) +
        geom_segment(
            x = onerow[["fpr_global"]], xend = onerow[["fpr_global"]],
            y = 0, yend = onerow[["tpr_global"]], col = "red"
        ) +
        geom_segment(
            x = onerow[["fpr_global"]], xend = 1,
            y = onerow[["tpr_global"]], yend = onerow[["tpr_global"]], col = "red"
        ) +
        # https://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
        coord_cartesian(
            xlim = c(-0.01, 1), # This focuses the x-axis on the range of interest
            ylim = c(-0.01, 1), # This focuses the x-axis on the range of interest
            clip = "off" # This keeps the labels from disappearing
        )

    if (include_part_auc_text) {
        p_full_density_restriction[[2]] <- p_full_density_restriction[[2]] +
            annotate("text",
                x = 1, y = 1,
                label = paste0(
                    "AUC=", round(onerow[["auc_low"]] / onerow[["scaling_low"]], 2),
                    sprintf(" s=%6.1f", onerow[["scaling_low"]]),
                    "  rAUC=", round(onerow[["auc_low"]], 2)
                ),
                col = default_part_colors[["low"]],
                vjust = 0, hjust = 1, size = 3
            ) +
            annotate("text",
                x = 0, y = -0.05,
                label = paste0(
                    "AUC=", round(onerow[["auc_high"]] / onerow[["scaling_high"]], 2),
                    sprintf(" s=%6.1f", onerow[["scaling_high"]]),
                    "  rAUC=", round(onerow[["auc_high"]], 2)
                ),
                col = default_part_colors[["high"]],
                vjust = 0, hjust = 0, size = 3
            ) +
            annotate("text",
                x = -.05, y = 1.02,
                label = paste0("AUC ", round(as.numeric(pROC::auc(rROC_res[["pROC_full"]])), 2)),
                vjust = 0, hjust = 0
            )
    }

    data_high <- roc_data_df[roc_data_df[["original.predictor"]] > onerow[["threshold"]], ]
    data_low <- roc_data_df[roc_data_df[["original.predictor"]] <= onerow[["threshold"]], ]

    roc_high <- pROC::roc(
        response = data_high[["original.response"]],
        predictor = data_high[["original.predictor"]],
        direction = "<",
        levels = c(FALSE, TRUE)
    )
    roc_low <- pROC::roc(
        response = data_low[["original.response"]],
        predictor = data_low[["original.predictor"]],
        direction = "<",
        levels = c(FALSE, TRUE)
    )

    p_density_roc_high <- plot_density_ROC_empirical(
        values_grouped = split(data_high[["original.predictor"]], data_high[["original.response"]]),
        length.out = 1000,
        xmin = density_range["min"], xmax = density_range["max"],
        direction = "<",
        levels = c(FALSE, TRUE)
    )
    p_density_roc_high[[1]] <- p_density_roc_high[[1]] +
        scale_y_continuous(labels = function(x) sprintf("%4.1f", x)) +
        theme(legend.position = "none") +
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = onerow[["threshold"]],
            xmax = Inf,
            col = default_part_colors["high"],
            alpha = 0.4, fill = NA, linewidth = 1.5
        ) +
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = -Inf,
            xmax = onerow[["threshold"]],
            alpha = 1, fill = "white"
        )
    p_density_roc_low <- plot_density_ROC_empirical(
        values_grouped = split(data_low[["original.predictor"]], data_low[["original.response"]]),
        length.out = 1000,
        xmin = density_range["min"], xmax = density_range["max"],
        direction = "<",
        levels = c(FALSE, TRUE)
    )
    p_density_roc_low[[1]] <- p_density_roc_low[[1]] +
        scale_y_continuous(labels = function(x) sprintf("%4.1f", x)) +
        theme(legend.position = "none") +
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = onerow[["threshold"]],
            xmax = Inf,
            alpha = 1, fill = "white"
        ) +
        annotate("rect",
            ymin = -Inf, ymax = Inf,
            xmin = -Inf,
            xmax = onerow[["threshold"]],
            col = default_part_colors["low"],
            alpha = 0.4, fill = NA, linewidth = 1.5
        )

    all_plots <- list(
        p_full_density_restriction,
        p_density_roc_high +
            theme(panel.border = element_rect(colour = default_part_colors["high"], fill = NA, linewidth = 4)) +
            annotate("rect",
                ymin = -Inf, ymax = Inf,
                xmin = -Inf,
                xmax = Inf,
                fill = default_part_colors["high"],
                alpha = 0.4
            ),
        p_density_roc_low +
            theme(panel.border = element_rect(colour = default_part_colors["low"], fill = NA, linewidth = 4)) +
            annotate("rect",
                ymin = -Inf, ymax = Inf,
                xmin = -Inf,
                xmax = Inf,
                fill = default_part_colors["low"],
                alpha = 0.4
            )
    )
    all_plots_named <- list(
        A = all_plots[[1]][[1]],
        B = all_plots[[1]][[2]],
        C = all_plots[[2]][[1]],
        D = all_plots[[2]][[2]],
        E = all_plots[[3]][[1]],
        F = all_plots[[3]][[2]]
    )
    if (include_part_auc_text) {
        all_plots_named[["D"]] <- all_plots_named[["D"]] +
            annotate("text",
                x = -.05, y = 1,
                label = paste0("AUC ", round(as.numeric(pROC::auc(roc_high)), 2)),
                vjust = 1, hjust = 0
            )
        all_plots_named[["F"]] <- all_plots_named[["F"]] +
            annotate("text",
                x = -.05, y = 1,
                label = paste0("AUC ", round(as.numeric(pROC::auc(roc_low)), 2)),
                vjust = 1, hjust = 0
            )
    }
    layout <- "
##CD
ABCD
ABEF
##EF
"
    wrapped_plots <- patchwork::wrap_plots(
        all_plots_named,
        design = layout, widths = c(0.2, 0.3, 0.2, 0.3)
    )
    return(list("plotlist" = all_plots_named, "patchworked" = wrapped_plots))
}
