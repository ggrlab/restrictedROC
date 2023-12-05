# All S3 methods must be exported https://github.com/r-lib/devtools/issues/2293

#' plot ROC including AUC significance
#'
#' Plot the complete ROC curve and the corresponding AUCs.
#'
#' @inheritParams plot_rROC_single
#' @param rROC_result
#' 	rROC result from \code{\link{simple_rROC}}, \code{\link{simple_rROC_interpret}} or \code{\link{rROC}}.
#'
#' @export
#'
#' @return
#' List of multiple ggplot elements, always "roc",
#' if split_roc_score=TRUE also a single "rzAUC" plot
#' @examples
#' # See also test-plot_rROC()
#' data(aSAH, package = "pROC")
#' simple_rROC_res <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' single_rROC <- simple_rROC_interpret(simple_rROC_res)
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = TRUE
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE,
#'     part = "high"
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE,
#'     part = "low"
#' )
#'
plot_rROC <- function(rROC_result,
                      col_rzAUC = "#999999",
                      part = c("high", "low"),
                      part_colors = default_part_colors,
                      split_roc_score = FALSE) {
    UseMethod("plot_rROC")
}
#' @export
plot_rROC.default <- function(rROC_result, ...) {
    plot_rROC.rROC(rROC_result, ...)
}
#' @export
plot_rROC.simple_rROC <- function(simple_rROC_result, ...) {
    plot_rROC_single(
        simple_rROC_interpret(simple_rROC_result),
        ...
    )
}

#' @export
plot_rROC.restrictedROC <- function(interpreted_simple_rROC_result, ...) {
    plot_rROC_single(
        interpreted_simple_rROC_result,
        ...
    )
}

#' @export
plot_rROC.rROC <- function(rROC_result, current_level = 0, title = "", ...) {
    if (all(is.null(rROC_result)) || all(is.na(rROC_result))) {
        return(NULL)
    } else if (!"permutation" %in% names(rROC_result)) {
        current_level <- current_level + 1
        l_results <- sapply(
            names(rROC_result), function(x) {
                plot_rROC.rROC(
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
        all_returned_plots <- plot_rROC_single(
            rROC_result[["permutation"]],
            ...
        )
        all_returned_plots_titles <- lapply(all_returned_plots, function(x) {
            x + ggplot2::ggtitle(title)
        })
        return(all_returned_plots_titles)
    }
}


#' plot ROC including AUC significance
#'
#' Plot the complete ROC curve and the corresponding AUCs.
#'
#' @param rROC_result
#' 	rROC result from:
#'
#' 		single_rROC <- simple_rROC_interpret(simple_rROC(
#'  		response = aSAH$outcome,
#'  		predictor = aSAH$ndka
#' 		))
#' @param col_rzAUC
#' Color for rzAUC points if split_roc_score=FALSE
#' @param part
#' "high", "low" or multiple
#' @param part_colors
#' Colors for the `part`
#' @param split_roc_score
#' Shoult the ROC score (rzAUC) be a separate plot ("TRUE") or inside the ROC
#' curve plot ("FALSE")
#'
#' @return
#' List of multiple ggplot elements, always "roc",
#' if split_roc_score=TRUE also a single "rzAUC" plot
#' @examples
#'
#' data(aSAH, package = "pROC")
#' simple_rROC_res <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' single_rROC <- simple_rROC_interpret(simple_rROC_res)
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = TRUE
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE,
#'     part = "high"
#' )
#' plot_rROC(
#'     single_rROC,
#'     split_roc_score = FALSE,
#'     part = "low"
#' )
#'
plot_rROC_single <- function(rROC_result,
                             col_rzAUC = "#999999",
                             part = c("high", "low"),
                             part_colors = default_part_colors,
                             split_roc_score = FALSE) {
    df <- rROC_result$perf
    roc_part <- plot_ROC(tpr = df[["tpr_global"]], fpr = df[["fpr_global"]])
    if (split_roc_score) {
        resplots <- list(
            "roc" = roc_part,
            "rzAUC" = plot_rzAUCs(
                rROC_result = rROC_result,
                part = part,
                part_colors = part_colors
            )
        )
        resplots <- lapply(resplots, function(x) {
            x +
                ggpubr::theme_pubr() +
                theme(
                    aspect.ratio = 1, # enforce square plot
                    legend.title = element_blank(),
                    legend.direction = "vertical"
                )
        })
    } else {
        if (length(part) > 1) {
            part <- part[1]
            warning(paste0("Multiple parts given, plot only the first (", part, ")"))
        }
        resplots <- list(
            "roc" = plot_rROC_joint(
                roc_part = roc_part,
                part = part,
                df = df,
                col_rzAUC = col_rzAUC
            ) +
                ggpubr::theme_pubr() +
                theme(
                    aspect.ratio = 1, # enforce square plot
                    axis.title.y.right = element_text(color = col_rzAUC, hjust = .8),
                    axis.line.y.right = element_blank(),
                    axis.text.y.right = element_text(color = col_rzAUC),
                    axis.ticks.y.right = element_line(color = col_rzAUC)
                )
        )
    }
    return(resplots)
}

#' Plot ROC curve with rzAUC values inside
#'
#' @param roc_part
#'  data.frame with true positive rate (tpr) and false positive rate (fpr)
#'  to plot the ROC curve
#' @param df
#' 	data.frame from single_rROC$perf where
#'
#' 		single_rROC <- simple_rROC_interpret(simple_rROC(
#' 		    response = aSAH$outcome,
#' 		    predictor = aSAH$ndka
#' 		))
#' @param part
#'  - "high" (for markerHIGH part, bottom-left part of ROC curve)
#'  - "low" (for markerLOW part, top-right part of ROC curve)
#' @param col_rzAUC
#' Color of the rzAUC points
#'
#' @return
#' ggplot object with two axes:
#' 	1. Usual ROC curve
#' 	2. The rzAUC in the same plot on the second axis
#'
#' @examples
#' # See plot_rROC()
plot_rROC_joint <- function(roc_part,
                            df,
                            part,
                            col_rzAUC) {
    rzAUC <- NULL # linting
    rzauc_name <- paste0("rzAUC_", part)
    max_abs_rzAUC <- max(abs(df[[rzauc_name]]), 2, na.rm = TRUE)
    max_rzAUC <- max(df[[rzauc_name]], 2, na.rm = TRUE)
    min_rzAUC <- min(0, df[[rzauc_name]], na.rm = TRUE)

    # https://stackoverflow.com/questions/49185583/two-y-axes-with-different-scales-for-two-datasets-in-ggplot2
    df_part <- df[, c("fpr_global", rzauc_name)]
    colnames(df_part) <- c("fpr_global", "rzAUC")
    return(
        roc_part +
            geom_point(
                data = df_part,
                aes(y = (rzAUC - min_rzAUC) / ((max_abs_rzAUC - min_rzAUC) * 2)),
                col = col_rzAUC,
                size = .8,
                na.rm = TRUE
            ) +
            scale_y_continuous(
                # Add a second axis and specify its features
                sec.axis = sec_axis(~ (. * ((max_abs_rzAUC - min_rzAUC) * 2)) + min_rzAUC,
                    name = "restricted standardized AUC",
                    breaks = labeling::extended(dmin = min_rzAUC, dmax = max_rzAUC, m = 5)
                )
            ) +
            geom_segment(
                aes(x = 1.05, xend = 1.05, y = -Inf, yend = .55),
                col = col_rzAUC, linewidth = .25
            ) +
            coord_cartesian(xlim = c(0, 1.01), clip = "off") +
            theme(
                axis.line.y.right = element_blank(),
                axis.title.y.right = element_text(hjust = .9)
            )
    )
}

#' Plot rzAUC vs FPR
#'
#' 	Plot rzAUCs for all calculated parts
#'
#' @param rROC_result
#' 	rROC result from:
#'
#' 		single_rROC <- simple_rROC_interpret(simple_rROC(
#'  		response = aSAH$outcome,
#'  		predictor = aSAH$ndka
#' 		))
#' @param part
#' vector which parts should be shown, can be multiple
#' @param part_colors
#' Named vector which part should receive which color
#'
#' @return
#' ggplot object with rzAUCs shown for all false positive rates. For one FPR
#' multiple values can exist.
#'
#' @examples
#' # See plot_rROC()
plot_rzAUCs <- function(rROC_result,
                        part = c("high", "low"),
                        part_colors = default_part_colors) {
    fpr_global <- rzAUC <- restrict_highlow <- NULL # linting
    df <- rROC_result$perf
    rzauc_part_names <- paste0("rzAUC_", part)
    df_long <- tidyr::pivot_longer(
        df,
        cols = rzauc_part_names,
        names_to = "restrict_highlow",
        values_to = "rzAUC"
    )
    p1 <- ggplot(
        df_long[!is.na(df_long["rzAUC"]), ],
        mapping = aes(x = fpr_global, y = rzAUC, col = restrict_highlow)
    ) +
        geom_point(size = .7) +
        xlab("False positive rate") +
        ylab("restricted standardized AUC") +
        theme(
            aspect.ratio = 1, # enforce square plot
            legend.title = element_blank()
        )
    if (!all(part %in% names(part_colors))) {
        warning("Not all part are in names(part_colors), color mapping not applied.")
        # all part must be in the names of part_colors.
        return(p1)
    } else {
        names(part_colors) <- paste0("rzAUC_", names(part_colors))
        return(
            p1 + scale_color_manual(values = part_colors)
        )
    }
}
