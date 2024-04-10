#' restricted ROC
#'
#' Calculate the restricted ROC curves.
#' @param response
#' A vector containing the true class labels. Care that it is VERY important
#' which class is the positive class because the _predictions_ are ordered according
#' to `restriction`
#' @param predictor
#' A vector containing the predictions.
#' @param direction
#' See [pROC::roc()], but only "<" is implemented right now. Maybe changing the
#' positive_label already solves your problem.
#' @param positive_label
#'  Label for the positive class. All other values of `response` are regarded as negative cases.
#' @param get_all_aucs_fun
#' How to calculate the AUCs. You would usually now want to set that.
#' Implemented are
#' 		`get_all_aucs()`:
#' 			Calculates the AUCs by actively splitting the data into markerHIGH
#' 			and markerLOW parts. Then calculates a usual AUC on the parts.
#'
#' 		`get_all_aucs_norecalculation()`:
#' 			Calculates the AUCs based on the scaling factor described in the
#' 			publication. Much faster after the ROC curve does not have to
#' 			be recalculated over and over again.
#' 			Todo: Could potentially be improved by not recalculating the partial
#' 			AUCs with pROC over and over but by just adding parts.
#'
#' @param return_proc
#' 	1) Should pROC::roc() be returned for the full dataset?
#' 	2) Should pROC::roc() be returned on each of the part datasets? Only works with
#' 	`get_all_aucs_fun=get_all_aucs` after  `get_all_aucs_norecalculation()` does
#' 	not calculate the ROC curves for each restriction separately.
#' @param do_parallel
#' 	`get_all_aucs()` has parallelization enabled, but for some reason it seemed
#' 	to not improve the speed of the calculation. Therefore throws an error.
#' 	`get_all_aucs_norecalculation()` does not use it at all.
#' @param check_positive_negative_count
#' 	Pure checking/testing parameter, you would not set that TRUE anytime.
#' 	Just enables checks if the number of positives/negatives was extracted
#' 	correctly for the restrictions
#'
#' @return
#' List of two elements:
#'
#' 	"positive_label": Label of the positive class
#' 	"joined_aucs": Table with the following columns:
#'      threshold
#'          The threshold which was used as restriction value.
#'          "high"-part is always >= threshold
#'          "low"-part is always   < threshold
#'      auc_high
#'          restricted AUC for the high part, including the scaling factor
#'      positives_high
#'          How many positives are in the restricted range of high values
#'      negatives_high
#'          How many negatives are in the restricted range of high values
#'      scaling_high
#'          Scaling factor which is multiplied with the actual partial
#'          area under the curve to obtain the "recalculated" area under
#'          the curve if it was RE-calculated on the samples being in the
#'          restricted range of high values
#'      auc_var_H0_high
#'          Estimated variance under the nullhypothesis using
#'          \deqn{\frac{n_{positives} + n_{negatives} + 1}{12\cdot n_{positives} \cdot n_{negatives}}}
#'      rzAUC_high
#'          restricted standardized AUC, obtained via
#'          \deqn{\frac{auc_high - .5}{\sqrt{auc_var_H0_high}}}
#'      pval_asym_onesided_high
#'          Asymptotic, onesided (is AUC bigger) p-value of the restricted standardized AUC,
#'          obtained via:
#'          \deqn{1 - pnorm(full_df[["rzAUC"]])}
#'          Here the requirements are not fullfilled, use with utmost caution!
#'      pval_asym_high
#'          Asymptotic, twosided (is AUC different) p-value of the restricted standardized AUC,
#'          obtained via:
#'          \deqn{(1 - pnorm(abs(full_df[["rzAUC"]]))) * 2}
#'          Here the requirements are not fullfilled, use with utmost caution!
#'      auc_low
#'          restricted AUC for the low part, including the scaling factor
#'      positives_low
#'          How many positives are in the restricted range of low values
#'      negatives_low
#'          How many negatives are in the restricted range of low values
#'      scaling_low
#'          Scaling factor which is multiplied with the actual partial
#'          area under the curve to obtain the "recalculated" area under
#'          the curve if it was RE-calculated on the samples being in the
#'          restricted range of low values
#'      auc_var_H0_low
#'          Estimated variance under the nullhypothesis using
#'          \deqn{\frac{n_{positives} + n_{negatives} + 1}{12\cdot n_{positives} \cdot n_{negatives}}}
#'      rzAUC_low
#'          restricted standardized AUC, obtained via
#'          \deqn{\frac{auc_low - .5}{\sqrt{auc_var_H0_low}}}
#'      pval_asym_onesided_low
#'          Asymptotic, onesided (is AUC bigger) p-value of the restricted standardized AUC,
#'          obtained via:
#'          \deqn{1 - pnorm(full_df[["rzAUC"]])}
#'          Here the requirements are not fullfilled, use with utmost caution!
#'      pval_asym_low
#'          Asymptotic, twosided (is AUC different) p-value of the restricted standardized AUC,
#'          obtained via:
#'          \deqn{(1 - pnorm(abs(full_df[["rzAUC"]]))) * 2}
#'          Here the requirements are not fullfilled, use with utmost caution!
#'      tp
#'          Number of true positives at that threshold including all samples
#'      fp
#'          Number of false positives at that threshold including all samples
#'      tpr_global
#'          True positive rate at that threshold including all ("global") samples
#'      fpr_global
#'          False positive rate at that threshold including all ("global") samples
#' @export
#'
#' @examples
#'
#' data(aSAH, package = "pROC")
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     positive_label = "Poor"
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     positive_label = "Good"
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     get_all_aucs_fun = restrictedROC:::get_all_aucs_norecalculation
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     get_all_aucs_fun = restrictedROC:::get_all_aucs
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     get_all_aucs_fun = restrictedROC:::get_all_aucs,
#'     return_proc = TRUE
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     check_positive_negative_count = TRUE
#' )
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     do_parallel = TRUE
#' )
#' \dontrun{
#' simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     get_all_aucs_fun = restrictedROC:::get_all_aucs,
#'     do_parallel = TRUE
#' )
#' }
simple_rROC <- function(response,
                        predictor,
                        direction = "<",
                        positive_label = NULL,
                        get_all_aucs_fun = get_all_aucs_norecalculation,
                        return_proc = FALSE,
                        do_parallel = FALSE,
                        check_positive_negative_count = FALSE) {
    if (is.null(positive_label)) {
        if (is.factor(response)) {
            positive_label <- levels(response)[length(levels(response))]
            message(paste0("Positive label not given, setting to last level of factor: ", positive_label))
        } else {
            positive_label <- max(response)
            message(paste0("Positive label not given, setting to max(response): ", positive_label))
        }
    }
    response <- response == positive_label
    true_pred_df <- data.frame("true" = response, "pred" = predictor)
    if (direction != "<") {
        stop("NotImplemented, please use direction='<'")
    }
    # nolint start
    # if (direction == "auto") {
    #     # ">":
    #     #   if the predictor values for the control group are higher than the values of the
    #     #   case group (controls > t >= cases).
    #     # "<":
    #     #    if the predictor values for the control group are lower or equal than the values of
    #     #    the case group (controls < t <= cases).
    #     direction <- ifelse(
    #         median(true_pred_df[response, ][["pred"]]) >
    #             median(true_pred_df[!response, ][["pred"]]),
    #         "<", ">"
    #     )
    #     message(paste(
    #         "Direction set 'auto': ",
    #         paste0("(not ", positive_label, ")"),
    #         direction,
    #         positive_label
    #     ))
    # }
    # nolint end

    full_roc <- pROC::roc(
        response = true_pred_df[["true"]],
        predictor = true_pred_df[["pred"]],
        direction = direction,
        levels = c(FALSE, TRUE),
    )
    full_coord <- pROC::coords(full_roc, "all", ret = c("tp", "fp", "tpr", "fpr", "threshold"))

    old_proc <- return_proc
    if (identical(get_all_aucs_fun, get_all_aucs_norecalculation) && old_proc) {
        return_proc <- FALSE
    }
    aucs_highpart <- get_all_aucs_fun(
        full_roc = full_roc,
        true_pred_df = true_pred_df,
        thresholds = full_coord[["threshold"]],
        high_low = "high",
        direction = direction,
        return_proc = return_proc,
        do_parallel = do_parallel,
        check_positive_negative_count = check_positive_negative_count
    )
    aucs_lowpart <- get_all_aucs_fun(
        full_roc = full_roc,
        true_pred_df = true_pred_df,
        thresholds = full_coord[["threshold"]],
        high_low = "low",
        direction = direction,
        return_proc = return_proc,
        do_parallel = do_parallel,
        check_positive_negative_count = check_positive_negative_count
    )
    if (identical(get_all_aucs_fun, get_all_aucs_norecalculation) && old_proc) {
        return_proc <- old_proc
    }

    joined_aucs <- dplyr::full_join(
        aucs_highpart[["perf"]],
        aucs_lowpart[["perf"]],
        by = "threshold",
        suffix = c("_high", "_low")
    )
    joined_aucs <- tibble::as_tibble(dplyr::full_join(joined_aucs, full_coord, by = "threshold"))
    colnames(joined_aucs)[(ncol(joined_aucs) - 1):ncol(joined_aucs)] <- paste0(
        colnames(joined_aucs)[(ncol(joined_aucs) - 1):ncol(joined_aucs)], "_global"
    )
    if (!return_proc) {
        reslist <- list(
            "joined_aucs" = joined_aucs,
            "positive_label" = positive_label
        )
    } else {
        reslist <- list(
            "joined_aucs" = joined_aucs,
            "positive_label" = positive_label,
            "pROC_lowpart" = aucs_lowpart[["pROC"]],
            "pROC_highpart" = aucs_highpart[["pROC"]],
            "pROC_full" = full_roc
        )
    }

    class(reslist) <- c("simple_rROC", class(reslist))
    return(reslist)
}


get_all_aucs_presto <- function(true_pred_df,
                                thresholds,
                                direction,
                                high_low = "high",
                                return_proc = FALSE,
                                ...) {
    lapply_fun <- lapply

    res <- lapply_fun(thresholds, function(threshold_x) {
        if (high_low == "high") {
            part_df <- true_pred_df[true_pred_df[["pred"]] >= threshold_x, ]
        } else {
            part_df <- true_pred_df[true_pred_df[["pred"]] < threshold_x, ]
        }

        wilcox_res <- NULL
        wilcox_res <- tryCatch(
            {
                # presto:::wilcoxauc.default
                wilcox_res <- presto::wilcoxauc(
                    X = t(part_df[["pred"]]),
                    y = part_df[["true"]]
                )
                wilcox_res |>
                    dplyr::filter(group == "TRUE") |>
                    dplyr::mutate(threshold = threshold_x, .before = 1)
            },
            error = function(err) {
                one_th_wc <- data.frame(
                    # "feature"   "group"     "avgExpr"   "logFC"     "statistic" "auc" "pval"      "padj"      "pct_in"    "pct_out"
                    "threshold" = threshold_x,
                    "feature" = NA,
                    "group" = NA,
                    "avgExpr" = NA,
                    "logFC" = NA,
                    "statistic" = NA,
                    "auc" = NA,
                    "pval" = NA,
                    "padj" = NA,
                    "pct_in" = NA,
                    "pct_out" = NA
                )
                one_th_wc
            }
        )
        wilcox_res[["positives"]] <- sum(part_df$true)
        wilcox_res[["negatives"]] <- nrow(part_df) - wilcox_res[["positives"]]
        return(wilcox_res)
    })
    full_procs <- tibble::as_tibble(do.call(rbind, res))
    target_output_columns <- c("threshold", "auc", "positives", "negatives", "auc_var_H0", "rzAUC", "pval_asym_onesided", "pval_asym")

    full_df <- tibble::tibble(
        threshold = full_procs$threshold,
        auc = full_procs$auc,
        positives = full_procs$positives,
        negatives = full_procs$negatives,
        rzAUC = full_procs$statistic,
        pval_asym_onesided = NA_real_,
        pval_asym = full_procs$pval
    )
    return(list("perf" = full_df, "pROC" = NA))
}

get_all_aucs <- function(true_pred_df,
                         thresholds,
                         direction,
                         high_low = "high",
                         return_proc = FALSE,
                         do_parallel = FALSE,
                         ...) {
    if (do_parallel) {
        stop("Applying parallel seems to be _slower_ for whatever reason, do not do this here.")
        if (!"future" %in% .packages()) {
            warning("Did you create a future 'plan()'? Running sequentially.")
        }
        lapply_fun <- function(x, FUN) {
            future.apply::future_lapply(
                x, FUN,
                future.globals = c("true_pred_df" = true_pred_df, "direction" = direction)
            )
        }
    } else {
        lapply_fun <- lapply
    }
    res <- lapply_fun(thresholds, function(threshold_x) {
        if (high_low == "high") {
            part_df <- true_pred_df[true_pred_df[["pred"]] >= threshold_x, ]
        } else {
            part_df <- true_pred_df[true_pred_df[["pred"]] < threshold_x, ]
        }
        part_roc <- NULL
        one_threshold_auc <- tryCatch(
            {
                part_roc <- pROC::roc(
                    response = part_df[["true"]],
                    predictor = part_df[["pred"]],
                    direction = direction,
                    levels = c(FALSE, TRUE),
                )
                inner_one_threshold_auc <- data.frame(
                    "threshold" = threshold_x,
                    "auc" = as.numeric(part_roc[["auc"]]),
                    "positives" = length(part_roc[["cases"]]),
                    "negatives" = length(part_roc[["controls"]])
                )
                inner_one_threshold_auc
            },
            error = function(err) {
                one_threshold_auc <- data.frame(
                    "threshold" = threshold_x,
                    "auc" = NA,
                    "positives" = sum(part_df[["true"]]),
                    "negatives" = sum(!part_df[["true"]])
                )
                return(one_threshold_auc)
            }
        )
        return(list("part_roc" = part_roc, "one_threshold_auc" = one_threshold_auc))
    })

    full_procs <- lapply(res, function(x) x[["part_roc"]])
    names(full_procs) <- as.character(thresholds)
    full_df <- do.call(rbind, lapply(res, function(x) x[["one_threshold_auc"]]))

    full_df <- calc_rzAUC(full_df)
    return(list("perf" = full_df, "pROC" = full_procs))
}

get_all_aucs_norecalculation <- function(full_roc, high_low = "high", check_positive_negative_count = FALSE, ...) {
    original.response <- original.predictor <- NULL # Just for "no visible binding"
    if (list(...)[["return_proc"]]) {
        warning("get_all_aucs_norecalculation() does not calculate single ROC curves, therefore cannot return them")
    }
    coords <- pROC::coords(full_roc, ret = c("threshold", "tp", "fp", "tpr", "fpr", "tn", "fn"))
    if (high_low == "high") {
        coords[["positives"]] <- coords[["tp"]]
        coords[["negatives"]] <- coords[["fp"]]
        if (check_positive_negative_count) {
            coords[["high_positives"]] <- vapply(full_roc[["thresholds"]], function(th) {
                with(full_roc, sum(original.response[original.predictor > th]))
            }, FUN.VALUE = numeric(1))
            coords[["high_negatives"]] <- vapply(full_roc[["thresholds"]], function(th) {
                with(full_roc, sum(!original.response[original.predictor > th]))
            }, FUN.VALUE = numeric(1))
            if (
                !all.equal(coords[["high_positives"]], coords[["positives"]]) ||
                    !all.equal(coords[["high_negatives"]], coords[["negatives"]])
            ) {
                stop("Counting of high positives/negatives went wrong")
            }
        }
    } else {
        coords[["positives"]] <- coords[["fn"]]
        coords[["negatives"]] <- coords[["tn"]]
        if (check_positive_negative_count) {
            coords[["low_positives"]] <- vapply(full_roc[["thresholds"]], function(th) {
                with(full_roc, sum(original.response[original.predictor <= th]))
            }, FUN.VALUE = numeric(1))
            coords[["low_negatives"]] <- vapply(full_roc[["thresholds"]], function(th) {
                with(full_roc, sum(!original.response[original.predictor <= th]))
            }, FUN.VALUE = numeric(1))
            if (
                !all.equal(coords[["low_positives"]], coords[["positives"]]) ||
                    !all.equal(coords[["low_negatives"]], coords[["negatives"]])) {
                stop("Counting of high positives/negatives went wrong")
            }
        }
    }
    coords[c("tp", "fp", "tn", "fn")] <- NULL

    # See examples/paper_07_rROC_vs_pROC and the resulting res/paper/proc_rroc_p07_pROC_parts.pdf
    # why we use these explicit values
    if (high_low == "high") {
        parts_left <- vapply(coords[["fpr"]], function(fpr) {
            as.numeric(
                pROC::auc(
                    full_roc,
                    partial.auc = c(1, 1 - fpr),
                    partial.auc.focus = "specificity",
                    partial.auc.correct = FALSE,
                    reuse.auc = FALSE
                )
            )
        }, FUN.VALUE = numeric(1))
        # nolint start
        # ##### Do the following plot to _see_ what area is calculated
        # fpr <- coords[50, ][["fpr"]]
        # pdf("removeme.pdf")
        # plot(full_roc,
        #     partial.auc = c(1, 1 - fpr),
        #     partial.auc.focus = "specificity",
        #     auc.polygon = TRUE,
        #     reuse.auc = FALSE,
        #     legacy.axes = TRUE
        # )
        # dev.off()
        # nolint end

        # Denominator is the area of the bottom-left part of the ROC curve corresponding to samples
        # with high values:
        # [0...fpr] * [0...tpr]
        parts_left_scaling <- with(coords, 1 / (fpr * tpr))
        auc_scaled <- parts_left * parts_left_scaling
    } else {
        parts_top <- vapply(coords[["tpr"]], function(tpr) {
            as.numeric(
                pROC::auc(
                    full_roc,
                    partial.auc = c(tpr, 1),
                    partial.auc.focus = "sensitivity",
                    partial.auc.correct = FALSE,
                    reuse.auc = FALSE
                )
            )
        }, FUN.VALUE = numeric(1))
        # nolint start
        ##### Do the following plot to _see_ what area is calculated
        # tpr <- coords[50, ][["sensitivity"]]
        # pdf("removeme.pdf")
        # plot(
        #     full_roc,
        #     auc.polygon = TRUE,
        #     partial.auc = c(tpr, 1),
        #     partial.auc.focus = "sensitivity",
        #     reuse.auc = FALSE,
        #     legacy.axes = TRUE
        # )
        # dev.off()
        # nolint end


        # Denominator is the area of the top-right part of the ROC curve corresponding to samples
        # with low values:
        # [1-fpr...1] * [1-tpr...1]
        parts_right_scaling <- with(coords, 1 / ((1 - fpr) * (1 - tpr)))
        auc_scaled <- parts_top * parts_right_scaling
    }
    full_df <- data.frame(
        "threshold" = coords[["threshold"]],
        "auc" = auc_scaled,
        "positives" = coords[["positives"]],
        "negatives" = coords[["negatives"]]
    )

    if (high_low == "high") {
        full_df[["scaling"]] <- parts_left_scaling
    } else {
        full_df[["scaling"]] <- parts_right_scaling
    }
    # To be consistent with get_all_aucs(), replace all NaN with NA
    full_mat <- as.matrix(full_df)
    full_mat[is.nan(full_mat)] <- NA

    full_df <- calc_rzAUC(tibble::as_tibble(full_mat))
    return(list("perf" = full_df, "pROC" = NA))
}


calc_rzAUC <- function(full_df) {
    # full_df:
    #       threshold       auc positives negatives
    # 1          -Inf 0.8341875        93       107
    # 2   0.006584433 0.8432548        92       107
    # 3   0.010199915 0.8417760        92       106
    # .............................................
    # 199 0.984633215        NA         2         0
    # 200 0.987881852        NA         1         0
    # 201         Inf        NA         0         0
    full_df[["auc_var_H0"]] <- (full_df[["positives"]] + full_df[["negatives"]] + 1.) /
        (12 * full_df[["positives"]] * full_df[["negatives"]])
    full_df[["rzAUC"]] <- (full_df[["auc"]] - .5) / sqrt(full_df[["auc_var_H0"]])
    # onesided, bigger
    full_df[["pval_asym_onesided"]] <- 1 - stats::pnorm(full_df[["rzAUC"]])
    # twosided
    full_df[["pval_asym"]] <- (1 - stats::pnorm(abs(full_df[["rzAUC"]]))) * 2
    return(full_df)
}
