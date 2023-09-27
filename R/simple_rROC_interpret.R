#' restricted ROC interpretation
#'
#' Interpret the results of restriction, e.g. make them nicer to read and further
#' process.
#'
#' @param perf_rroc
#' Result from simple_rROC
#' @param use_abs_rzAUC
#' One of the results is the restriction with the maximum rzAUC value.
#' If TRUE, report the absolute highest, otherwise the general highest
#' including the sign.
#' @return
#' 	S3-class restrictedROC of
#'      - performances
#'          previously reported perf_rroc result, previously seen as "joined_aucs"
#'      - global
#'          AUC, AUC variance under H0, rzAUC and asymptotic pvalue
#'
#'          when including all samples. This pvalue is now appropriately usable according to
#'      - keep_highs
#'          AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold
#'
#'          Threshold is the restriction value, all other values the corresponding
#'          performances for the highest (potentially absolute) rzAUC when keeping high
#'          values (markerHIGH)
#'      - keep_lows
#'          AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold
#'
#'          Threshold is the restriction value, all other values the corresponding
#'          performances for the highest (potentially absolute) rzAUC when keeping low
#'          values (markerLOW)
#'      - max_total
#'          AUC, AUC variance under H0, rzAUC, asymptotic pvalue, threshold, part
#'
#'          Threshold is the restriction value, all other values the corresponding
#'          performances for the highest (potentially absolute) rzAUC overall.
#'          This compares "global", "keep_highs" and "keep_lows", therefore `part`
#'          denotes where the result was coming from.
#'      - positive_label
#'          What is the positive label
#'      - pROC_full
#'          [pROC::roc()] result of the full dataset, only if `return_pROC` was TRUE
#' @export
#'
#' @examples
#' data(aSAH, package = "pROC")
#' tmp <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' simple_rROC_interpret(tmp)
#' simple_rROC_interpret(tmp, use_abs_rzAUC = FALSE)
#'
simple_rROC_interpret <- function(perf_rroc, use_abs_rzAUC = TRUE) {
    proc_full <- NULL
    if ("pROC_full" %in% names(perf_rroc)) {
        proc_full <- perf_rroc[["pROC_full"]]
    }
    positive_label <- perf_rroc[["positive_label"]]
    if ("joined_aucs" %in% names(perf_rroc)) {
        perf_rroc <- perf_rroc[["joined_aucs"]]
    }
    full_perf <- perf_rroc[1, ]
    if (use_abs_rzAUC) {
        max_rzauc_perf_highpart <- perf_rroc[which.max(abs(perf_rroc[["rzAUC_high"]])), ]
        max_rzauc_perf_lowpart <- perf_rroc[which.max(abs(perf_rroc[["rzAUC_low"]])), ]
    } else {
        max_rzauc_perf_highpart <- perf_rroc[which.max(perf_rroc[["rzAUC_high"]]), ]
        max_rzauc_perf_lowpart <- perf_rroc[which.max(perf_rroc[["rzAUC_low"]]), ]
    }

    part_global <- data.frame(
        "auc" = full_perf[["auc_high"]],
        "auc_var_H0" = full_perf[["auc_high"]],
        "rzAUC" = full_perf[["rzAUC_high"]],
        "pval_asym" = full_perf[["pval_asym_high"]]
    )

    part_res <- data.frame(
        "auc" = NA,
        "auc_var_H0" = NA,
        "rzAUC" = NA,
        "pval_asym" = NA,
        "threshold" = NA
    )

    part_low <- part_res
    if (nrow(max_rzauc_perf_lowpart) != 0) {
        for (name_x in names(part_res)) {
            x <- max_rzauc_perf_lowpart[[paste0(name_x, "_low")]]
            if (is.null(x)) {
                x <- max_rzauc_perf_lowpart[[name_x]]
            }
            part_low[[name_x]] <- x
        }
    }
    part_high <- part_res
    if (nrow(max_rzauc_perf_highpart) != 0) {
        for (name_x in names(part_res)) {
            x <- max_rzauc_perf_highpart[[paste0(name_x, "_high")]]
            if (is.null(x)) {
                x <- max_rzauc_perf_highpart[[name_x]]
            }
            part_high[[name_x]] <- x
        }
    }

    if (nrow(max_rzauc_perf_highpart) == 0 && nrow(max_rzauc_perf_lowpart) == 0) {
        max_rzauc <- NA
        max_rzauc_part <- "global"
    } else if (nrow(max_rzauc_perf_highpart) == 0) {
        max_rzauc <- max_rzauc_perf_lowpart
        max_rzauc_part <- "low"
    } else if (nrow(max_rzauc_perf_lowpart) == 0) {
        max_rzauc <- max_rzauc_perf_highpart
        max_rzauc_part <- "high"
    } else {
        if (use_abs_rzAUC) {
            comparison <- abs(max_rzauc_perf_lowpart[["rzAUC_low"]]) > abs(max_rzauc_perf_highpart[["rzAUC_high"]])
        } else {
            comparison <- max_rzauc_perf_lowpart[["rzAUC_low"]] > max_rzauc_perf_highpart[["rzAUC_high"]]
        }
        if (comparison) {
            max_rzauc <- max_rzauc_perf_lowpart
            max_rzauc_part <- "low"
        } else {
            max_rzauc <- max_rzauc_perf_highpart
            max_rzauc_part <- "high"
        }
    }

    part_total <- part_res
    part_total[["part"]] <- max_rzauc_part
    if (!all(is.na(max_rzauc))) {
        for (name_x in names(part_res)) {
            x <- max_rzauc[[paste0(name_x, "_", max_rzauc_part)]]
            if (is.null(x)) {
                x <- max_rzauc[[name_x]]
            }
            part_total[[name_x]] <- x
        }
        # if the number of samples in the selected part is the same as the full number of cases+controls,
        # we have _actually_ NOT selected anything, therefore it is GLOBAL!
        if (
            sum(max_rzauc[paste0(c("positives_", "negatives_"), max_rzauc_part)]) ==
                sum(full_perf[["positives_high"]] + full_perf[["negatives_high"]])
        ) {
            part_total[["part"]] <- "global"
        }
    }
    res <- list(
        "performances" = perf_rroc,
        "global" = part_global,
        "keep_highs" = part_high,
        "keep_lows" = part_low,
        "max_total" = part_total,
        "positive_label" = positive_label
    )
    res[["pROC_full"]] <- proc_full
    class(res) <- c(class(res), "restrictedROC")
    return(res)
}
