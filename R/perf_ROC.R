#' Performance of ROC curves
#'
#' @param response
#' A vector containing the true class labels. Care that it is VERY important
#' which class is the positive class because the _predictions_ are ordered according
#' to `restriction`
#' @param predictor
#' A vector containing the predictions.
#' @param how
#' Previously, I allowed "pROC" and "ROCR", but now I only allow "pROC" after it
#' is way faster.
#' @param positive_label
#' Label of the positive class
#' @param ...
#' Further arguments to [perf_pROC()]
#' @return
#' List of
#' 	perf:			Result of [perf_pROC()], comes from [pROC::coords()]
#' 	auc:			Area under the ROC curve, comes from [pROC::auc()]
#' 	positive_label: Label of the positive class
#' 	auc_variance:	Variance of the AUC under Nullhypothesis:
#' 					 \deqn{\frac{n_{positives} + n_{negatives} + 1}{12\cdot n_{positives} \cdot n_{negatives}}}
#' 	rzAUC:			restricted standardized AUC, combines the calculated AUC with the variance.
#'
#'
#' 	Looks then e.g. like:
#'      $perf
#'      # A tibble: 110 × 5
#'            tp    fp   tpr   fpr threshold
#'         <dbl> <dbl> <dbl> <dbl>     <dbl>
#'       1    41    72 1     1       -Inf
#'       2    41    71 1     0.986      3.44
#'       3    40    71 0.976 0.986      4.24
#'       4    40    70 0.976 0.972      4.82
#'       5    40    69 0.976 0.958      5.11
#'       6    39    69 0.951 0.958      5.18
#'       7    38    69 0.927 0.958      5.28
#'       8    37    69 0.902 0.958      5.68
#'       9    37    68 0.902 0.944      6.00
#'      10    37    67 0.902 0.931      6.15
#'      # … with 100 more rows
#'      # ℹ Use `print(n = ...)` to see more rows
#'
#'      $auc
#'      [1] 0.611958
#'
#'      $positive_label
#'      [1] "Poor"
#'
#'      $auc_variance
#'      [1] 0.003218157
#'
#'      $rzAUC
#'      [1] 1.973565
#'
#' @export
#' @examples
#'
#' data(aSAH, package = "pROC")
#' \dontrun{
#' # ROCR is not implemented anymore
#' perf_ROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     how = "ROCR"
#' )
#' }
#' perf_ROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     how = "pROC"
#' )
#' perf_ROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' a <- perf_ROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
perf_ROC <- function(response, predictor, how = "pROC", positive_label = NULL, ...) {
    if (length(how) > 1) {
        how <- how[1]
    }

    if (!is.null(positive_label)) {
        response <- response == positive_label
        positive_label <- TRUE
    }
    if (how == "ROCR") {
        # auc_tpr_fpr <- perf_ROCR(response = response, predictor = predictor, ...)
        stop("NotImplemented")
    } else if (how == "pROC") {
        auc_tpr_fpr <- perf_pROC(response = response, predictor = predictor, positive_label = positive_label, ...)
    }
    # From
    # Restricted ROC curves are useful tools to evaluate the performance of tumour markers
    # S Parodi et al. 2016
    # https://journals.sagepub.com/doi/full/10.1177/0962280212452199
    true_table <- table(response)
    auc_variance <- (sum(true_table) + 1) / (12. * prod(true_table))
    rzAUC <- (auc_tpr_fpr[["auc"]] - .5) / sqrt(auc_variance)
    return(
        c(
            auc_tpr_fpr,
            list(
                "auc_variance" = auc_variance,
                "rzAUC" = rzAUC
            )
        )
    )
}
#' Performance ROC curves (pROC)
#'
#' @inheritParams perf_ROC
#' @param coords_ret
#'  Coordinates from [pROC::coords()] which should be returned in the "perf"-listelement
#' @param quiet
#' Parameter of [pROC::roc]
#' @param ...
#' Further parameters to [pROC::roc]
#' @return
#' List of
#' 	perf:			Result of [perf_pROC()], comes from [pROC::coords()]
#' 	auc:			Area under the ROC curve, comes from [pROC::auc()]
#' 	positive_label: Label of the positive class
#'
#'
#'
#' @examples
#'
#' data(aSAH, package = "pROC")
#' restrictedROC:::perf_pROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' restrictedROC:::perf_pROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     levels = c("Poor", "Good"), # response-values for NEGATIVE and POSITIVE
#'     direction = "<" # predictions of positives are higher than negatives
#' )
#' restrictedROC:::perf_pROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     levels = c("Poor", "Good"), # response-values for NEGATIVE and POSITIVE
#'     direction = ">" # predictions of positives are higher than negatives
#' )
#' restrictedROC:::perf_pROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     levels = c("Good", "Poor"), # response-values for NEGATIVE and POSITIVE
#'     direction = "<" # predictions of positives are higher than negatives
#' )
#'
perf_pROC <- function(response, predictor, quiet = FALSE, coords_ret = c("tp", "fp", "tpr", "fpr", "threshold"), ...) {
    pred <- pROC::roc(
        response = response,
        predictor = predictor,
        quiet = quiet,
        ...
    )
    full_coord <- NULL
    if (length(coords_ret) >= 1) {
        full_coord <- pROC::coords(pred, "all", ret = coords_ret)
    }
    return(
        list(
            "perf" = tibble::as_tibble(full_coord),
            "auc" = as.numeric(pROC::auc(pred)),
            "positive_label" = pred$levels[2] # definition from pROC
        )
    )
}
