#' S3 class predict for class "restrictedROC"
#'
#' Predict new samples for a given restrictedROC result
#'
#' @param object
#' Result from [simple_rROC_interpret()], easiest calculated from simple_rROC(..., return_proc=TRUE)
#' @param newdata
#' The new dataframe which should be predicted
#' @param newdata_predictor_column
#' Column name or number of the predictor (What you use to predict the response)
#' @param newdata_response_column
#' Column name or number of the true response (/outcome)
#' @param pred_high_label
#' Label of predictions HIGHER than the cutoff (Youden-index)
#' @param pred_low_label
#' Label of predictions LOWER OR EQUAL than the cutoff (Youden-index)
#' @param original.response
#' If the original response cannot be loaded from `object`, you have to give the
#' original response.
#' @param original.predictor
#' If the original predictor cannot be loaded from `object`, you have to give the
#' predictor
#' @param ...
#'  Not used but keeps devtools::check() happy
#' @return
#' List of
#' 	table_full
#' 		Table of full predictions, classified using the Youden-Index. This is the
#' 		classical way of predicting.
#' 	table_restricted
#' 		Table of restricted predictions, classified using the Youden-Index.
#' 		Samples which were identified as "unclassifiable" with our restriction
#' 		method were removed.
#' 	pred
#' 		tibble with
#' 			"response": 	True response
#' 			"predictor":	Values to use as predictors
#' 			"keep": 		Should they be kept according to restriction
#' 			"prediction":	Youden-based predictions, regardless if kept or not
#' 	threshold_and_restriction
#' 		Vector with
#' 			"restriction":
#' 				The restriction value, decides together with `restriction_part`
#' 				which samples are kept.
#' 			"classification_threshold":
#' 				Values above will be classified as `pred_high_label`
#' 	restriction_part
#' 		Should "high", "low" or "global" (=all) samples be kept, therefore
#' 		samples with values higher or lower (or all) with respect to the restriction
#' 		are kept.
#'
#' 	tab_classifiable
#' 		Restricted table including the unclassifiable predictions, e.g.
#'
#' 		        	        response
#' 			prediction      	Poor Good
#' 				Poor              29   35
#' 				Good               8   34
#' 				unclassifiable     4    3
#'
#' @export
#'
#' @examples
#'
#' data(aSAH, package = "pROC")
#' tmp <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka,
#'     return_proc = TRUE
#' )
#' single_rROC <- simple_rROC_interpret(tmp)
#'
#' predict(
#'     single_rROC,
#'     newdata = aSAH,
#'     newdata_predictor_column = "ndka",
#'     newdata_response_column = "outcome",
#'     pred_high_label = "Poor",
#'     pred_low_label = "Good"
#' )
#'
#' tmp <- simple_rROC(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
#' single_rROC_noFullROC <- simple_rROC_interpret(tmp)
#' \dontrun{
#' predict(
#'     single_rROC_noFullROC,
#'     newdata = aSAH,
#'     newdata_predictor_column = "ndka",
#'     newdata_response_column = "outcome",
#'     pred_high_label = "Poor",
#'     pred_low_label = "Good"
#' )
#' }
#' predict(
#'     single_rROC_noFullROC,
#'     newdata = aSAH,
#'     newdata_predictor_column = "ndka",
#'     newdata_response_column = "outcome",
#'     pred_high_label = "Poor",
#'     pred_low_label = "Good",
#'     original.response = aSAH$outcome,
#'     original.predictor = aSAH$ndka
#' )
#'
predict.restrictedROC <- function(object,
                                  newdata,
                                  newdata_predictor_column = 1,
                                  newdata_response_column = 2,
                                  pred_high_label = 1,
                                  pred_low_label = 0,
                                  original.response = NA,
                                  original.predictor = NA,
                                  #   do_caret = TRUE  # when doing caret, S3 seems to break.
                                  ...) {
    single_rroc_result <- object
    restriction <- single_rroc_result[["max_total"]][["threshold"]]
    restriction_part <- single_rroc_result[["max_total"]][["part"]]
    if (!"pROC_full" %in% names(single_rroc_result)) {
        if (all(is.na(original.response)) || all(is.na(original.predictor))) {
            stop(
                paste0(
                    "You need to have calculated pROC_full such that original response",
                    " and predictor can be extracted, OR given original.response AND ",
                    "original.predictor"
                )
            )
        }
        original.response <- original.response == pred_high_label
    } else {
        original.response <- single_rroc_result[["pROC_full"]]$original.response
        original.predictor <- single_rroc_result[["pROC_full"]]$original.predictor
    }
    previous_df <- data.frame(
        "response" = original.response,
        "predictor" = original.predictor
    )

    response <- predictor <- pred_youden_low <- NULL # linting
    if (
        !pred_high_label %in% newdata[[newdata_response_column]] &&
            !pred_low_label %in% newdata[[newdata_response_column]]
    ) {
        print(newdata[[newdata_response_column]])
        print(pred_high_label)
        print(pred_low_label)
        stop("Response column is above, followed by the pred_high_label and pred_low_label, they do BOTH not match?")
    } else {
        pred_df <- tibble::tibble(
            response = factor(
                newdata[[newdata_response_column]],
                levels = c(pred_high_label, pred_low_label)
            ),
            predictor = newdata[[newdata_predictor_column]]
        )
    }
    if (any(is.na(pred_df))) {
        warning(
            sprintf(
                paste0(
                    "\nThere are %d missing RESPONSE values",
                    "\nThere are %d missing PREDICTOR values",
                    "\nOut of those: %d missing both",
                    "\nIn total: %d missing rows\n"
                ),
                sum(is.na(pred_df$response)),
                sum(is.na(pred_df$predictor)),
                sum(is.na(pred_df$response) & is.na(pred_df$predictor)),
                sum(apply(pred_df, 1, function(x) any(is.na(x))))
            )
        )
    }
    if (restriction_part == "low") {
        pred_df$keep <- pred_df$predictor <= restriction
        previous_df$keep <- previous_df$predictor <= restriction
    } else if (restriction_part == "high") {
        pred_df$keep <- pred_df$predictor > restriction
        previous_df$keep <- previous_df$predictor > restriction
    } else {
        pred_df$keep <- TRUE
        previous_df$keep <- TRUE
    }
    proc_auto_full <- pROC::roc(
        response = previous_df$response,
        predictor = previous_df$predictor,
        levels = c(FALSE, TRUE),
        direction = "auto",
        quiet = TRUE
    )
    classification_threshold_full <- pROC::coords(
        proc_auto_full,
        x = "best",
        input = "threshold",
        best.method = "youden"
    )[["threshold"]]
    classification_direction_full_control_X_case <- proc_auto_full$direction
    # prediction youden
    if (length(classification_threshold_full) > 1) {
        warning("FULL dataset: Multiple optimal youden cutoffs identified, using the smallest")
    }
    classification_threshold_full <- classification_threshold_full[1]

    pred_df$pred_full <- pred_df$predictor > classification_threshold_full
    if (classification_direction_full_control_X_case == ">") {
        pred_df$pred_full <- c("TRUE" = pred_low_label, "FALSE" = pred_high_label)[as.character(pred_df$pred_full)]
    } else {
        pred_df$pred_full <- c("TRUE" = pred_high_label, "FALSE" = pred_low_label)[as.character(pred_df$pred_full)]
    }
    pred_df$pred_full <- factor(pred_df$pred_full, levels = c(pred_high_label, pred_low_label))
    tab_full <- with(pred_df, table(pred_full, response))


    previous_kept_df <- previous_df[previous_df$keep, ]
    # Just using single_rroc_result[["pROC_full"]] would potentially lead to wrong thresholds
    # if the ROC-curve is  below the diagonal!
    proc_auto_kept <- pROC::roc(
        response = previous_kept_df$response,
        predictor = previous_kept_df$predictor,
        levels = c(FALSE, TRUE),
        direction = "auto",
        quiet = TRUE
    )
    classification_threshold_kept <- pROC::coords(
        proc_auto_kept,
        x = "best",
        input = "threshold",
        best.method = "youden"
    )[["threshold"]]
    classification_direction_kept_control_X_case <- proc_auto_kept$direction
    # prediction youden
    if (length(classification_threshold_kept) > 1) {
        warning("RESTRICTED dataset: Multiple optimal youden cutoffs identified, using the smallest")
    }
    classification_threshold_kept <- classification_threshold_kept[1]

    pred_df$pred_kept <- pred_df$predictor > classification_threshold_kept
    if (classification_direction_kept_control_X_case == ">") {
        pred_df$pred_kept <- c("TRUE" = pred_low_label, "FALSE" = pred_high_label)[as.character(pred_df$pred_kept)]
    } else {
        pred_df$pred_kept <- c("TRUE" = pred_high_label, "FALSE" = pred_low_label)[as.character(pred_df$pred_kept)]
    }
    pred_df$pred_kept <- factor(pred_df$pred_kept, levels = c(pred_high_label, pred_low_label))
    tab_restricted <- with(pred_df[pred_df$keep, ], table(pred_kept, response))

    res <- list("table_full" = tab_full, "table_restricted" = tab_restricted, "pred" = pred_df)
    # nolint start
    # if (do_caret) {
    #     res[["table_full_caret"]] <- caret::confusionMatrix(tab_full)
    #     res[["table_restricted_caret"]] <- caret::confusionMatrix(tab_restricted)
    # }
    # nolint end
    diffmat <- tab_full - tab_restricted
    if (all(diffmat == 0)) {
        tab_classifiable <- rbind(tab_restricted, 0)
    } else {
        tab_classifiable <- rbind(tab_restricted, apply(diffmat, 2, sum))
    }
    rownames(tab_classifiable)[3] <- "unclassifiable"
    names(dimnames(tab_classifiable)) <- names(dimnames(tab_restricted))
    res[["threshold_and_restriction"]] <- data.frame(
        "restriction" = restriction,
        "classification_threshold_restricted" = classification_threshold_kept,
        "classification_direction_restricted_control_X_case" = classification_direction_kept_control_X_case,
        "classification_threshold_full" = classification_threshold_full,
        "classification_direction_full_control_X_case" = classification_direction_full_control_X_case
    )
    res[["restriction_part"]] <- restriction_part
    res[["table_classifiable"]] <- tab_classifiable
    return(res)
}
