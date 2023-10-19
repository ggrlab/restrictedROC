#' @title Merge (cbind) applied rROC results
#' @description Merge (cbind) applied rROC results
#' @param rroc_applied A _single outcome_ result of apply_restriction()
#' @param which_preds Which predictions should be returned? Default is all.
#' You can also specify "all_split" to return a list of predictions for each
#' type of prediction (e.g. "predictor", "bounded", "keep"). Or you can specify
#' a subset of these types (e.g. c("predictor", "bounded")), then the bound predictions
#' including the predictions of only these types will be returned.
merge_applied <- function(rroc_applied,
                          which_preds = c("all", "all_split", "predictor", "bounded", "keep")) {
    per_feature_rroc_thresholds <- do.call(rbind, lapply(rroc_applied, function(y) y[["thresholds"]]))
    rownames(per_feature_rroc_thresholds) <- NULL # just cleanup

    predictions_allbound <- tibble::as_tibble(
        do.call(cbind, lapply(rroc_applied, function(y) y[["predictions"]]))
    )
    col_type <- sub(".*\\.", "", colnames(predictions_allbound))
    col_types_split <- split(colnames(predictions_allbound), col_type)
    if (all(which_preds == "all")) {
        res <- predictions_allbound
    } else if (all(which_preds == "all_split")) {
        res <- lapply(col_types_split, function(colnames_x) {
            tmp <- predictions_allbound[, colnames_x]
            colnames(tmp) <- sub("^.*___", "", colnames(tmp))
            return(tmp)
        })
    } else {
        col_types_split_selection <- col_types_split[names(col_types_split) %in% which_preds]
        res <- predictions_allbound[, unlist(col_types_split_selection)]
    }
    return(res)
}
