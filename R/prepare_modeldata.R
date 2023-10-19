#' @title Prepare model data
#' @description This function prepares the data for the model. It takes the rROC result and
#' applies the restrictions to the data. It then merges the predictions into a
#' single data.frame which can be used as input for model building.
#' @inheritParams prepare_modeldata.data.frame
#' @inheritDotParams prepare_modeldata.data.frame
#' @export
prepare_modeldata <- function(x, ...) {
    UseMethod("prepare_modeldata", x)
}

#' @inheritDotParams prepare_modeldata.data.frame
#' @export
prepare_modeldata.matrix <- function(x,
                                     ...) {
    prepare_modeldata.data.frame(x, ...)
}
#' @title Prepare model data
#' @description This function prepares the data for the model. It takes the rROC result and
#' applies the restrictions to the data. It then merges the predictions into a
#' single data.frame which can be used as input for model building.
#' @param x Data which should be prepared. Can be a data.frame or matrix, but
#' must have column names. Rows are samples, columns are features.
#' @param y The outcome vector. If NULL, rroc_result must be supplied. If given, only
#' rROC() uses it.
#' @param rroc_result The result of rROC(). If NULL, rROC() is called with x, y and ``...``.
#' @param rroc_savefile If not NULL, the rroc_result is saved to this file.
#' @param which_preds Which predictions should be returned? Default is "bounded". See
#' \code{\link{merge_applied}} for details.
#' @param ... Arguments passed to rROC() if rroc_result is NULL.
#' @export
prepare_modeldata.data.frame <- function(x,
                                         y = NULL,
                                         rroc_result = NULL,
                                         rroc_savefile = NULL,
                                         which_preds = "bounded",
                                         ...) {
    if (all(is.null(rroc_result))) {
        if (all(is.null(y))) {
            stop("Either rroc_result or y must be supplied to recalculate rroc_result")
        }
        rroc_result <- rROC(
            x,
            independent_vars = colnames(x),
            y = y,
            n_permutations = 0, ...
        )
        if (!is.null(rroc_savefile)) {
            qs::qsave(rroc_result, file = rroc_savefile)
        }
    }
    if (!"permutation" %in% names(rroc_result[[1]])) {
        if (length(rroc_result) != 1) {
            stop("E1: rroc_result must be a single outcome, multiple feature rROC object")
        } else {
            # Go down one single list level which was the list of outcomes
            # The last if checked that there was only one outcome
            rroc_result <- rroc_result[[1]]
            if (!"permutation" %in% names(rroc_result[[1]])) {
                stop("E2: rroc_result must be a single outcome, multiple feature rROC object")
            }
        }
    }

    rroc_applied <- apply_restriction(
        object = rroc_result,
        newdata = x,
        ...
    )

    if (!all(is.null(which_preds))) {
        x_restricted_merged <- merge_applied(rroc_applied, which_preds = which_preds)
    } else {
        x_restricted_merged <- merge_applied(rroc_applied)
    }

    return(x_restricted_merged)
}

# # rROC_model.matrix <- function(x, y, rroc_result = NULL, rroc_applied = NULL) {
# #     print("test")
# # }
# #' @export
# rROC_model.rROC <- function(x, y, rroc_applied_savefile...) {
#     apply_restriction()
#     print("test")
# }
