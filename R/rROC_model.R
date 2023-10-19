#' @export
rROC_model <- function(x, y, ...) {
    UseMethod("rROC_model", x)
}

#' @export
rROC_model.matrix <- function(x, y,
                              rroc_result = NULL,
                              rroc_applied = NULL,
                              rroc_applied_savefile = NULL,
                              rroc_savefile = NULL,
                              ...) {
    rROC_model.data.frame(x, y, rroc_result, rroc_applied, rroc_applied_savefile, rroc_savefile, ...)
}

#' @export
rROC_model.data.frame <- function(x, y,
                                  rroc_result = NULL,
                                  rroc_applied = NULL,
                                  rroc_applied_savefile = NULL,
                                  rroc_savefile = NULL,
                                  ...) {
    if (all(is.null(rroc_result))) {
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

    if (all(is.null(rroc_applied_savefile))) {
        rroc_applied <- apply_restriction(
            object = rroc_result,
            newdata = x,
            ...
        )
        if (!is.null(rroc_applied_savefile)) {
            qs::qsave(rroc_applied, file = rroc_applied_savefile)
        }
    }
    # browser()
    # return(rROC_model(rroc_result, y = y, ...))
}

# rROC_model.matrix <- function(x, y, rroc_result = NULL, rroc_applied = NULL) {
#     print("test")
# }
#' @export
rROC_model.rROC <- function(x, y, rroc_applied_savefile...) {
    apply_restriction()
    print("test")
}
