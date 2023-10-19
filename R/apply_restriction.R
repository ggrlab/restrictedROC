#' @title Apply a [nested list] of rROC results to new data
#' @description
#' Restriction returns an informative range which tells that some samples should be
#' discarded according to their predictor value. This function applies the restriction
#' to a new dataset. Predictor values outside the informative range are imputed with
#' \code{removed_impute}.
#' @param object A [nested list] of rROC results. See \code{\link{rROC}} for details.
#' @param newdata A data frame with all predictor variables.
#' @param feature The current feature. If \code{object} is a single restrictedROC object,
#' this is the name of the feature. If \code{object} is a [nested list], this is the name
#' of the current list element. Example: \code{list("outcome_1"=list("feature_1"=rroc_result))}
#' \code{feature} is \code{"feature_1"} when \code{rroc_result} is processed when
#' rroc_result is detected.
#' @param removed_impute The value to impute for samples outside the informative range.
#' @return
#' A list with three elements:
#' \itemize{
#'      \item \code{predictions}: A data frame with the original predictor values and the
#'      imputed values. It has three columns:
#'      \itemize{
#'          \item{\code{predictor}} {is the original predictor value.}
#'          \item{\code{keep}} {is a logical vector that indicates whether the sample is inside the
#'          informative range.}
#'          \item{\code{bounded}} {is the imputed value.}
#'      }
#'      \item \code{thresholds}: A data frame with the feature, threshold, the part of the
#'      predictor that was used and the imputed value for predictor values outside the
#'      informative range. The informative range is then:
#'          \itemize{
#'              \item \code{low}: \code{(-Inf, threshold]}
#'              \item \code{high}: \code{(threshold, Inf)}
#'          }
#' }
#'
apply_restriction <- function(object, newdata, feature = NA, removed_impute = -1) {
    if ("single_rROC" %in% names(object)) {
        object <- object[["single_rROC"]]
    } else if ("permutation" %in% names(object)) {
        object <- object[["permutation"]]
    } else {
        return(
            sapply(
                names(object),
                function(x) apply_restriction(object[[x]], newdata, x),
                USE.NAMES = TRUE,
                simplify = FALSE
            )
        )
    }
    single_rroc_result <- object
    if (!"restrictedROC" %in% class(single_rroc_result)) {
        stop("object is not a restrictedROC object")
    }
    if (is.na(feature)) {
        stop(paste0(
            "feature must be specified. It usually is the name of the current ",
            "list element. If a single restrictedROC object is supplied, give the feature name."
        ))
    }
    if (!feature %in% names(newdata)) {
        stop(paste0(
            "feature '", feature, "' not found in newdata. ",
            "It must be a column of newdata (",
            paste0(names(newdata), collapse = ", "),
            ")"
        ))
    }
    pred_df <- data.frame("predictor" = newdata[[feature]])
    restriction <- single_rroc_result[["max_total"]][["threshold"]]
    restriction_part <- single_rroc_result[["max_total"]][["part"]]

    if (restriction_part == "low") {
        pred_df$keep <- pred_df$predictor <= restriction
        informative_range <- c(-Inf, restriction)
    } else if (restriction_part == "high") {
        pred_df$keep <- pred_df$predictor > restriction
        informative_range <- c(restriction, Inf)
    } else {
        pred_df$keep <- TRUE
        informative_range <- c(-Inf, Inf)
    }
    pred_df[["bounded"]] <- pred_df[["predictor"]]
    pred_df[["bounded"]][!pred_df[["keep"]]] <- removed_impute
    reslist <- list(
        "predictions" = tibble::as_tibble(pred_df),
        "thresholds" = tibble::tibble(
            "feature" = feature,
            "threshold" = single_rroc_result[["max_total"]][["threshold"]],
            "part" = single_rroc_result[["max_total"]][["part"]],
            "imputed" = removed_impute,
            "informative_range.min" = min(informative_range),
            "informative_range.max" = max(informative_range)
        )
    )
    class(reslist) <- c("applied_rROC", class(reslist))
    return(reslist)
}
