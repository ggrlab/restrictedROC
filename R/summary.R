#' @title Give a summary of a rROC() result
#' @description \code{\link{rROC}} returns a nested list. It looks something like:
#' \code{single_restriction <- reslist[[dependent_variables]][[independent_variables]]}
#'  where every single restriction has done permutations, a p-value and additional info.
#'  This function gives a summary of the result.
#' @param object A \code{\link{rROC}} result
#' @param relevant_cols_regex A regex to filter the columns of a single restriction.
#' @param current_level The current level of the nested list. This is used internally.
#' @param searchword
#' If the searchword is not found in the names of the current element, the current level
#' is increased and the function is called recursively. If the searchword is found, the
#' summary is returned.
#' @param ... Additional arguments passed to \code{\link{summary}}.
#' @export
#' @return A summary of the result:
summary.rROC <- function(object,
                         relevant_cols_regex = c(
                             "pval.twoside.*", "n_permutations",
                             "positive_label", ".*\\.auc$", "part",
                             "restriction",
                             "informative_range.*"
                         ),
                         searchword = "max_total",
                         current_level = 0,
                         ...) {
    if (all(is.null(object)) || all(is.na(object))) {
        return(NULL)
    } else if (!searchword %in% names(object)) {
        nontraverse_vars <- which(names(object) %in% c("plots"))
        if (length(nontraverse_vars) > 0) {
            object <- object[-nontraverse_vars]
        }
        current_level <- current_level + 1
        l_results <- lapply(
            object,
            summary.rROC,
            current_level = current_level,
            relevant_cols_regex = relevant_cols_regex,
            searchword = searchword
        )
        if (current_level == 0) {
            return(dplyr::bind_rows(lapply(l_results, tibble::as_tibble)))
        } else {
            return(dplyr::bind_rows(l_results, .id = paste0("level_", current_level)))
        }
    } else {
        single_summary <- (data.frame(c(
            object[["permutation_pval"]],
            "positive_label" = as.character(object[["positive_label"]]),
            object["max_total"],
            object["global"]
        )))
        restriction <- object[["max_total"]][["threshold"]]
        restriction_part <- object[["max_total"]][["part"]]
        if (restriction_part == "low") {
            informative_range <- c(-Inf, restriction)
        } else if (restriction_part == "high") {
            informative_range <- c(restriction, Inf)
        } else {
            informative_range <- c(-Inf, Inf)
        }
        single_summary[["restriction"]] <- restriction
        single_summary[["informative_range.min"]] <- min(informative_range)
        single_summary[["informative_range.max"]] <- max(informative_range)

        keeping_cols <- c()
        if (!all(is.na(relevant_cols_regex))) {
            for (regex_x in c(relevant_cols_regex, "^level_")) {
                keeping_cols <- c(keeping_cols, grep(regex_x, colnames(single_summary)))
            }
            return(single_summary[, keeping_cols])
        } else {
            return(single_summary)
        }
    }
}
