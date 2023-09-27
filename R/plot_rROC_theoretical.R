#' Plot (restricted) ROC curves for theoretic distributions
#'
#'
#' @param qnorm_positive
#' Quantile distribution function of positive class
#' @param qnorm_negative
#' Quantile distribution function of negative class
#' @param n_positive
#'  Number of positive samples.
#' @param n_negative
#'  Number of negative samples.
#' @param length.out_densityplot
#' Number of points to draw the density plot per curve
#' @param return_all
#' If true, also return the rROC result, not only the plots
#' @param ...
#' Further arguments to [plot_density_rROC_empirical()]
#'
#' @return
#' If return_all: List of "data" and "rroc" result
#' Else: Only the "plots" from "rroc" result
#' @export
#'
#' @examples
#'
#' plot_rROC_theoretical(
#'     qnorm_positive = function(x) qnorm(x, mean = 1, sd = 1),
#'     qnorm_negative = function(x) qnorm(x, mean = 0, sd = 1)
#' )
plot_rROC_theoretical <- function(qnorm_positive, qnorm_negative,
                                  length.out_densityplot = 500,
                                  n_positive = 500, n_negative = 500,
                                  return_all = FALSE, ...) {
    tmp <- list(
        "positive" = qnorm_positive(
            seq(from = 0, to = 1, length.out = n_positive + 2)[2:(n_positive + 1)]
        ),
        "negative" = qnorm_negative(
            seq(from = 0, to = 1, length.out = n_negative + 2)[2:(n_negative + 1)]
        )
    )
    rroc_res <- plot_density_rROC_empirical(
        tmp,
        length.out = length.out_densityplot,
        positive_label = "positive",
        direction = "<",
        ...
    )
    rroc_res[["plots"]] <- rroc_res[["plots"]] + patchwork::plot_annotation(
        title = paste0("n_positive: ", n_positive, "\nn_negative: ", n_negative)
    )
    if (return_all) {
        return(list("data" = tmp, "rroc" = rroc_res))
    } else {
        return(rroc_res[["plots"]])
    }
}
