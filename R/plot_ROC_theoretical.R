#' ROC curve for positive cumulative distribution and negative quantile function
#'
#' @param pnorm_positive
#' Cumulative distribution function of positive class
#' @param qnorm_negative
#' Quantile function (inverse of the cumulative distribution function) of the negative class
#' @param length.out
#' How many false positive rates should the ROC-curve be calculated for?
#'
#' @return
#' List of
#' 	- ggplot of the ROC curve
#' 	- AUC of the curve
#' @export
#'
#' @examples
#'
#' plot_ROC_theoretical(
#'     pnorm_positive = function(x) pnorm(x, mean = 1, sd = 1),
#'     qnorm_negative = function(x) qnorm(x, mean = 1, sd = .5)
#' )
plot_ROC_theoretical <- function(pnorm_positive, qnorm_negative, length.out = 500) {
    fpr <- seq(from = 0, to = 1, length.out = length.out)
    # Gneiting, Vogel (Machine Learning 2022)
    # 	Receiver operating characteristic (ROC) curves:
    # 	equivalences, beta model, and minimum distance estimation
    # :Math:  R(p) = 1-F_1(F_0^-1(1-p))
    df <- data.frame(
        "tpr" = 1 - pnorm_positive(qnorm_negative(1 - fpr)),
        "fpr" = fpr
    )
    auc <- stats::integrate(function(fpr) {
        1 - pnorm_positive(qnorm_negative(1 - fpr))
    }, lower = 0, upper = 1, subdivisions = length.out)
    return(list("plot" = plot_ROC(df), "auc" = auc))
}
