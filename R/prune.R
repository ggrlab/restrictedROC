#' Restriction for multiple dependent and independent variables
#'
#' @param x
#' \describe{
#'  \item{data.frame}{
#'      See \code{\link{rROC.data.frame}}. data.frame containing all dependent and
#'      independent variables as columns. Dependent/independent variable column names
#'      must be given as "dependent_vars"/"independent_vars" arguments.
#' }
#'  \item{matrix}{
#'      See \code{\link{rROC.matrix}}. Matrix of (samples x features).
#'      Dependent variable(s) must be given as "y" argument.
#' }
#'  \item{numeric vector}{
#'      See \code{\link{rROC.numeric}}. Numeric vector of independent variable.
#'      Dependent variable(s) must be given as "y" argument.
#' }
#' }
#' @param ... TODO
#' @inheritDotParams rROC.data.frame -dependent_vars -independent_vars
#' @inheritDotParams simple_rROC_permutation -response -predictor -direction
#' @inherit rROC.data.frame return
#' @export
prune <- function(x, ...) {
    # R possesses a simple generic function mechanism which can be used for an
    # object-oriented style of programming. Method dispatch takes place based on
    # the class(es) of the first argument to the generic function or of the object
    # supplied as an argument to UseMethod or NextMethod.
    UseMethod("prune", x)
}



#' Prune a rROC result
#'
#' @param x rROC object returned by \code{\link{rROC}}.
#' @export
prune.rROC <- function(x,
                       ...) {
    prune_rROC(x, ...)
}

prune_rROC <- function(x, ...) {
    lapply(x, function(results_dv) {
        lapply(results_dv, function(results_dv_iv) {
            results_dv_iv$permutation$performances <- "pruned"
            results_dv_iv$permutation$perm_max_bound <- "pruned"
            results_dv_iv$permutation$perm_global_bound <- "pruned"
            results_dv_iv$plot <- NA
            results_dv_iv
        })
    })
}
