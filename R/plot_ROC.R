#' @import ggplot2
#'
plot_ROC <- function(tpr, fpr) {
    if (all(class(tpr) == "roc")) {
        tpr <- pROC::coords(tpr, ret = c("fpr", "tpr"))
    }
    if (any(class(tpr) == "restrictedROC")) {
        tpr <- tpr$perf
        tpr$fpr <- tpr[["fpr_global"]]
        tpr$tpr <- tpr[["tpr_global"]]
    }
    if ("fpr" %in% names(tpr)) {
        # then tpr was a dataframe containing tpr _and_ fpr
        df <- tpr
    } else {
        df <- data.frame("tpr" = tpr, "fpr" = fpr)
    }
    return(
        ggplot(df, aes(x = fpr, y = tpr)) +
            geom_path() +
            geom_abline(slope = 1, intercept = 0, linetype = 2) +
            xlab("False positive rate") +
            ylab("True positive rate") +
            ggpubr::theme_pubr() +
            theme(aspect.ratio = 1) # enforce square plot
    )
}
