test_that("plot_rROC_part", {
    library(restrictedROC)
    data("aSAH", package = "pROC")

    ret_procs <- simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka,
        return_proc = TRUE
    )
    pdf("removeme.pdf")
    # In this here are three warnings:
    # Warning messages:
    # 1: In get_all_aucs_fun(full_roc = full_roc, true_pred_df = true_pred_df,  :
    #   get_all_aucs_norecalculation() does not calculate single ROC curves, therefore cannot return them
    # 2: In get_all_aucs_fun(full_roc = full_roc, true_pred_df = true_pred_df,  :
    #   get_all_aucs_norecalculation() does not calculate single ROC curves, therefore cannot return them
    # 3: In plot_rROC_part(ret_procs) :
    # Threshold
    #   10
    # not found, using the closest instead:
    #   9.9
    plot_rROC_part(ret_procs, threshold = 10)[["patchworked"]]
    dev.off()

    ret_procs_rroc <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        return_proc = TRUE,
        n_permutations = 0
    )
    pdf("removeme.pdf")
    plot_rROC_part(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], threshold = 10)[["patchworked"]]
    dev.off()
})
test_that("plot_rROC_part different inputs", {
    library(restrictedROC)
    # devtools::load_all()
    data("aSAH", package = "pROC")
    ret_procs <- simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka,
        return_proc = TRUE
    )
    pdf("removeme.pdf")
    plot_rROC_part(ret_procs, threshold = 10)[["patchworked"]]
    plot_rROC_part(simple_rROC_interpret(ret_procs), threshold = 10)[["patchworked"]]
    dev.off()

    ret_procs_rroc <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        return_proc = TRUE,
        n_permutations = 0
    )
    pdf("removeme.pdf")
    plot_rROC_part(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], threshold = 10)[["patchworked"]]
    plot_rROC_part(ret_procs_rroc, threshold = 10)
    dev.off()
})
