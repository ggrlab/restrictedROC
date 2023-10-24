test_that("plot_rROC different inputs", {
    library(restrictedROC)
    # devtools::load_all()
    data("aSAH", package = "pROC")
    ret_procs <- simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka,
        return_proc = TRUE
    )
    pdf("removeme.pdf")
    plot_rROC(ret_procs, part = "high")
    plot_rROC(simple_rROC_interpret(ret_procs), part = "high")
    dev.off()

    ret_procs_rroc <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        return_proc = TRUE,
        n_permutations = 0
    )
    pdf("removeme.pdf")
    plot_rROC(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], part = "high")
    plot_rROC(ret_procs_rroc, part = "high")
    dev.off()
})



# test_that("plot_rROC", {
#     library(restrictedROC)
#     data("aSAH", package = "pROC")

#     ret_procs <- simple_rROC(
#         response = aSAH$outcome,
#         predictor = aSAH$ndka,
#         return_proc = TRUE
#     )
#     pdf("removeme.pdf")
#     # In this here are three warnings:
#     # Warning messages:
#     # 1: In get_all_aucs_fun(full_roc = full_roc, true_pred_df = true_pred_df,  :
#     #   get_all_aucs_norecalculation() does not calculate single ROC curves, therefore cannot return them
#     # 2: In get_all_aucs_fun(full_roc = full_roc, true_pred_df = true_pred_df,  :
#     #   get_all_aucs_norecalculation() does not calculate single ROC curves, therefore cannot return them
#     # 3: In plot_rROC_part(ret_procs) :
#     # Threshold
#     #   10
#     # not found, using the closest instead:
#     #   9.9
#     plot_rROC_part(ret_procs, threshold = 10)
#     dev.off()

#     ret_procs_rroc <- rROC(
#         aSAH,
#         dependent_vars = "outcome",
#         independent_vars = "ndka",
#         return_proc = TRUE,
#         n_permutations = 0
#     )
#     pdf("removeme.pdf")
#     plot_rROC_part(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], threshold = 10)
#     dev.off()
# })
