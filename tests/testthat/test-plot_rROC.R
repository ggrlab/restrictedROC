test_that("plot_rROC different inputs", {
    library(restrictedROC)
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


    testthat::expect_true(TRUE)  # Just that the test is not skipped as "empty"
})
