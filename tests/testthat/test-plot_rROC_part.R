test_that("plot_rROC_part", {
    library(restrictedROC)
    data("aSAH", package = "pROC")

    ret_procs <- simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka,
        return_proc = TRUE,
        positive_label = "Good"
    )
    pdf("removeme.pdf")
    # In plot_rROC_part(ret_procs) :
    # Threshold
    #   10
    # not found, using the closest instead:
    #   9.9
    testthat::expect_warning(
        print(
            plot_rROC_part(ret_procs, threshold = 10)[["patchworked"]]
        ),
        regexp = "not found, using the closest instead:"
    )
    dev.off()

    ret_procs_rroc <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        return_proc = TRUE,
        n_permutations = 0,
        positive_label = "Good"
    )
    pdf("removeme.pdf")
    testthat::expect_warning(
        print(
            plot_rROC_part(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], threshold = 10)[["patchworked"]]
        ),
        regexp = "not found, using the closest instead:"
    )
    dev.off()

    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})
test_that("plot_rROC_part different inputs", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    ret_procs <- simple_rROC(
        response = aSAH$outcome,
        predictor = aSAH$ndka,
        return_proc = TRUE
    )
    pdf("removeme.pdf")
    testthat::expect_warning(
        print(plot_rROC_part(ret_procs, threshold = 10)[["patchworked"]]),
        regexp = "not found, using the closest instead:"
    )
    testthat::expect_warning(
        print(plot_rROC_part(simple_rROC_interpret(ret_procs), threshold = 10)[["patchworked"]]),
        regexp = "not found, using the closest instead:"
    )
    dev.off()

    ret_procs_rroc <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        return_proc = TRUE,
        n_permutations = 0,
        positive_label = "Good"
    )
    pdf("removeme.pdf")
    testthat::expect_warning(
        print(plot_rROC_part(ret_procs_rroc[["outcome"]][["ndka"]][["permutation"]], threshold = 10)[["patchworked"]]),
        regexp = "not found, using the closest instead:"
    )
    testthat::expect_warning(
        print(plot_rROC_part(ret_procs_rroc, threshold = 10)),
        regexp = "not found, using the closest instead:"
    )
    testthat::expect_warning(
        print(plot_rROC_part(x = ret_procs_rroc, threshold = 10)),
        regexp = "not found, using the closest instead:"
    )
    dev.off()

    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})
