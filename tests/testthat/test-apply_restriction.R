test_that("rROC methods", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2
    )
    applied_1 <- apply_restriction(res_df, aSAH)
    applied_2 <- apply_restriction(res_df[[1]], aSAH)
    applied_3 <- apply_restriction(res_df[[1]][1], aSAH)
    applied_4 <- apply_restriction(res_df[[1]][[1]], aSAH, feature = "ndka")


    testthat::expect_equal(applied_1[["outcome"]][["ndka"]], applied_2[["ndka"]])
    testthat::expect_equal(applied_1[["outcome"]][["ndka"]], applied_3[["ndka"]])
    testthat::expect_equal(applied_1[["outcome"]][["ndka"]], applied_4)

    testthat::expect_error(
        apply_restriction(res_df[[1]][[1]], aSAH),
        "feature must be specified"
    )
})
