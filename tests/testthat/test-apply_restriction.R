test_that("rROC apply restriction", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
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
test_that("rROC apply restriction missing feature", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    testthat::expect_error(
        apply_restriction(res_df, aSAH[, "ndka"]),
        # aSAH[, "ndka"] returns a (unnamed) vector, therefore the feature "ndka" does "not exist"
        "feature 'ndka' not found in newdata"
    )
    applied_1 <- apply_restriction(res_df, aSAH[, "ndka", drop = FALSE])
    testthat::expect_error(
        apply_restriction(res_df, aSAH[, "s100b", drop = FALSE]),
        "feature 'ndka' not found in newdata"
    )
    applied_2 <- apply_restriction(res_df, aSAH[, c("ndka", "s100b"), drop = FALSE])
    testthat::expect_equal(applied_1, applied_2)
})
