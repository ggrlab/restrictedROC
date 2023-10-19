test_that("rROC summary", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    testthat::expect_equal(dim(summary(res_df)), c(1, 10))


    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2,
        positive_label = "Good"
    )
    testthat::expect_equal(dim(summary(res_df)), c(2, 10))


    # Two warnings will emerge here, one for every outcome
    # Changing to the first sorted unique value of 'outcome': 'Good'
    suppressWarnings(
        res_df <- rROC(
            aSAH,
            dependent_vars = c("outcome", "gender"),
            independent_vars = c("ndka", "s100b"),
            n_permutations = 2
        )
    )
    testthat::expect_equal(dim(summary(res_df)), c(4, 10))
})
