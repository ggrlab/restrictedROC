test_that("prune rROC", {
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2,
        positive_label = "Good"
    )

    usual_prepared <- prepare_modeldata(aSAH[, c("ndka", "s100b")], rroc_result = res_df)

    pruned_rroc <- prune(res_df)
    pruned_prepared <- prepare_modeldata(aSAH[, c("ndka", "s100b")], rroc_result = pruned_rroc)
    # The names are different, but the values are the same
    testthat::expect_equal(usual_prepared, pruned_prepared)
})
