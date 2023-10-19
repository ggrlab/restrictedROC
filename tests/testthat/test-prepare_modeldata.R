test_that("prepare_modeldata: From data", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    prepared_df <- prepare_modeldata(
        x = aSAH[, "ndka", drop = FALSE],
        y = aSAH["outcome"]
    )
    testthat::expect_true(all(dim(prepared_df) == c(113, 1)))

    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"]
    )
    testthat::expect_true(all(dim(prepared_df) == c(113, 2)))

    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded")
    )
    testthat::expect_true(all(dim(prepared_df) == c(113, 2)))

    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded", "predictor")
    )
    testthat::expect_true(all(dim(prepared_df) == c(113, 4)))
})

test_that("prepare_modeldata: From rROC", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    rroc_res <- rROC(
        x = aSAH[, "ndka", drop = FALSE],
        independent_vars = "ndka",
        y = aSAH["outcome"],
        n_permutations = 0
    )
    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        rroc_result = rroc_res
    )
    # rROC was calculated only on ndka as feature, so only one column
    # should be returned even when giving more features to prepare_modeldata
    # Especially: rROC is NOT recalculated!
    testthat::expect_true(all(dim(prepared_df) == c(113, 1)))

    prepared_df_2 <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"]
    )
    testthat::expect_true(all(dim(prepared_df_2) == c(113, 2)))
})
