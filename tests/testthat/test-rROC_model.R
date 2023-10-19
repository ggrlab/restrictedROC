test_that("rROC duplicate dependent/independent errors", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded"),
        positive_label = "Good"
    )
    model <- train_rROC_h2o(
        x_prepared = prepared_df,
        y = aSAH[["outcome"]]
    )
    predict_rROC_h2o(
        h2o_model = model,
        x_prepared = prepared_df,
        y = aSAH[["outcome"]],
    )
})

test_that("rROC duplicate dependent/independent errors", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded"),
        positive_label = "Good"
    )
    model <- train_rROC_h2o(
        x_prepared = prepared_df,
        y = aSAH[["outcome"]],
    )
    prepared_df_2 <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b", "age"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded")
    )
    pred_original <- predict_rROC_h2o(
        h2o_model = model,
        x_prepared = prepared_df,
        y = aSAH[["outcome"]],
    )
    pred_too_much_info <- predict_rROC_h2o(
        h2o_model = model,
        x_prepared = prepared_df_2,
        y = aSAH[["outcome"]],
    )

    # The metrics must be the same
    testthat::expect_equal(pred_original[["metrics"]], pred_too_much_info[["metrics"]])
    # The predictions however INCLUDE x_prepared, therefore they are different
    # because in pred_too_much_info, x_prepared has one more column
    testthat::expect_false(all(dim(pred_original[["predictions"]]) == dim(pred_too_much_info[["predictions"]])))
})
