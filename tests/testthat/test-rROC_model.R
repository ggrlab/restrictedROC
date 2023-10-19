devtools::load_all()
test_that("rROC duplicate dependent/independent errors", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    prepared_df <- prepare_modeldata(
        x = aSAH[, c("ndka", "s100b"), drop = FALSE],
        y = aSAH["outcome"],
        which_preds = c("bounded")
    )
    model <- train_rROC_h2o(
        x_prepared = prepared_df,
        y = aSAH[["outcome"]],
    )
    predict_rROC_h2o(
        h2o_model = model,
        x_prepared = prepared_df,
        y = aSAH[["outcome"]],
    )
})
