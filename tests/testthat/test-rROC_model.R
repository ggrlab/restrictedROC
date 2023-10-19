test_that("rROC model", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC_model(
        x = aSAH[, "ndka", drop = FALSE],
        y = aSAH["outcome"]
    )
})
