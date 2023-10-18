test_that("rROC data.frame", {
    devtools::load_all()
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka"
    )
})
