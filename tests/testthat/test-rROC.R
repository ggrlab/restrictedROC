test_that("rROC data.frame", {
    options(warn = 1)
    devtools::load_all()
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    rROC(
        aSAH,
        dependent_vars = c("outcome", "outcome"),
        independent_vars = "ndka",
        n_permutations = 5
    )
})
