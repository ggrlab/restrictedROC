library(testthat)
test_that("Paired rROC", {
    # library(restrictedROC)
    devtools::load_all()
    data("aSAH", package = "pROC")
    aSAH <- aSAH[aSAH[["ndka"]] < 100, ]
    set.seed(100)
    res_rroc <- simple_rROC_paired(
        x = aSAH[["ndka"]],
        y = aSAH[["ndka"]] - rnorm(nrow(aSAH), mean = 1, sd = 0.1),
        positive_label = "Good"
    )
    set.seed(194)
    res_rroc <- simple_rROC_paired(
        x = aSAH[["ndka"]]**rnorm(nrow(aSAH), mean = 1, sd = 0.1),
        y = aSAH[["ndka"]]**rnorm(nrow(aSAH), mean = 1.2, sd = 0.1),
        positive_label = "Good"
    )
    set.seed(194)
    res_rroc <- simple_rROC_paired(
        x = aSAH[["ndka"]],
        y = aSAH[["ndka"]] * rnorm(nrow(aSAH), mean = 1.1, sd = 0.1),
        positive_label = "Good"
    )
    set.seed(194)
    res_rroc <- simple_rROC_paired(
        x = aSAH[["ndka"]] / 50,
        y = (aSAH[["ndka"]] / 50)**rnorm(nrow(aSAH), mean = 1, sd = 0.25),
        positive_label = "Good"
    )
})
