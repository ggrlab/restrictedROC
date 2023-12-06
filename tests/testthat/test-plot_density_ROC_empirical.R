test_that("plot_rROC different inputs", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    pdf("removeme.pdf")
    print(
        plot_density_rROC_empirical(
            values_grouped = split(aSAH$ndka, aSAH$outcome),
        )
    )
    print(
        plot_density_rROC_empirical(
            values_grouped = split(aSAH$ndka, aSAH$outcome),
            plot_n_points = 100
        )
    )
    dev.off()

    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})
