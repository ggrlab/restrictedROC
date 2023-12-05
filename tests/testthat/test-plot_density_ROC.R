test_that("plot_density_ROC", {
    library(restrictedROC)
    plot_density_ROC(
        density_positive = function(x) dnorm(x, mean = 0, sd = 1),
        cdf_positive = function(x) pnorm(x, mean = 0, sd = 1),
        density_negative = function(x) dnorm(x, mean = -1, sd = 1.5),
        quantile_negative = function(x) qnorm(x, mean = -1, sd = 1.5),
        xmin = -4, xmax = 7
    )
    testthat::expect_true(TRUE)
})
