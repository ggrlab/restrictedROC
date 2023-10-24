test_that("rROC summary", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    testthat::expect_equal(dim(summary(res_df)), c(1, 10))


    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2,
        positive_label = "Good"
    )
    testthat::expect_equal(dim(summary(res_df)), c(2, 10))


    # Two warnings will emerge here, one for every outcome
    # Changing to the first sorted unique value of 'outcome': 'Good'
    suppressWarnings(
        res_df <- rROC(
            aSAH,
            dependent_vars = c("outcome", "gender"),
            independent_vars = c("ndka", "s100b"),
            n_permutations = 2
        )
    )
    testthat::expect_equal(dim(summary(res_df)), c(4, 10))
})


test_that("rROC summary results", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    tmp_s <- summary(res_df)

    testthat::expect_true(
        all(c(
            "level_1", "level_2", "level_3",
            paste0("pval.twoside.", c("max", "global")),
            "n_permutations", "positive_label",
            paste0(c("max_total", "global"), ".auc"),
            "max_total.part",
            "restriction",
            "informative_range.min",
            "informative_range.max"
        ) %in% colnames(tmp_s))
    )
})
