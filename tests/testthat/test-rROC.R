test_that("rROC duplicate dependent/independent errors", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    testthat::expect_error(
        rROC(
            aSAH,
            dependent_vars = c("outcome", "outcome"),
            independent_vars = "ndka",
            n_permutations = 5
        ),
        "dependent_vars must be unique"
    )
    testthat::expect_error(
        rROC(
            aSAH,
            dependent_vars = "outcome",
            independent_vars = c("ndka", "ndka"),
            n_permutations = 5
        ),
        "independent_vars must be unique"
    )
})

test_that("rROC methods", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2
    )
    set.seed(100)
    res_numeric <- rROC(
        x = aSAH[["ndka"]],
        y = aSAH[["outcome"]],
        n_permutations = 2
    )

    set.seed(100)
    res_matrix <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = aSAH[["outcome"]],
        n_permutations = 2
    )

    # The names are different, but the values are the same
    testthat::expect_equal(res_df[[1]][[1]], res_numeric[[1]][[1]])
    testthat::expect_equal(res_df[[1]][[1]], res_matrix[[1]][[1]])
})


test_that("rROC methods, multiple features", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2
    )
    set.seed(100)
    res_matrix <- rROC(
        x = as.matrix(aSAH[, c("ndka", "s100b")]),
        y = aSAH[["outcome"]],
        n_permutations = 2
    )

    # The names are different, but the values are the same
    testthat::expect_equal(res_df[[1]], res_matrix[[1]])
})

test_that("rROC methods, all columns except dependent variables", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2
    )
    set.seed(100)
    res_df_all <- rROC(
        aSAH[, c("outcome", "ndka", "s100b")],
        dependent_vars = "outcome",
        independent_vars = NULL,
        n_permutations = 2
    )
    testthat::expect_equal(res_df, res_df_all)
})
