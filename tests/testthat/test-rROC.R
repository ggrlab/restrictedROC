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

test_that("rROC methods, multiple dependent, numeric", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_numeric <- rROC(
        x = aSAH[["ndka"]],
        y = aSAH[c("outcome", "outcome")], # this is allowed as data.frame() within rROC.numeric will make unique names
        n_permutations = 2
    )
    set.seed(100)
    res_numeric_v2 <- rROC(
        x = aSAH[["ndka"]],
        y = list("outcome_1" = aSAH[["outcome"]]),
        n_permutations = 2
    )
    set.seed(100)
    res_numeric_v3 <- rROC(
        x = aSAH[["ndka"]],
        y = list("outcome_1" = aSAH[["outcome"]], "outcome_2" = aSAH[["outcome"]]),
        n_permutations = 2
    )

    # The names are different, but the values are the same
    testthat::expect_equal(res_numeric[[1]][[1]], res_numeric[[2]][[1]])
    testthat::expect_equal(res_numeric[[1]][[1]], res_numeric_v2[[1]][[1]])
    testthat::expect_equal(res_numeric[[1]][[1]], res_numeric_v3[[1]][[1]])
    testthat::expect_equal(res_numeric[[1]][[1]], res_numeric_v3[[2]][[1]])
})

test_that("rROC methods, multiple dependent, matrix", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_mat <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = aSAH[c("outcome", "outcome")],
        n_permutations = 2
    )
    set.seed(100)
    res_mat_v2 <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = list("outcome_1" = aSAH[["outcome"]]),
        n_permutations = 2
    )
    set.seed(100)
    res_mat_v3 <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = list("outcome_1" = aSAH[["outcome"]], "outcome_2" = aSAH[["outcome"]]),
        n_permutations = 2
    )

    # The names are different, but the values are the same
    testthat::expect_equal(res_mat[[1]][[1]], res_mat[[2]][[1]])
    testthat::expect_equal(res_mat[[1]][[1]], res_mat_v2[[1]][[1]])
    testthat::expect_equal(res_mat[[1]][[1]], res_mat_v3[[1]][[1]])
    testthat::expect_equal(res_mat[[1]][[1]], res_mat_v3[[2]][[1]])
})

test_that("rROC methods, fix_seed", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = FALSE
    )
    res_df_2 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = FALSE
    )
    testthat::expect_true(all.equal(res_df, res_df))
    testthat::expect_false(isTRUE(all.equal(res_df, res_df_2)))

    set.seed(100)
    res_df_3 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = FALSE
    )
    testthat::expect_equal(res_df, res_df_3)


    res_df_4 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = 0
    )
    testthat::expect_false(isTRUE(all.equal(res_df, res_df_4)))


    res_df_5 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = 0
    )
    testthat::expect_equal(res_df_4, res_df_5)

    set.seed(129387)
    res_df_6 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        fix_seed = 0
    )
    testthat::expect_equal(res_df_4, res_df_6)
})
