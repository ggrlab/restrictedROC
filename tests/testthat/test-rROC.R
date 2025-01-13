test_that("rROC duplicate dependent/independent errors", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    testthat::expect_error(
        rROC(
            aSAH,
            dependent_vars = c("outcome", "outcome"),
            independent_vars = "ndka",
            n_permutations = 5,
            positive_label = "Good"
        ),
        "dependent_vars must be unique"
    )
    testthat::expect_error(
        rROC(
            aSAH,
            dependent_vars = "outcome",
            independent_vars = c("ndka", "ndka"),
            n_permutations = 5,
            positive_label = "Good"
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
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_df_2 <- rROC(
        aSAH,
        y = aSAH[["outcome"]],
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_df_3 <- rROC(
        aSAH,
        y = aSAH["outcome"],
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_numeric <- rROC(
        x = aSAH[["ndka"]],
        y = aSAH[["outcome"]],
        n_permutations = 2,
        positive_label = "Good"
    )

    set.seed(100)
    res_matrix <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = aSAH[["outcome"]],
        n_permutations = 2,
        positive_label = "Good"
    )

    # The data are the same, but the names are different.
    testthat::expect_equal(res_df[[1]], res_df_2[[1]])
    # Giving "y" as a vector without name results in a placeholder OUTCOME-name: "y_manually_given"
    testthat::expect_false(isTRUE(all.equal(res_df, res_df_2)))
    testthat::expect_equal(res_df, res_df_3)

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
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_matrix <- rROC(
        x = as.matrix(aSAH[, c("ndka", "s100b")]),
        y = aSAH[["outcome"]],
        n_permutations = 2,
        positive_label = "Good"
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
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_df_all <- rROC(
        aSAH[, c("outcome", "ndka", "s100b")],
        dependent_vars = "outcome",
        independent_vars = NULL,
        n_permutations = 2,
        positive_label = "Good"
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
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_numeric_v2 <- rROC(
        x = aSAH[["ndka"]],
        y = list("outcome_1" = aSAH[["outcome"]]),
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_numeric_v3 <- rROC(
        x = aSAH[["ndka"]],
        y = list("outcome_1" = aSAH[["outcome"]], "outcome_2" = aSAH[["outcome"]]),
        n_permutations = 2,
        positive_label = "Good"
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
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_mat_v2 <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = list("outcome_1" = aSAH[["outcome"]]),
        n_permutations = 2,
        positive_label = "Good"
    )
    set.seed(100)
    res_mat_v3 <- rROC(
        x = as.matrix(aSAH[["ndka"]]),
        y = list("outcome_1" = aSAH[["outcome"]], "outcome_2" = aSAH[["outcome"]]),
        n_permutations = 2,
        positive_label = "Good"
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
        positive_label = "Good",
        fix_seed = FALSE
    )
    res_df_2 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
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
        positive_label = "Good",
        fix_seed = FALSE
    )
    testthat::expect_equal(res_df, res_df_3)


    res_df_4 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        fix_seed = 0
    )
    testthat::expect_false(isTRUE(all.equal(res_df, res_df_4)))


    res_df_5 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        fix_seed = 0
    )
    testthat::expect_equal(res_df_4, res_df_5)

    set.seed(129387)
    res_df_6 <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        fix_seed = 0
    )
    testthat::expect_equal(res_df_4, res_df_6)
})

test_that("rROC without permutation", {
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
    set.seed(100)
    res_df_noperm <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 0,
        positive_label = "Good"
    )
    class(res_df_noperm[[1]][[1]][["permutation"]])
    class(res_df[[1]][[1]][["permutation"]])

    basic_rroc_results <- c(
        "performances",
        "global",
        "keep_highs",
        "keep_lows",
        "max_total",
        "positive_label"
    )
    testthat::expect_false(isTRUE(all.equal(res_df, res_df_noperm)))
    testthat::expect_true(
        all(basic_rroc_results %in% names(res_df_noperm[[1]][[1]][["permutation"]]))
    )


    added_permutation_results <- c(
        "permutation_pval",
        "perm_max_bound",
        "perm_global_bound"
    )
    testthat::expect_true(
        all(
            c(
                basic_rroc_results,
                added_permutation_results
            ) %in% names(res_df[[1]][[1]][["permutation"]])
        )
    )
})

test_that("rROC direction", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        direction = "<"
    )

    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})


test_that("rROC vs simple_rROC", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)

    rroc <- restrictedROC::rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = "ndka",
        positive_label = "Good",
        direction = "<",
        return_proc = TRUE,
        n_permutations = 0
    )[["outcome"]][["ndka"]][["permutation"]]
    rroc_2 <- restrictedROC::simple_rROC(
        response = aSAH[["outcome"]],
        predictor = aSAH[["ndka"]],
        positive_label = "Good",
        direction = "<",
        return_proc = TRUE,
    )
    rroc_2_tmp <- restrictedROC::simple_rROC_interpret(rroc_2)
    testthat::expect_true(all.equal(rroc_2_tmp, rroc), info = "rROC and simple_rROC_interpret(simple_rROC()) do not return the same results")
    testthat::expect_true(all(
        all.equal(rroc[["performances"]], rroc_2[["joined_aucs"]]),
        all.equal(rroc[["positive_label"]], rroc_2[["positive_label"]]),
        all.equal(rroc[["pROC_full"]], rroc_2[["pROC_full"]]),
        is.na(rroc_2[["pROC_lowpart"]]),
        is.na(rroc_2[["pROC_highpart"]])
    ), info = "rROC and simple_rROC are almost the same, just nicer formatted")
})

test_that("rROC too long independent var names", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    ultra_long_feature_name <- paste0(rep("a", 255), collapse = "")
    aSAH[[ultra_long_feature_name]] <- aSAH[["ndka"]]
    rroc <- restrictedROC::rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = ultra_long_feature_name,
        positive_label = "Good",
        direction = "<",
        return_proc = TRUE,
        n_permutations = 1,
        save_intermediate = TRUE,
        # Results are saved with qs::qsave()
        save_path = paste0("removeme_tests.qs")
    )

    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})

test_that("rROC with plotting and multiple dependent vars", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = c("outcome", "gender"),
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        do_plots = FALSE
    )
    res_df_plots <- rROC(
        aSAH,
        dependent_vars = c("outcome", "gender"),
        independent_vars = "ndka",
        n_permutations = 2,
        positive_label = "Good",
        do_plots = TRUE
    )
    testthat::expect_true(TRUE) # Just that the test is not skipped as "empty"
})



test_that("rROC print, multiple features", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_df <- rROC(
        aSAH,
        dependent_vars = "outcome",
        independent_vars = c("ndka", "s100b"),
        n_permutations = 2,
        positive_label = "Good"
    )
    output <- testthat::capture_output(print(res_df))
    # Test that print(res_df) includes "rROC object" as text:
    testthat::expect_true(any(grepl("rROC object", output)))
})