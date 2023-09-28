test_that("Permutation workflow", {
    library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    res_rroc <- simple_rROC_permutation(
        response = aSAH[["outcome"]],
        predictor = aSAH[["ndka"]],
        n_permutations = 25,
        positive_label = "Good"
    )
    # print(res_rroc$global)
    # #        auc auc_var_H0     rzAUC pval_asym
    # # 1 0.388042   0.388042 -1.973565 0.0484312
    testthat::expect_equal(res_rroc$global$auc, 0.388042, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$global$auc_var_H0, 0.388042, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$global$rzAUC, -1.9735652, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$global$pval_asym, 0.0484312, tolerance = 1e-5)

    # print(res_rroc$max_total)
    # #         auc  auc_var_H0     rzAUC   pval_asym threshold part
    # # 1 0.3397963 0.003492623 -2.710795 0.006712216     5.685 high
    testthat::expect_equal(res_rroc$max_total$auc, 0.33979632, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$max_total$auc_var_H0, 0.003492623, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$max_total$rzAUC, -2.7107947, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$max_total$pval_asym, 0.006712216, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$max_total$threshold, 5.685, tolerance = 1e-5)
    testthat::expect_equal(res_rroc$max_total$part, "high")

    # print(res_rroc$permutation_pval)
    # #    pval.twoside.max pval.twoside.global      n_permutations
    # #          0.23076923          0.03846154         25.00000000
    testthat::expect_equal(
        res_rroc$permutation_pval[["pval.twoside.max"]], 0.23076923,
        tolerance = 1e-5
    )
    testthat::expect_equal(
        res_rroc$permutation_pval[["pval.twoside.global"]], 0.03846154,
        tolerance = 1e-5
    )
    testthat::expect_equal(
        res_rroc$permutation_pval[["n_permutations"]], 25,
        tolerance = 1e-5
    )
})
