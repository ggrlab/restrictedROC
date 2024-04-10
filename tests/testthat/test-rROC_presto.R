testthat::test_that("rROC duplicate dependent/independent errors", {
    # library(restrictedROC)
    data("aSAH", package = "pROC")
    set.seed(100)
    times <- 10
    # pacman::p_install("microbenchmark")
    mb_rroc <- microbenchmark::microbenchmark(
        rROC(
            aSAH,
            dependent_vars = "outcome",
            independent_vars = "ndka",
            n_permutations = 0,
            positive_label = "Good",
            verbose = FALSE
        ),
        times = times
    )

    mb_rroc_presto <- microbenchmark::microbenchmark(
        rROC(
            aSAH,
            dependent_vars = "outcome",
            independent_vars = "ndka",
            n_permutations = 0,
            positive_label = "Good",
            verbose = FALSE,
            get_all_aucs_fun = get_all_aucs_presto
        ),
        times = times
    )
    # devtools::install_github("immunogenomics/presto")

    mb_presto <- microbenchmark::microbenchmark(
        tmp <- presto::wilcoxauc(
            # A feature-by-sample matrix,
            X = t(aSAH$ndka),
            y = aSAH$outcome
        ),
        times = times
    )


    bound_perfs <- rbind(
        mb_rroc,
        mb_presto,
        mb_rroc_presto
    )
    levels(bound_perfs[["expr"]]) <- c("rROC", "presto", "rroc_presto")
    print(bound_perfs)

    str(mb_rroc)
})
