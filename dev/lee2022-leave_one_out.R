wrapper <- function(
    summarized_experiment,
    boolean_train,
    boolean_test) {
    if (length(boolean_train) != length(boolean_test)) {
        stop("boolean_train and boolean_test must be the same length")
    }
    if (length(booleant_train) != ncol(summarized_experiment)) {
        stop("boolean_train must be the same length as the number of columns in summarized_experiment")
    }
}

for (data_x in list(data_pfs)) {
    for (subcohort_XX in rownames(table_lsubcohort_pfs)) {
        cat("\n\nsubcohort: ", subcohort_XX, "\n")
        samples_boolean_test <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] != subcohort_XX
        samples_boolean_train <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] == subcohort_XX

        stop()
        pheno_pfs_part <- SummarizedExperiment::colData(data_x)[samples_boolean, ]
        data_x_part <- data_x[, samples_boolean]
        rroc_permutation_many_features(
            x = SummarizedExperiment::assay(data_x_part)[1:3, ],
            response = pheno_pfs_part[["PFS12"]],
            save_intermediate_file = file.path(outdir, paste0(
                "dev-lee2022_pfs_rroc_nperm.", n_permutations,
                "_SUB.", subcohort_XX, ".qrds"
            )),
            n_permutations = n_permutations,
            positive_label = "yes",
            parallel_permutations = TRUE,
            verbose = TRUE
        )
    }
}
