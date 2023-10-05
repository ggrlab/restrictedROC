source("dev/lee2022-utils.R")
library(Biobase)
library(TreeSummarizedExperiment)
qs::qload("intermediate_data/2022-lee/dev-data_lee.rds")
outcome <- "PFS12"
data_pfs <- data_lee[, !is.na(SummarizedExperiment::colData(data_lee)[[outcome]])]
table_lsubcohort_pfs <- table(
    SummarizedExperiment::colData(data_lee)[["lee_subcohort"]],
    factor(SummarizedExperiment::colData(data_lee)[[outcome]], levels = c("yes", "no"))
)
# remove the cohorts without samples
table_lsubcohort_pfs <- table_lsubcohort_pfs[apply(table_lsubcohort_pfs, 1, sum) != 0, ]
repeated_cv_n <- 100
k_fold_cv <- 5
for (data_x in list(data_pfs)) {
    for (subcohort_XX in c("all", rownames(table_lsubcohort_pfs))) {
        for (datatype in c("bounded", "full")) {
            cat(
                "\n\nsubcohort: ", subcohort_XX,
                "  datatype: ", datatype,
                "\n\n"
            )
            savepath <- paste0(
                "intermediate_data/2022-lee/CrossValidate/",
                subcohort_XX,
                "_",
                datatype
            )
            dir.create(dirname(savepath), recursive = TRUE, showWarnings = FALSE)
            if (subcohort_XX == "all") {
                data_subX <- data_x
            } else {
                data_subX <- data_x[, SummarizedExperiment::colData(data_x)[["lee_subcohort"]] == subcohort_XX]
            }

            repeated_cv_results <- list()
            for (repeat_i in seq_len(repeated_cv_n)) {
                sampled_cv_ids <- sample(seq_len(ncol(data_subX)), replace = FALSE)
                splitted_ids <- split(sampled_cv_ids, rep_len(1:k_fold_cv, length(sampled_cv_ids)))
                if (length(splitted_ids[length(splitted_ids)]) < 4) {
                    splitted_ids[[length(splitted_ids) - 1]] <- c(
                        splitted_ids[[length(splitted_ids) - 1]],
                        splitted_ids[[length(splitted_ids)]]
                    )
                    splitted_ids <- splitted_ids[-length(splitted_ids)]
                    k_fold_cv_current <- k_fold_cv - 1
                } else {
                    k_fold_cv_current <- k_fold_cv
                }

                cv_results <- list()
                for (cv_i in seq_len(k_fold_cv_current)) {
                    savepath_cv <- paste0(savepath, "_repeat.", repeat_i, "_cv.", cv_i)
                    samples_boolean_test <- seq_len(ncol(data_subX)) %in% splitted_ids[[cv_i]]
                    samples_boolean_train <- !samples_boolean_test
                    if (
                        (sum(samples_boolean_train) + sum(samples_boolean_test)) !=
                            ncol(data_subX)) {
                        stop("Something went wrong when assigning samples_boolean_train and samples_boolean_test")
                    }
                    cv_results[[cv_i]] <- wrapper_traintest(
                        data_x = data_subX,
                        samples_boolean_train = samples_boolean_train,
                        samples_boolean_test = samples_boolean_test,
                        outcome = outcome,
                        savepath = savepath_cv
                    )
                }
                cv_all_preds <- do.call(
                    rbind,
                    lapply(cv_results, function(x) {
                        x[["pred_test"]]
                    })
                )
                cv_all_roc <- pROC::roc(
                    response = cv_all_preds[["y"]],
                    predictor = cv_all_preds[, 4], # cv_all_preds[["yes"]],
                    direction = "<",
                    levels = levels(cv_all_preds[["y"]])
                )
                repeated_cv_results[[repeat_i]] <- list(
                    "cv_results" = cv_results,
                    "cv_all_preds" = cv_all_preds,
                    "cv_all_roc" = cv_all_roc
                )
                qs::qsave(repeated_cv_results, file = paste0(savepath, "-repeated_cv_results.qrds"))
            }
        }
    }
}
