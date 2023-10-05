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

for (data_x in list(data_pfs)) {
    for (subcohort_XX in rownames(table_lsubcohort_pfs)) {
        for (datatype in c("bounded", "full")) {
            cat(
                "\n\nsubcohort: ", subcohort_XX,
                "  datatype: ", datatype,
                "\n\n"
            )
            savepath <- paste0(
                "intermediate_data/2022-lee/leave_one_out/",
                subcohort_XX,
                "_",
                datatype
            )
            samples_boolean_train <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] != subcohort_XX
            samples_boolean_test <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] == subcohort_XX

            rroc_predictions_traintest <- wrapper_predictions_rroc(
                summarized_experiment = data_x,
                boolean_train = samples_boolean_train,
                boolean_test = samples_boolean_test,
                outcome = outcome,
                positive_label = "yes"
            )

            model_and_predictions <- wrapper_modelling(
                train_x = rroc_predictions_traintest[["train"]][["predictions"]][[datatype]],
                train_y = SummarizedExperiment::colData(data_x)[samples_boolean_train, ][[outcome]],
                test_x = rroc_predictions_traintest[["test"]][["predictions"]][[datatype]],
                test_y = SummarizedExperiment::colData(data_x)[samples_boolean_test, ][[outcome]]
            )

            h2o::h2o.saveModel(
                model_and_predictions[["model"]],
                path = paste0(savepath, ".model_h2o")
            )
            model_and_predictions_nomodel <- model_and_predictions[-1]
            qs::qsave(rroc_predictions_traintest, file = paste0(savepath, "-rroc_predictions_traintest.qrds"))
            qs::qsave(model_and_predictions_nomodel, file = paste0(savepath, "-model_and_predictions_nomodel.qrds"))
        }
    }
}



####
sink("dev/lee2022-cross-cohort.txt")
for (data_x in list(data_pfs)) {
    for (subcohort_a in rownames(table_lsubcohort_pfs)) {
        for (subcohort_b in rownames(table_lsubcohort_pfs)) {
            if (subcohort_a == subcohort_b) {
                next
            }
            for (datatype in c("bounded", "full")) {
                cat(
                    "\n\n\n\n\nsubcohort_a: ", subcohort_a,
                    "subcohort_b: ", subcohort_b,
                    "  datatype: ", datatype,
                    "\n\n"
                )
                savepath <- paste0(
                    "intermediate_data/2022-lee/leave_one_out/",
                    subcohort_a, ".vs.", subcohort_b,
                    "_",
                    datatype
                )
                samples_boolean_train <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] == subcohort_a
                samples_boolean_test <- SummarizedExperiment::colData(data_x)[["lee_subcohort"]] == subcohort_b

                rroc_predictions_traintest <- wrapper_predictions_rroc(
                    summarized_experiment = data_x,
                    boolean_train = samples_boolean_train,
                    boolean_test = samples_boolean_test,
                    outcome = outcome,
                    positive_label = "yes"
                )

                model_and_predictions <- wrapper_modelling(
                    train_x = rroc_predictions_traintest[["train"]][["predictions"]][[datatype]],
                    train_y = SummarizedExperiment::colData(data_x)[samples_boolean_train, ][[outcome]],
                    test_x = rroc_predictions_traintest[["test"]][["predictions"]][[datatype]],
                    test_y = SummarizedExperiment::colData(data_x)[samples_boolean_test, ][[outcome]]
                )

                h2o::h2o.saveModel(
                    model_and_predictions[["model"]],
                    path = paste0(savepath, ".model_h2o")
                )
                model_and_predictions_nomodel <- model_and_predictions[-1]
                qs::qsave(rroc_predictions_traintest, file = paste0(savepath, "-rroc_predictions_traintest.qrds"))
                qs::qsave(model_and_predictions_nomodel, file = paste0(savepath, "-model_and_predictions_nomodel.qrds"))
            }
        }
    }
}
sink()
