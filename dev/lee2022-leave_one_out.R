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
            if (!file.exists(paste0(savepath, ".model_h2o"))) {
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
            } else {
                rroc_predictions_traintest <- qs::qread(paste0(savepath, "-rroc_predictions_traintest.qrds"))
                model_and_predictions_nomodel <- qs::qread(paste0(savepath, "-model_and_predictions_nomodel.qrds"))
            }

            # plotting
            library(ggplot2)
            df_data <- rroc_predictions_traintest[["train"]][["predictions"]][["restricted"]]
            df_data_cSamples_rFeatures <- t(as.matrix(df_data))
            f_data <- tibble::tibble(
                restrictedness = apply(df_data_cSamples_rFeatures, 1, function(x) {
                    sum(is.na(x)) / length(x)
                }),
                variance = apply(df_data_cSamples_rFeatures, 1, function(x) {
                    var(x, na.rm = TRUE)
                }),
            )

            p_data <- as.data.frame(SummarizedExperiment::colData(data_x)[samples_boolean_train, ])[, "lee_subcohort", drop = FALSE]
            # remove zero variance features
            df_data_cSamples_rFeatures <- df_data_cSamples_rFeatures[f_data[["variance"]] != 0, ]
            f_data <- f_data[f_data[["variance"]] != 0, ]
            pdf("removeme.pdf", width = 25, height = 20)
            ComplexHeatmap::Heatmap(
                df_data_cSamples_rFeatures,
                name = "train",
                show_column_names = FALSE,
                show_row_names = FALSE,
                column_title = "samples",
                row_title = "features",
                cluster_rows = TRUE,
                clustering_distance_rows = function(m) {
                    m[is.na(m)] <- min(m, na.rm = TRUE) - 1
                    calc_dist <- dist(m)
                    # calc_dist[is.na(calc_dist)] <- max(calc_dist, na.rm = TRUE)
                    # calc_dist[is.na(calc_dist)] <- 0
                    return(calc_dist)
                },
                cluster_columns = TRUE,
                na_col = "#00ff4c25",
                right_annotation = ComplexHeatmap::rowAnnotation(
                    df = f_data,
                    col = list(
                        "restrictedness" = circlize::colorRamp2(
                            col = c("#04a804", "#e90000"),
                            breaks = c(0, 1)
                        )
                    )
                ),
                top_annotation = ComplexHeatmap::columnAnnotation(
                    df = p_data,
                    col = list(
                        "lee_subcohort" = structure(
                            RColorBrewer::brewer.pal(
                                n = length(unique(p_data[["lee_subcohort"]])),
                                name = "Dark2"
                            ),
                            names = unique(p_data[["lee_subcohort"]])
                        )
                    )
                )
            )
            ComplexHeatmap::Heatmap(
                df_data_cSamples_rFeatures,
                name = "train",
                show_column_names = FALSE,
                show_row_names = FALSE,
                column_title = "samples",
                row_title = "features",
                row_order = order(f_data[["restrictedness"]]),
                cluster_columns = TRUE,
                na_col = "#00ff4c25",
                right_annotation = ComplexHeatmap::rowAnnotation(
                    df = f_data,
                    col = list(
                        "restrictedness" = circlize::colorRamp2(
                            col = c("#04a804", "#e90000"),
                            breaks = c(0, 1)
                        )
                    )
                ),
                top_annotation = ComplexHeatmap::columnAnnotation(
                    df = p_data,
                    col = list(
                        "lee_subcohort" = structure(
                            RColorBrewer::brewer.pal(
                                n = length(unique(p_data[["lee_subcohort"]])),
                                name = "Dark2"
                            ),
                            names = unique(p_data[["lee_subcohort"]])
                        )
                    )
                )
            )
            ComplexHeatmap::Heatmap(
                df_data_cSamples_rFeatures,
                name = "train",
                show_column_names = FALSE,
                show_row_names = FALSE,
                column_title = "samples",
                row_title = "features",
                row_order = order(f_data[["variance"]]),
                cluster_columns = TRUE,
                na_col = "#00ff4c25",
                right_annotation = ComplexHeatmap::rowAnnotation(
                    df = f_data,
                    col = list(
                        "restrictedness" = circlize::colorRamp2(
                            col = c("#04a804", "#e90000"),
                            breaks = c(0, 1)
                        ),
                        "variance" = circlize::colorRamp2(
                            col = c("#04a804", "#e90000"),
                            breaks = c(0, 1)
                        )
                    )
                ),
                top_annotation = ComplexHeatmap::columnAnnotation(
                    df = p_data,
                    col = list(
                        "lee_subcohort" = structure(
                            RColorBrewer::brewer.pal(
                                n = length(unique(p_data[["lee_subcohort"]])),
                                name = "Dark2"
                            ),
                            names = unique(p_data[["lee_subcohort"]])
                        )
                    )
                )
            )
            dev.off()
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
