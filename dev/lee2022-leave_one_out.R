source("dev/lee2022-utils.R")

library(ggplot2)
plot_heatmap <- function(df_data,
                         samples_boolean,
                         data_x_SummarizedExperiment,
                         name,
                         lee_subcohort_named_colors = c(
                             "FrankelAE_2017" = "#8DD3C7",
                             "GopalakrishnanV_2018" = "#FFFFB3",
                             "Barcelona" = "#BEBADA",
                             "Leeds" = "#FB8072",
                             "Manchester" = "#80B1D3",
                             "PRIMM-NL" = "#FDB462",
                             "PRIMM-UK" = "#B3DE69",
                             "MatsonV_2018" = "#FCCDE5",
                             "PetersBA_2019" = "#D9D9D9",
                             "WindTT_2020" = "#BC80BD"
                         )) {
    df_data_cSamples_rFeatures <- t(as.matrix(df_data))
    f_data <- tibble::tibble(
        restrictedness = apply(df_data_cSamples_rFeatures, 1, function(x) {
            sum(is.na(x)) / length(x)
        }),
        variance = apply(df_data_cSamples_rFeatures, 1, function(x) {
            var(x, na.rm = TRUE)
        }),
    )
    f_data[["variance"]][is.na(f_data[["variance"]])] <- 0

    p_data <- as.data.frame(SummarizedExperiment::colData(
        data_x_SummarizedExperiment
    )[samples_boolean, ])[, "lee_subcohort", drop = FALSE]
    # remove zero variance features
    df_data_cSamples_rFeatures <- df_data_cSamples_rFeatures[f_data[["variance"]] != 0, ]
    f_data <- f_data[f_data[["variance"]] != 0, ]
    ComplexHeatmap::Heatmap(
        df_data_cSamples_rFeatures,
        name = name,
        show_column_names = FALSE,
        show_row_names = FALSE,
        column_title = "samples",
        row_title = "features",
        row_order = order(f_data[["restrictedness"]]),
        cluster_columns = TRUE,
        # na_col = "#00ff4c25",
        na_col = "black",
        right_annotation = ComplexHeatmap::rowAnnotation(
            df = f_data,
            col = list(
                "restrictedness" = circlize::colorRamp2(
                    col = c("#04a804", "#e90000"),
                    breaks = c(0, 1)
                ),
                "variance" = circlize::colorRamp2(
                    range(f_data[["variance"]]),
                    hcl_palette = "Blue-Red 2"
                )
            )
        ),
        top_annotation = ComplexHeatmap::columnAnnotation(
            df = p_data,
            col = list("lee_subcohort" = lee_subcohort_named_colors)
        )
    )
}


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
            if (datatype == "full") {
                dir.create("intermediate_data/2022-lee/results", recursive = TRUE, showWarnings = FALSE)
                pdf(
                    paste0(
                        "intermediate_data/2022-lee/results/LOO-restriction_heatmap-",
                        basename(savepath), ".pdf"
                    ),
                    width = 20, height = 25
                )
                for (tt in c("train", "test")) {
                    if (tt == "train") {
                        samples_boolean <- samples_boolean_train
                    } else {
                        samples_boolean <- samples_boolean_test
                    }
                    ComplexHeatmap::draw(
                        plot_heatmap(
                            df_data = rroc_predictions_traintest[[tt]][["predictions"]][["restricted"]],
                            samples_boolean = samples_boolean,
                            data_x_SummarizedExperiment = data_x,
                            name = tt
                        ),
                        column_title = tt,
                        column_title_gp = grid::gpar(fontsize = 16)
                    )
                }
                dev.off()
            }
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
