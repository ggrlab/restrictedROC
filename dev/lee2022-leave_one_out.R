gather_predictions <- function(predicted_rroc_list) {
    if (any(duplicated(names(predicted_rroc_list)))) {
        stop("Duplicated feature names. Please check the input list.")
    }

    per_feature_rroc <- lapply(names(predicted_rroc_list), function(x) {
        data_x <- predicted_rroc_list[[x]]
        preds <- data_x[["pred"]]
        preds_restricted <- preds$predictor
        preds_restricted[!preds$keep] <- NA
        tmp_df <- tibble::tibble(
            "full" = preds[["predictor"]],
            "restricted" = preds_restricted,
            "keep" = preds$keep
        )
        tmp_df[["bounded"]] <- ifelse(
            tmp_df[["keep"]],
            tmp_df[["full"]],
            # data_x[["threshold_and_restriction"]][["restriction"]]
            -1
        )
        colnames(tmp_df) <- paste0(colnames(tmp_df), "___", x)
        return(tmp_df)
    })

    tmp <- do.call(rbind, lapply(predicted_rroc_list, function(y) y[["threshold_and_restriction"]]))
    per_feature_rroc_thresholds <- tibble::as_tibble(
        cbind("feature" = names(predicted_rroc_list), tmp),
        rownames = "feature"
    )

    per_feature_rroc_bound <- tibble::as_tibble(do.call(cbind, per_feature_rroc))
    col_type <- sapply(strsplit(colnames(per_feature_rroc_bound), "___", fixed = TRUE), function(x) x[[1]])
    col_types_split <- split(colnames(per_feature_rroc_bound), col_type)
    predictions <- lapply(col_types_split, function(colnames_x) {
        tmp <- per_feature_rroc_bound[, colnames_x]
        colnames(tmp) <- sub("^.*___", "", colnames(tmp))
        return(tmp)
    })
    return(
        list(
            "predictions_bound" = per_feature_rroc_bound,
            "predictions" = predictions,
            "thresholds" = per_feature_rroc_thresholds
        )
    )
}

wrapper_predictions_rroc <- function(summarized_experiment,
                                     boolean_train,
                                     boolean_test,
                                     outcome = "PFS12",
                                     ...) {
    if (length(boolean_train) != length(boolean_test)) {
        stop("boolean_train and boolean_test must be the same length")
    }
    if (length(boolean_train) != ncol(summarized_experiment)) {
        stop("boolean_train must be the same length as the number of columns in summarized_experiment")
    }
    train_sE <- summarized_experiment[, boolean_train]
    test_sE <- summarized_experiment[, boolean_test]

    train_x <- SummarizedExperiment::assay(train_sE)
    test_x <- SummarizedExperiment::assay(test_sE)
    # From ?restrictedROC::predict.restrictedROC:
    #' tmp <- simple_rROC(
    #'     response = aSAH$outcome,
    #'     predictor = aSAH$ndka,
    #'     return_proc = TRUE
    #' )
    #' single_rROC <- simple_rROC_interpret(tmp)
    #'
    #' predict(
    #'     single_rROC,
    #'     newdata = aSAH,
    #'     newdata_predictor_column = "ndka",
    #'     newdata_response_column = "outcome",
    #'     pred_high_label = "Poor",
    #'     pred_low_label = "Good"
    #' )
    res_rroc <- apply(train_x, 1, function(x) {
        restrictedROC::simple_rROC(
            response = train_sE[[outcome]],
            predictor = x,
            return_proc = TRUE,
            ...
            # positive_label = "yes",
        )
    })
    res_rroc_interpret <- lapply(res_rroc, restrictedROC::simple_rROC_interpret)
    # res_rroc_interpret[[1]]

    train_restricted <- sapply(names(res_rroc_interpret), function(feature_x) {
        tmp_df <- data.frame(train_x[feature_x, ], train_sE[[outcome]])
        colnames(tmp_df) <- c(feature_x, outcome)
        restrictedROC:::predict.restrictedROC(
            object = res_rroc_interpret[[feature_x]],
            newdata = tmp_df,
            newdata_predictor_column = feature_x,
            newdata_response_column = outcome,
            pred_high_label = "yes",
            pred_low_label = "no"
        )
    }, USE.NAMES = TRUE, simplify = FALSE)

    test_restricted <- sapply(names(res_rroc_interpret), function(feature_x) {
        tmp_df <- data.frame(test_x[feature_x, ], test_sE[[outcome]])
        colnames(tmp_df) <- c(feature_x, outcome)
        restrictedROC:::predict.restrictedROC(
            object = res_rroc_interpret[[feature_x]],
            newdata = tmp_df,
            newdata_predictor_column = feature_x,
            newdata_response_column = outcome,
            pred_high_label = "yes",
            pred_low_label = "no"
        )
    }, USE.NAMES = TRUE, simplify = FALSE)
    gathered_train <- gather_predictions(train_restricted)
    gathered_test <- gather_predictions(test_restricted)
    return(
        list(
            "train" = gathered_train,
            "test" = gathered_test
        )
    )
}

wrapper_modelling <- function(train_x, train_y, test_x, test_y, verbose = TRUE, ...) {
    train_data <- cbind(train_x, factor(train_y))
    test_data <- cbind(test_x, factor(test_y, levels = levels(train_data[, ncol(train_data)])))
    data.table::fwrite(train_data, file = "h2o_train.csv")
    data.table::fwrite(test_data, file = "h2o_test.csv")

    library(h2o)
    h2o.init()
    # prostate_path = system.file("extdata", "prostate.csv", package = "h2o")
    # prostate = h2o.importFile(path = prostate_path)
    read_h2o <- function(file_path, original_data = NA) {
        tmp <- h2o.importFile(file_path, header = TRUE)
        if (nrow(tmp) == nrow(original_data) + 1) {
            tmp <- tmp[-1, ]
        }
        if (!all(is.na(original_data))) {
            if (!all.equal(
                as.data.frame(tmp),
                original_data,
                tolerance = 1e-5,
                check.names = FALSE
            )) {
                stop("Writing/Reading into h2o went wrong.")
            }
        }
        return(tmp)
    }

    train_h2o <- read_h2o("h2o_train.csv", train_data)
    test_h2o <- read_h2o("h2o_test.csv", test_data)


    drf <- h2o.randomForest(
        training_frame = train_h2o,
        y = ncol(train_h2o),
        x = 1:(ncol(train_h2o) - 1),
        ntrees = 1000,
        max_depth = 20, # default
        min_rows = 1, # default
        nbins = 20, # default
        seed = 4242,
        ...
    )

    pred_drf.train <- cbind(
        y = train_data[, ncol(train_data)],
        tibble::as_tibble(h2o.predict(drf, newdata = train_h2o))
    )
    table_train <- table(pred_drf.train[, c("y", "predict")])

    pred_drf.test <- cbind(
        y = test_data[, ncol(test_data)],
        tibble::as_tibble(h2o.predict(drf, newdata = test_h2o))
    )
    table_test <- table(pred_drf.test[, c("y", "predict")])

    roc_train <- pROC::roc(
        response = pred_drf.train[["y"]],
        predictor = pred_drf.train[, 4], # pred_drf.train[["yes"]],
        direction = "<", levels = levels(pred_drf.train[["y"]])
    )
    roc_test <- pROC::roc(
        response = pred_drf.test[["y"]],
        predictor = pred_drf.test[, 4], # pred_drf.test[["yes"]],
        direction = "<", levels = levels(pred_drf.test[["y"]])
    )
    if (verbose) {
        cat("Train\n")
        print(table_train)

        cat("Test\n")
        print(table_test)
        # print(fisher.test(table_test))
        print(roc_train)
        print(roc_test)
    }
    return(
        list(
            "model" = drf,
            "pred_train" = pred_drf.train,
            "pred_test" = pred_drf.test,
            "table_train" = table_train,
            "table_test" = table_test,
            "train" = roc_train,
            "test" = roc_test
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
