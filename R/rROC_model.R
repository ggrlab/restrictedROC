#' @title Train a predictive model using (rROC-preprocessed) data
#' @description A convenience function to train a model using the data x_prepared to
#' predict the outcome y. Per default, the function is a wrapper around the
#' h2o.randomForest function.
#' @param x_prepared A data frame containing the preprocessed data to use in the
#' model. The data frame should not contain the outcome variable.
#' Usually the result of a call to \code{\link{prepare_modeldata}}.
#' @param y The outcome variable. Usually a factor.
#' @param init_h2o Whether to initialize h2o. Defaults to TRUE.
#' @param h2o_trainfun The function to use to train the model. Defaults to
#' \code{h2o.randomForest} with some default parameters.
#' Needs to have ``df``, ``col_y`` and ``cols_x`` as arguments where ``df`` is
#' the data frame to use, ``col_y`` is the column index of the outcome variable
#' and ``cols_x`` is a vector of column indices of the features to use to predict the
#' outcome.
#' @param ... Arguments passed to h2o_trainfun.
#' @export
train_rROC_h2o <- function(x_prepared,
                           y,
                           init_h2o = TRUE,
                           h2o_trainfun = function(df, col_y, cols_x, ...) {
                               h2o::h2o.randomForest(
                                   training_frame = df,
                                   y = col_y,
                                   x = cols_x,
                                   ntrees = 1000,
                                   max_depth = 20, # default
                                   min_rows = 1, # default
                                   nbins = 20, # default
                                   seed = 4242,
                                   ...
                               )
                           },
                           ...) {
    if (all(is.null(levels(y)))) {
        unique_y <- sort(unique(y))
    } else {
        unique_y <- levels(y)
    }

    train_data <- cbind(x_prepared, factor(y, levels = unique_y))
    data.table::fwrite(train_data, file = "h2o_train.csv")
    if (init_h2o) {
        # library(h2o)
        h2o::h2o.init()
    }
    # Without normalizePath(), testthat will not find the file (and fail)
    train_h2o <- h2o::h2o.importFile(normalizePath("h2o_train.csv"))

    h2o_model <- h2o_trainfun(
        df = train_h2o,
        col_y = ncol(train_h2o),
        cols_x = 1:(ncol(train_h2o) - 1),
        ...
    )
    return(h2o_model)
}


#' @title Predict new data with a trained model
#' @description
#' A convenience function to predict new data with a trained model.
#' The function is a wrapper around the h2o.predict function.
#' It returns a list with the predictions and optionally the metrics.
#' @param h2o_model The model to use for prediction.
#' @param x_prepared A data frame containing the preprocessed data to use in the
#' model. The data frame can contain more variables than necessary within h2o_model
#' @param y The true outcome variable. Usually a factor.
#' @param init_h2o Whether to initialize h2o. Defaults to FALSE as I expect it was
#' initialized during training already.
#' @param calculate_metrics Whether to calculate metrics. Defaults to TRUE.
#' @param sample_split A vector of length(y) containing the "split" of the data.
#' The metrices are calculated for each split separately. Defaults to NULL, which means
#' that the whole data is used.
#' @export
predict_rROC_h2o <- function(h2o_model,
                             x_prepared,
                             y,
                             init_h2o = FALSE,
                             calculate_metrics = TRUE,
                             sample_split = NULL) {
    if (all(is.null(levels(y)))) {
        unique_y <- sort(unique(y))
    } else {
        unique_y <- levels(y)
    }
    df_00 <- cbind(x_prepared, y = factor(y, levels = unique_y))
    data.table::fwrite(df_00, file = "h2o_df_00.csv")
    if (init_h2o) {
        # library(h2o)
        h2o::h2o.init()
    }
    # Without normalizePath(), testthat will not find the file (and fail)
    df_00_h2o <- h2o::h2o.importFile(normalizePath("h2o_df_00.csv"))

    preds <- tibble::as_tibble(h2o::h2o.predict(h2o_model, newdata = df_00_h2o))
    retdf <- tibble::as_tibble(cbind(df_00, preds))
    res <- list(
        "predictions" = retdf
    )
    if (calculate_metrics) {
        if (!all(is.null(sample_split))) {
            if (length(sample_split) != length(y)) {
                stop("sample_split must be of length(y)")
            }
            split_samples <- split(retdf, sample_split)
        } else {
            split_samples <- list("all" = retdf)
        }
        res[["metrics"]] <- lapply(split_samples, function(x) {
            h2o::h2o.make_metrics(
                predicted = h2o::as.h2o(x[, ncol(x)]),
                actuals = h2o::as.h2o(x[["y"]])
            )
        })
    }
    return(res)
}
