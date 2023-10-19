train_rROC_h2o <- function(x_prepared,
                           y,
                           init_h2o = TRUE,
                           h2o_trainfun = function(df, col_y, cols_x, ...) {
                               h2o.randomForest(
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
        library(h2o)
        h2o.init()
    }
    train_h2o <- h2o.importFile("h2o_train.csv")

    h2o_model <- h2o_trainfun(
        df = train_h2o,
        col_y = ncol(train_h2o),
        cols_x = 1:(ncol(train_h2o) - 1),
        ...
    )
    return(h2o_model)
}

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
        library(h2o)
        h2o.init()
    }
    df_00_h2o <- h2o.importFile("h2o_df_00.csv")

    preds <- tibble::as_tibble(h2o.predict(h2o_model, newdata = df_00_h2o))
    retdf <- tibble::as_tibble(cbind(df_00, preds))
    res <- list(
        "predictions" = retdf
    )
    if (calculate_metrics) {
        if (!all(is.null(sample_split))) {
            split_samples <- split(retdf, sample_split)
        } else {
            split_samples <- list("all" = retdf)
        }
        preds_h2o <- as.h2o(x[, ncol(x)])
        actuals_h2o <- as.h2o(x[["y"]])

        res[["metrics"]] <- lapply(split_samples, function(x) {
            h2o::h2o.make_metrics(predicted = preds_h2o, actuals = actuals_h2o)
        })
    }
    return(res)
}
