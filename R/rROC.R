# All S3 methods must be exported https://github.com/r-lib/devtools/issues/2293

#' Restriction for multiple dependent and independent variables
#'
#' @param x
#' \itemize{
#'  \item {data.frame}: {
#'      See \code{\link{rROC.data.frame}}. data.frame containing all dependent and
#'      independent variables as columns. Dependent/independent variable column names
#'      must be given as "dependent_vars"/"independent_vars" arguments.
#' }
#'  \item {matrix}: {
#'      See \code{\link{rROC.matrix}}. Matrix of (samples x features).
#'      Dependent variable(s) must be given as "y" argument.
#' }
#'  \item {numeric vector}: {
#'      See \code{\link{rROC.numeric}}. Numeric vector of independent variable.
#'      Dependent variable(s) must be given as "y" argument.
#' }
#' }
#' @param ... TODO
#' @inheritDotParams rROC.data.frame -dependent_vars -independent_vars
#' @inheritDotParams simple_rROC_permutation -response -predictor -direction
#' @export
rROC <- function(x, ...) {
    # R possesses a simple generic function mechanism which can be used for an
    # object-oriented style of programming. Method dispatch takes place based on
    # the class(es) of the first argument to the generic function or of the object
    # supplied as an argument to UseMethod or NextMethod.
    UseMethod("rROC", x)
}

#' rROC on a matrix of (samples x features)
#'
#' @param x Matrix, rows are samples, columns are features. Every feature should be restricted.
#' @param y Factor vector, values of the dependent variable for every column (element/sample).
#' @inheritDotParams rROC.data.frame -dependent_vars -independent_vars
#' @export
rROC.matrix <- function(x, y, ...) {
    if (is.list(y)) {
        # Then y is an actual list (not data.frame) of dependent variables
        y_df <- data.frame(y, check.names = FALSE)
    } else {
        y_df <- data.frame("y" = y)
    }

    if (!nrow(x) == nrow(y_df)) {
        # x: input matrix, of dimension nobs x nvars; each row is an
        # observation vector. Can be in sparse matrix format (inherit
        # from class '"sparseMatrix"' as in package 'Matrix')
        stop(
            paste0(
                "Following the convention of glmnet: Rows are samples, columns are ",
                "features. y (the response) must be a vector of length nrow(x) or a list",
                "of vectors of length nrow(x)."
            )
        )
    }

    x <- tibble::as_tibble(data.frame(x))
    dependent_vars <- colnames(y_df)
    independent_vars <- colnames(x)
    x <- tibble::add_column(x, y_df)
    rROC.data.frame(
        x = x,
        dependent_vars = dependent_vars,
        independent_vars = independent_vars,
        ...
    )
}


#' rROC on a single numeric vector
#'
#' @param x Numeric vector, values of the independent variable for every element/sample.
#' @param y Factor vector, values of the dependent variable for every element/sample.
#' @inheritDotParams rROC.data.frame -dependent_vars -independent_vars
#' @export
rROC.numeric <- function(x, y, ...) {
    if (is.list(y)) {
        # Then y is an actual list (not data.frame) of dependent variables
        y_df <- data.frame(y, check.names = FALSE)
    } else {
        y_df <- data.frame("y" = y)
    }

    if (length(x) != nrow(y_df)) {
        stop("x and y must have the same length")
    }

    dependent_vars <- colnames(y_df)
    x <- tibble::tibble("x" = x)
    x <- tibble::add_column(x, y_df)
    rROC.data.frame(
        x = x,
        dependent_vars = dependent_vars,
        independent_vars = "x",
        ...
    )
}

#' @title Restriction for multiple dependent and independent variables
#' @description
#' Restriction for multiple dependent and independent variables.
#' Traverses all dependent variables and within all independent variables.
#' Then calculates the rROC in the sense of dependent ~ independent.
#'
#' Can save intermediate results to disk, to avoid re-calculating for crash-reasons
#' or to save time when re-running the same analysis.
#'
#' Can return plot_density_rROC_empirical for every combination.
#'
#' @param x A data.frame containing all dependent and independent variables as columns.
#' @param dependent_vars A character vector of dependent variable column names.
#' @param independent_vars A character vector of independent variable column names.
#' If NULL, all columns except dependent_vars are used.
#' @param save_path Path to save the results to. Intermediate results are saved into
#' the directory file.path(save_path, "_partial_directory").
#' @param save_intermediate Should intermediate results be saved to disk? If TRUE,
#' every combination by itself is saved into file.path(save_path, "_partial_directory").
#' @param load_existing_intermediate Should the earlier saved intermediate results
#' in the folder file.path(save_path, "_partial_directory") be loaded?
#' @param do_plots Should the plot_density_rROC_empirical be calculated and returned?
#' @param verbose Should progress be printed?
#' @inheritParams simple_rROC_permutation
#' @inheritDotParams simple_rROC_permutation -response -predictor -direction
#' @export
rROC.data.frame <- function(x,
                            dependent_vars,
                            independent_vars,
                            save_path = NULL,
                            parallel_permutations = TRUE,
                            n_permutations = 10000,
                            save_intermediate = TRUE,
                            load_existing_intermediate = TRUE,
                            positive_label = 1,
                            verbose = TRUE,
                            do_plots = FALSE,
                            fix_seed = 0,
                            ...) {
    original_positive_label <- positive_label
    if (is.null(save_path)) {
        save_intermediate <- load_existing_intermediate <- FALSE
    }
    if (anyDuplicated(dependent_vars) != 0) {
        stop("dependent_vars must be unique")
    }
    if (is.null(independent_vars)) {
        independent_vars <- colnames(x)[!(colnames(x) %in% dependent_vars)]
    }
    if (anyDuplicated(independent_vars) != 0) {
        stop("independent_vars must be unique")
    }

    reslist <- list()
    for (dv in dependent_vars) {
        positive_label <- original_positive_label
        if (!positive_label %in% x[[dv]]) {
            positive_label <- sort(unique(x[[dv]]))[1]
            warning(
                paste0(
                    "positive_label '", original_positive_label, "' not in '", dv,
                    "'., Changing to the first sorted unique value of '", dv, "': '",
                    positive_label, "'"
                )
            )
        }
        reslist[[dv]] <- list()
        for (iv in independent_vars) {
            if (verbose) {
                cat(date(), "    ", dv, iv, "\n")
            }
            current_file <- file.path(paste0(save_path, "_partial_directory"), paste0(dv, "_____", gsub("/", ".", iv), ".qs"))
            if (file.exists(current_file) && load_existing_intermediate) {
                if (verbose) {
                    cat("    loaded\n")
                }
                reslist[[dv]][[iv]] <- qs::qread(current_file)
            } else {
                reslist[[dv]][[iv]] <- list()
                df <- tibble::tibble(
                    "response" = x[[dv]],
                    "predictor" = x[[iv]]
                )
                df$predictor[is.na(df$predictor)] <- median(df$predictor, na.rm = TRUE)
                per_resp_list <- split(df$predictor, df$response)

                if (length(per_resp_list) == 1 || any(vapply(per_resp_list, length, FUN.VALUE = numeric(1)) == 1)) {
                    plot_full_rroc <- NA
                    rroc_permutation <- NA
                } else {
                    if (do_plots) {
                        plot_full_rroc <- plot_density_rROC_empirical(
                            rev(per_resp_list),
                            xmin = min(df$predictor),
                            xmax = max(df$predictor),
                            positive_label = positive_label,
                            direction = "<",
                        )
                    } else {
                        plot_full_rroc <- NA
                    }
                    rroc_permutation <- simple_rROC_permutation(
                        response = x[[dv]],
                        predictor = x[[iv]],
                        positive_label = positive_label,
                        direction = "<",
                        parallel_permutations = parallel_permutations,
                        n_permutations = n_permutations,
                        fix_seed = fix_seed,
                        ...
                    )
                }
                reslist[[dv]][[iv]][["plots"]] <- plot_full_rroc
                reslist[[dv]][[iv]][["permutation"]] <- rroc_permutation
                if (save_intermediate) {
                    dir.create(paste0(save_path, "_partial_directory"), showWarnings = FALSE, recursive = TRUE)
                    qs::qsave(reslist[[dv]][[iv]], current_file)
                    if (verbose) {
                        cat("    Calculated + wrote", current_file, "\n")
                    }
                }
            }
        }
    }
    if (!is.null(save_path)) {
        qs::qsave(reslist, paste0(save_path, ".qs"))
        cat("Wrote final reslist to ", paste0(save_path, ".qs"), "\n")
    }
    return(reslist)
}