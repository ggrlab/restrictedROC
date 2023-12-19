simple_rROC_paired <- function(x,
                               y,
                               positive_label = NULL,
                               get_all_aucs_fun = partwise_results_wilcox_paired,
                               n_tested_thresholds = NA,
                               #    n_tested_thresholds = 100,
                               return_proc = FALSE,
                               do_parallel = FALSE,
                               check_positive_negative_count = FALSE) {
    diff <- x - y

    complete_df <- tibble(
        x = x,
        y = y
    ) |>
        dplyr::mutate(
            "diff" = x - y
        ) |>
        # dplyr::mutate(
        #     "diff_rank" = rank(diff)
        # ) |>
        dplyr::arrange(diff)

    # full_roc <- pROC::roc(
    #     response = c(rep(0, nrow(complete_df)), rep(1, nrow(complete_df))),
    #     predictor = c(complete_df[["x"]], complete_df[["y"]]),
    #     levels = c(0, 1),
    #     direction = "<"
    # )
    # full_coord <- pROC::coords(full_roc, "all", ret = c("tp", "fp", "tpr", "fpr", "threshold"))
    # full_coord <- tibble

    unique_thresholds <- sort(unique(diff))
    selection_seq <- as.integer(seq.int(
        from = 1L,
        to = as.integer(length(unique_thresholds)),
        length.out = min(
            ifelse(is.na(n_tested_thresholds), Inf, n_tested_thresholds),
            length(unique_thresholds)
        )
    ))
    all_n_unique_thresholds <- unique_thresholds[selection_seq]
    auc_parts <- sapply(c("high", "low"), function(hl) {
        get_all_aucs_fun(
            true_pred_df = complete_df,
            thresholds = all_n_unique_thresholds,
            high_low = hl,
            do_parallel = do_parallel,
            check_positive_negative_count = check_positive_negative_count,
            return_proc = return_proc
        )
    }, USE.NAMES = TRUE, simplify = FALSE)

    joined_aucs <- dplyr::full_join(
        auc_parts[[1]][["perf"]],
        auc_parts[[2]][["perf"]],
        by = "threshold",
        suffix = c("_high", "_low")
    )
    joined_aucs <- dplyr::full_join(
        complete_df,
        joined_aucs,
        by = c("diff" = "threshold")
    )
    # joined_aucs <- tibble::as_tibble(dplyr::full_join(joined_aucs, full_coord, by = "threshold"))
    # colnames(joined_aucs)[(ncol(joined_aucs) - 1):ncol(joined_aucs)] <- paste0(
    #     colnames(joined_aucs)[(ncol(joined_aucs) - 1):ncol(joined_aucs)], "_global"
    # )

    lowest_pvalue_high <- which.min(joined_aucs[["p.value_high"]])
    lowest_pvalue_low <- which.min(joined_aucs[["p.value_low"]])
    library(ggplot2)
    pdf("removeme.pdf")
    print(ggplot(
        tidyr::pivot_longer(
            joined_aucs,
            cols = c("p.value_high", "p.value_low"),
            # cols = c("statistic_high", "statistic_low"),
            names_to = "part",
            values_to = "p.value"
            # values_to = "statistic"
        ),
        aes(x = diff, y = -log10(p.value), col = part)
        # aes(x = threshold, y = statistic, col = part)
    ) +
        geom_point())
    print(
        ggplot(
            joined_aucs,
            aes(x = diff)
        ) +
            geom_density(alpha = .5) +
            geom_vline(xintercept = joined_aucs[lowest_pvalue_low, ][["diff"]], col = "blue") +
            geom_vline(xintercept = joined_aucs[lowest_pvalue_high, ][["diff"]], col = "red")
    )
    # print(
    #     ggplot(
    #         tidyr::pivot_longer(complete_df,
    #             cols = c("x", "y"),
    #             names_to = "part",
    #             values_to = "val"
    #         ),
    #         aes(x = val, fill = part)
    #     ) +
    #         geom_density(alpha = .5)
    # )
    # print(ggplot(
    #     tidyr::pivot_longer(
    #         joined_aucs,
    #         cols = c("statistic_high", "statistic_low"),
    #         names_to = "part",
    #         values_to = "statistic"
    #     ),
    #     aes(x = threshold, y = statistic, col = part)
    # ) +
    #     geom_point())
    print(
        ggplot(
            tidyr::pivot_longer(joined_aucs,
                cols = c("x", "y"),
                names_to = "part",
                values_to = "val"
            ),
            aes(x = val, fill = part)
        ) +
            geom_density(alpha = .5)
    )
    # make a paired samples plot
    joined_aucs_keep_colors <- joined_aucs |>
        dplyr::mutate(
            "col" = ifelse(
                y - x > joined_aucs[lowest_pvalue_high, ][["diff"]],
                "keep_high",
                "removed"
            )
        ) |>
        dplyr::mutate(
            "col" = ifelse(
                y - x < joined_aucs[lowest_pvalue_low, ][["diff"]],
                "keep_low",
                col
            )
        )
    print(ggplot(joined_aucs_keep_colors, aes(x = x, y = y, col = col)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1))
    # print(Rvarious::plot_samplewise())
    dev.off()

    writexl::write_xlsx(
        joined_aucs,
        path = "joined_aucs.xlsx"
    )
    # if (!return_proc) {
    #     reslist <- list(
    #         "joined_aucs" = joined_aucs,
    #         "positive_label" = positive_label
    #     )
    # } else {
    #     reslist <- list(
    #         "joined_aucs" = joined_aucs,
    #         "positive_label" = positive_label,
    #         "pROC_lowpart" = aucs_lowpart[["pROC"]],
    #         "pROC_highpart" = aucs_highpart[["pROC"]],
    #         "pROC_full" = full_roc
    #     )
    # }

    # class(reslist) <- c("simple_rROC", class(reslist))
    # return(reslist)
}

partwise_results_wilcox_paired <- function(
    true_pred_df,
    thresholds,
    ...) {
    partwise_results(
        true_pred_df = true_pred_df,
        thresholds = thresholds,
        part_fun = function(part_df, ...) {
            tested <- wilcox.test(x = part_df[[1]], y = part_df[[2]], paired = TRUE, ...)
            return(tibble::tibble(
                "statistic" = tested$statistic,
                "p.value" = tested$p.value,
                "alternative" = tested$alternative,
                "method" = tested$method,
                "data.name" = tested$data.name
            ))
        },
        ...
    )
}
#' @param part_fun
#'  Function to apply to the part of the data frame.
#' Must return a list with the following elements:
#' - "auc": The AUC of the part.
#' - "cases": The number of cases in the part.
#' - "controls": The number of controls in the part.
#'
partwise_results <- function(
    true_pred_df,
    thresholds,
    part_fun,
    return_proc = FALSE,
    do_parallel = FALSE,
    high_low = "high",
    ...) {
    if (do_parallel) {
        stop("Applying parallel seems to be _slower_ for whatever reason, do not do this here.")
        if (!"future" %in% .packages()) {
            warning("Did you create a future 'plan()'? Running sequentially.")
        }
        lapply_fun <- function(x, FUN) {
            future.apply::future_lapply(
                x, FUN,
                future.globals = c("true_pred_df" = true_pred_df)
            )
        }
    } else {
        lapply_fun <- lapply
    }
    res <- lapply_fun(thresholds, function(threshold_x) {
        if (high_low == "high") {
            part_df <- true_pred_df[true_pred_df[["diff"]] >= threshold_x, , drop = FALSE]
        } else {
            part_df <- true_pred_df[true_pred_df[["diff"]] < threshold_x, , drop = FALSE]
        }
        tmp <- data.frame(
            class = c(rep("x", nrow(part_df)), rep("y", nrow(part_df))),
            val = c(part_df[[1]], part_df[[2]])
        )
        one_threshold_auc <- tryCatch(
            {
                part_results <- part_fun(part_df, ...)
                part_proc <- pROC::roc(class ~ val,
                    data = tmp,

                    # nolint start
                    # wilcox.test(part_df[[1]], part_df[[2]], "less")
                    # with alternative ="less" does:
                    #   "the null hypothesis is that the distribution of x is less than or equal to the distribution of y"
                    # TODO: Therefore, I THINK (NOT VALIDATED)
                    #  that for the ROC-curve, we use x as the negative class and y as the positive class
                    # where "x < y"
                    # nolint end
                    levels = c("x", "y"),
                    direction = "<"
                )
                inner_one_threshold <- tibble::add_column(part_results, "threshold" = threshold_x)
                list(
                    inner_one_threshold,
                    part_proc
                )
            },
            error = function(err) {
                return(list(NA, NA))
            }
        )
        return(one_threshold_auc)
    })
    full_df <- do.call(rbind, lapply(res, function(x) x[[1]]))
    return(list("perf" = full_df, "pROC" = lapply(res, function(x) x[[2]])))
}
