#' restricted ROC
#'
#' Calculate the restricted ROC curves.
#'
#' @inheritParams simple_rROC
#' @param n_permutations
#' 	How many permutations should be done
#' @param fix_seed
#' boolean: If not FALSE, the seed for each permutation will be set by set.seed(fix_seed + permutation_i)
#' @param parallel_permutations
#' boolean: If TRUE, the permutation will be done via `future.apply::future_lapply`,
#' otherwise by `base::lapply`
#' @param verbose
#' 	Print in which permutation we are
#' @return
#'  List of:
#'  	- Results of [simple_rROC_interpret()]:
#'  		performances, global, keep_highs, keep_lows, max_total, positive_label
#'  	"pROC_full": Result of [pROC::roc()] on the full dataset, calculated once
#' 		"permutation_pval":
#' 			Permutation p-values globally and with optimized ("max") restriction
#'
#' 				pval.twoside.max pval.twoside.global      n_permutations
#' 							0.16                0.01              100.00
#' 		"perm_max_bound":
#' 			Bound table of the optimal restriction results for all permutations
#'
#'              # A tibble: 100 × 6
#'              auc auc_var_H0 rzAUC pval_asym threshold part
#'              <dbl>      <dbl> <dbl>     <dbl>     <dbl> <chr>
#'              	1 0.161     0.0134  -2.92   0.00345     17.4  high
#'              2 0.631     0.00635  1.64   0.101       11.7  high
#'              3 0.332     0.00826 -1.85   0.0644      13.2  high
#'
#' 		"perm_global_bound":
#' 			Bound table of the global AUC (= use all samples) for all permutations
#'
#'              # A tibble: 100 × 4
#'                   auc auc_var_H0   rzAUC pval_asym
#'                 <dbl>      <dbl>   <dbl>     <dbl>
#'               1 0.453      0.453 -0.827      0.408
#'               2 0.504      0.504  0.0627     0.950
#'
#'
#' @export
#'
#' @examples
#'
#' data(aSAH, package = "pROC")
#' a <- simple_rROC_permutation(
#'     response = aSAH$outcome,
#'     predictor = aSAH$ndka
#' )
simple_rROC_permutation <- function(response,
                                    predictor,
                                    positive_label = NULL,
                                    direction = "<",
                                    n_permutations = 100,
                                    fix_seed = 0,
                                    parallel_permutations = FALSE,
                                    return_proc = FALSE,
                                    verbose = FALSE,
                                    ...) {
    if (length(response) != length(predictor)) {
        stop("response and predictor must have the same length")
    }
    res_full <- simple_rROC(
        response = response,
        predictor = predictor,
        positive_label = positive_label,
        direction = direction,
        return_proc = return_proc,
        ...
    )
    res <- simple_rROC_interpret(res_full)
    if (return_proc) {
        res[["pROC_full"]] <- res_full[["pROC_full"]]
    }

    if (n_permutations > 0) {
        # permutation values
        if (parallel_permutations) {
            lapply_fun <- function(x, FUN) {
                future.apply::future_lapply(x, FUN, future.seed = TRUE)
            }
        } else {
            lapply_fun <- lapply
        }
        permutation_res <- lapply_fun(seq_len(n_permutations), function(permutation_i) {
            if (verbose) {
                print(permutation_i)
            }
            if (!isFALSE(fix_seed)) {
                set.seed(fix_seed + permutation_i)
            }
            sampled_response <- sample(response)
            res_permutation <- simple_rROC(
                response = sample(sampled_response),
                predictor = predictor,
                positive_label = positive_label,
                direction = direction
            )
            res_permutation_interpreted <- simple_rROC_interpret(res_permutation)
            return(c(res_permutation_interpreted["max_total"], res_permutation_interpreted["global"]))
        })
        perm_max_bound <- do.call(rbind, lapply(permutation_res, function(x) x[["max_total"]]))
        perm_global_bound <- do.call(rbind, lapply(permutation_res, function(x) x[["global"]]))

        # Proportion of values above the actually observed MAXIMUM value from all permutations
        # nolint start
        # n_perm_above_observed <- sum(permutation_res_bound[["rzAUC"]] > res[["max_total"]][["rzAUC"]])
        # n_perm_below_observed <- sum(permutation_res_bound[["rzAUC"]] < res[["max_total"]][["rzAUC"]])
        # nolint end
        n_perm_twosided_max <- sum(abs(perm_max_bound[["rzAUC"]]) >= abs(res[["max_total"]][["rzAUC"]]))
        n_perm_twosided_global <- sum(abs(perm_global_bound[["rzAUC"]]) >= abs(res[["global"]][["rzAUC"]]))

        groups_table <- table(response)

        # nolint start
        ## statmod::permp calculates the exact permutation p-value.
        ## An easy approximation is
        ##  (n_more_extreme + 1) / (n_permutations + 1.)
        ## But is too conservative according to Phipson&Smyth 2010,
        ## therefore they introduce a slightly modified calculation in statmod::permp
        # nolint end
        perm_fun <- function(n_more_extreme) {
            statmod::permp(
                # number of permutations that yielded test statistics at least as extreme
                # as the observed data. May be vector/array.
                x = n_more_extreme,
                # total number of permutations performed
                nperm = n_permutations,
                # sample size of group 1,
                # the sum() is if there are more than 2 negative groups
                # that MIGHT be a valid thing if we want to compare ONE group to ALL OTHERS
                n1 = sum(groups_table[names(groups_table) != res_full[["positive_label"]]]),
                # sample size of group 2,
                n2 = groups_table[res_full[["positive_label"]]],
                twosided = TRUE
            )
        }
        res[["permutation_pval"]] <- c(
            "pval.twoside.max" = perm_fun(n_perm_twosided_max),
            "pval.twoside.global" = perm_fun(n_perm_twosided_global),
            "n_permutations" = n_permutations
        )
        res[["perm_max_bound"]] <- tibble::as_tibble(perm_max_bound)
        res[["perm_global_bound"]] <- tibble::as_tibble(perm_global_bound)
    }
    return(res)
}
