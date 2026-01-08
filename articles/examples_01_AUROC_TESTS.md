# AUROC, Mann-Whitney U and Wilcoxon rank-sum

``` r
options(warn = 1)
library(restrictedROC)
library(MASS) # for rnbinom
main_plotname <- "res/paper/auroc_wilcoxon"
dir.create("res/paper", recursive = TRUE)
```

From [Wackerly et al, 2014, Cengage
Learning](https://ggrlab.github.io/restrictedROC/articles/Wackerly,%20Dennis,%20William%20Mendenhall,%20and%20Richard%20L.%20Scheaffer.%20Mathematical%20statistics%20with%20applications.%20Cengage%20Learning,%202014.):
The Mann-Whitney U test is a nonparametric test obtained by:

1.  Ordering all (n_1 + n_2) observations by their value
2.  Count how many observations of sample I precede *each* observation
    of sample II
3.  Sum all counts from step 2

``` r
# Example from Wackerly 2014, p758, 15.6
values <- c(25, 26, 27, 28, 29, 31, 32, 35)
names(values) <- c("I", "I", "I", "II", "II", "I", "II", "II")
print(values)
#>  I  I  I II II  I II II 
#> 25 26 27 28 29 31 32 35

# Count how many observations in I and II
n_1 <- length(values[names(values) == "I"])
n_2 <- length(values[names(values) == "II"])
```

``` r
# 1. Ordering all (n_1 + n_2) observations by their value
values_sorted <- sort(values)
print(values_sorted)
#>  I  I  I II II  I II II 
#> 25 26 27 28 29 31 32 35

# 2. Count how many observations of sample I precede each observation of sample II
u_values <- c()
for (y_observation in values_sorted[names(values_sorted) == "II"]) {
    u_values <- c(u_values, sum(values_sorted[names(values_sorted) == "I"] < y_observation))
}

u_total <- sum(u_values)
print(u_total)
#> [1] 14
```

From [Wackerly et al, 2014, Cengage
Learning](https://ggrlab.github.io/restrictedROC/articles/Wackerly,%20Dennis,%20William%20Mendenhall,%20and%20Richard%20L.%20Scheaffer.%20Mathematical%20statistics%20with%20applications.%20Cengage%20Learning,%202014.),
15.6 we see that the U statistic has a direct connection to the Wilcoxon
rank sum statistic. For that, we need to calculate the ranks of the
values.

``` r
values_ranks <- rank(values)
print(values_ranks)
#>  I  I  I II II  I II II 
#>  1  2  3  4  5  6  7  8

# Sum of ranks of for sample I and II
rs_I <- sum(values_ranks[names(values_ranks) == "I"])
rs_II <- sum(values_ranks[names(values_ranks) == "II"])
ranksum <- min(rs_I, rs_II)
print(ranksum)
#> [1] 12
```

``` r
# R's wilcox.test:
#   STATISTIC <- c(W = sum(r[seq_along(x)]) - n.x * (n.x + 1)/2)
# translated:
#  STATISTIC <- c("W"=rs_I - n_1*(n_1 + 1)/2)
w_test <- wilcox.test(
    x = values[names(values) == "I"],
    y = values[names(values) == "II"],
    paired = FALSE,
    alternative = "two.sided"
)
print(w_test)
#> 
#>  Wilcoxon rank sum exact test
#> 
#> data:  values[names(values) == "I"] and values[names(values) == "II"]
#> W = 2, p-value = 0.1143
#> alternative hypothesis: true location shift is not equal to 0
print(c("W - manually calculated" = rs_I - n_1 * (n_1 + 1) / 2))
#> W - manually calculated 
#>                       2
```

With the formula from Wackerly2014:

$$U = n_{1}n_{2} + \frac{n_{1}\left( n_{1} + 1 \right)}{2} - ranksum_{I}$$

we see that
$$ranksum_{I} = n_{1}n_{2} + \frac{n_{1}\left( n_{1} + 1 \right)}{2} - U$$

and therefore

$$W_{Rwilcox} = ranksum_{I} - \frac{n_{1}\left( n_{1} + 1 \right)}{2} = n_{1}n_{2} - U$$

``` r
ranksum_I_from_u <- n_1 * n_2 + n_1 * (n_1 + 1) / 2 - u_total
w_r <- ranksum_I_from_u - n_1 * (n_1 + 1) / 2
w_r_alternative <- n_1 * n_2 - u_total

print(w_r_alternative)
#> [1] 2
print(w_r)
#> [1] 2
print(w_test)
#> 
#>  Wilcoxon rank sum exact test
#> 
#> data:  values[names(values) == "I"] and values[names(values) == "II"]
#> W = 2, p-value = 0.1143
#> alternative hypothesis: true location shift is not equal to 0

u_from_wilcox <- n_1 * n_2 - w_test$statistic
print(u_from_wilcox)
#>  W 
#> 14
print(u_total)
#> [1] 14
```

### AUROC is the equivalent to Mann-Whitney U statistic

``` r
# Example from Wackerly 2014, p758, 15.6
values <- c(25, 26, 27, 28, 29, 31, 32, 35)
names(values) <- c("I", "I", "I", "II", "II", "I", "II", "II")
print(values)
#>  I  I  I II II  I II II 
#> 25 26 27 28 29 31 32 35

# Count how many observations in I and II
n_1 <- length(values[names(values) == "I"])
n_2 <- length(values[names(values) == "II"])

auroc_from_u <- u_total / (n_1 * n_2)
print(auroc_from_u)
#> [1] 0.875

# Explicitely calculate AUC with pROC
print(pROC::roc(
    response = names(values),
    predictor = values,
    levels = c("I", "II"),
    direction = "<",
    auc = TRUE
))
#> 
#> Call:
#> roc.default(response = names(values), predictor = values, levels = c("I",     "II"), direction = "<", auc = TRUE)
#> 
#> Data: values in 4 controls (names(values) I) < 4 cases (names(values) II).
#> Area under the curve: 0.875
```

### Bigger example of AUROC vs Mann-Whitney U statistic

``` r
set.seed(2)
values <- c(
    a = rnorm(50, mean = 8, sd = 1),
    b = rnorm(50, mean = 9, sd = 2)
)
names(values) <- gsub("[0-9]*", "", names(values))
n_1 <- length(values[names(values) == "a"])
n_2 <- length(values[names(values) == "b"])

big_roc <- pROC::roc(
    response = names(values),
    predictor = values,
    levels = c("a", "b"),
    direction = "<",
    auc = TRUE
)
big_wilcox <- wilcox.test(
    x = values[names(values) == "a"],
    y = values[names(values) == "b"],
    paired = FALSE,
    alternative = "two.sided"
)
big_ttest <- t.test(
    x = values[names(values) == "a"],
    y = values[names(values) == "b"],
    paired = FALSE,
    alternative = "two.sided"
)

u_from_wilcox <- n_1 * n_2 - big_wilcox$statistic
auc_from_u <- u_from_wilcox / (n_1 * n_2)
print(auc_from_u)
#>      W 
#> 0.5592
print(big_roc)
#> 
#> Call:
#> roc.default(response = names(values), predictor = values, levels = c("a",     "b"), direction = "<", auc = TRUE)
#> 
#> Data: values in 50 controls (names(values) a) < 50 cases (names(values) b).
#> Area under the curve: 0.5592

# Plot ROC curve
library(ggplot2)
df_split <- as.data.frame(split(values, names(values)))

plot_box <- ggplot(
    tidyr::pivot_longer(df_split, dplyr::everything()),
    aes(x = name, y = value, fill = name)
) +
    geom_boxplot() +
    geom_jitter(size = 1, width = .1) +
    scale_fill_manual(values = c("a" = "#ee000070", "b" = "#008B4570")) +
    ggpubr::theme_pubr()
plots_rroc <- restrictedROC::plot_density_ROC_empirical(
    values_grouped = df_split,
    xmin = min(values), xmax = max(values),
    levels = c("a", "b"),
    direction = "<"
)
scaling <- .35
pdf(paste0(main_plotname, "boxplot_density_roc.pdf"), width = 30 * scaling, height = 10 * scaling)
set.seed(51) # for geom_jitter reproducibility
patchwork::wrap_plots(
    plot_box,
    plots_rroc[[1]] +
        scale_fill_manual(values = c("a" = "#ee000070", "b" = "#008B4570")),
    plots_rroc[[2]]
)
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.
#> Ignoring unknown labels:
#> • colour : ""
dev.off()
#> agg_png 
#>       2
```

``` r
rroc_perm <- restrictedROC::simple_rROC_permutation(
    response = names(values),
    predictor = values,
    n_permutations = 1000,
    positive_label = "b",
    direction = "<",
    return_proc = TRUE
)
```

``` r
# nolint start
print(big_roc)
#> 
#> Call:
#> roc.default(response = names(values), predictor = values, levels = c("a",     "b"), direction = "<", auc = TRUE)
#> 
#> Data: values in 50 controls (names(values) a) < 50 cases (names(values) b).
#> Area under the curve: 0.5592
# Call:
# roc.default(response = names(values), predictor = values, levels = c("a",     "b"), direction = "<", auc = TRUE)

# Data: values in 50 controls (names(values) a) < 50 cases (names(values) b).
# Area under the curve: 0.5592

print(big_ttest)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  values[names(values) == "a"] and values[names(values) == "b"]
#> t = -1.7942, df = 69.92, p-value = 0.07711
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -1.41436615  0.07477889
#> sample estimates:
#> mean of x mean of y 
#>  8.069138  8.738932
#         Welch Two Sample t-test
# data:  values[names(values) == "a"] and values[names(values) == "b"]
# t = -1.7942, df = 69.92, p-value = 0.07711
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.41436615  0.07477889
# sample estimates:
# mean of x mean of y
#  8.069138  8.738932

print(big_wilcox)
#> 
#>  Wilcoxon rank sum test with continuity correction
#> 
#> data:  values[names(values) == "a"] and values[names(values) == "b"]
#> W = 1102, p-value = 0.3092
#> alternative hypothesis: true location shift is not equal to 0
# data:  values[names(values) == "a"] and values[names(values) == "b"]
# W = 1102, p-value = 0.3092
# alternative hypothesis: true location shift is not equal to 0

print(rroc_perm$permutation_pval)
#>    pval.twoside.max pval.twoside.global      n_permutations 
#>        6.993007e-03        3.226773e-01        1.000000e+03
#    pval.twoside.max pval.twoside.global      n_permutations
#        6.993007e-03        3.226773e-01        1.000000e+03
print(rroc_perm$max_total)
#>         auc auc_var_H0    rzAUC    pval_asym threshold part
#> 1 0.7549342 0.00486568 3.654739 0.0002574438  7.381726 high
#         auc auc_var_H0    rzAUC    pval_asym threshold part
# 1 0.7549342 0.00486568 3.654739 0.0002574438  7.381726 high

cat("Informative range established from restriction: (", round(rroc_perm$max_total[["threshold"]], 2), ", Inf)\n", sep = "")
#> Informative range established from restriction: (7.38, Inf)
# Informative range established from restriction: (7.38, Inf)
# nolint end

rroc_part_plots <- restrictedROC::plot_rROC_part(
    rroc_perm,
    threshold = rroc_perm$max_total[["threshold"]]
)
informative_range <- c(rroc_perm$max_total[["threshold"]])
pdf(paste0(main_plotname, "boxplot_density_roc_restriction.pdf"), width = 30 * scaling, height = 10 * scaling)
set.seed(51) # for geom_jitter reproducibility
patchwork::wrap_plots(
    plot_box + geom_hline(yintercept = informative_range, col = "red", linewidth = 1),
    plots_rroc[[1]] +
        geom_vline(xintercept = informative_range, col = "red", linewidth = 1) +
        scale_fill_manual(values = c("a" = "#ee000070", "b" = "#008B4570")),
    plots_rroc[[2]]
)
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.
#> Ignoring unknown labels:
#> • colour : ""
dev.off()
#> agg_png 
#>       2
pdf(paste0(main_plotname, "boxplot_density_roc_restriction_parts.pdf"), height = 4, width = 4)
print(rroc_part_plots)
#> $plotlist
#> $plotlist$A
#> Ignoring unknown labels:
#> • colour : ""
#> 
#> $plotlist$B
#> 
#> $plotlist$C
#> Ignoring unknown labels:
#> • colour : ""
#> 
#> $plotlist$D
#> 
#> $plotlist$E
#> Ignoring unknown labels:
#> • colour : ""
#> 
#> $plotlist$F
#> 
#> 
#> $patchworked
#> Ignoring unknown labels:
#> • colour : ""
#> Ignoring unknown labels:
#> • colour : ""
#> Ignoring unknown labels:
#> • colour : ""
dev.off()
#> agg_png 
#>       2
```

R’s two-sample `wilcoxon.test` is (by default) calculated “exact” if
there are less than 50 observations in both groups. `n.x` is the number
of samples in the first group, `n.y` the number of samples in the second
group.

See
<https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/wilcox.test.R>
for the code. Briefly going through that code:

- Line 245 starts the two sample case
- Line 252: If `exact == NULL` (default), then
  `exact <- (n.x < 50) && (n.y < 50)`
- With ties: z-standardization approximization
- Without ties: Exact calculation

``` r
r <- rank(c(x - mu, y))
STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)

NTIES <- table(r)
z <- STATISTIC - n.x * n.y / 2
SIGMA <- sqrt((n.x * n.y / 12) * ((n.x + n.y + 1) - sum(NTIES^3 - NTIES) / ((n.x + n.y) * (n.x + n.y - 1))))
z <- (z - CORRECTION) / SIGMA

if (correct) {
    CORRECTION <- switch(alternative,
        "two.sided" = sign(z) * 0.5,
        "greater" = 0.5,
        "less" = -0.5
    )
    METHOD <- paste(METHOD, "with continuity correction")
}

PVAL <- switch(alternative,
    "less" = pnorm(z),
    "greater" = pnorm(z, lower.tail = FALSE),
    "two.sided" = 2 * min(
        pnorm(z),
        pnorm(z, lower.tail = FALSE)
    )
)
```

``` r
r <- rank(c(x - mu, y))
STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)

PVAL <- switch(alternative,
    "two.sided" = {
        p <- if (STATISTIC > (n.x * n.y / 2)) {
            pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
        } else {
            pwilcox(STATISTIC, n.x, n.y)
        }
        min(2 * p, 1)
    },
    "greater" = {
        pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
    },
    "less" = pwilcox(STATISTIC, n.x, n.y)
)
```

``` r
set.seed(4)
values <- c(
    a = rnorm(55, mean = 8, sd = 1),
    b = rnorm(55, mean = 8, sd = 1)
    # b = rnorm(75, mean = 8, sd = 2)
)
names(values) <- gsub("[0-9]*", "", names(values))
rroc_perm_2 <- restrictedROC::rROC(
    y = names(values),
    x = values,
    n_permutations = 0,
    positive_label = "b",
    direction = "<",
    return_proc = TRUE
)
#> Thu Jan  8 12:36:04 2026      y x ( 1 )
rroc_perfs <- rroc_perm_2[[1]][[1]][["permutation"]][["performances"]]

data_df <- tibble::tibble(
    response = names(values),
    predictor = values
)
wilcox_res <- lapply(rroc_perfs[["threshold"]], function(t_x) {
    samples_above_threshold <- data_df |> dplyr::filter(predictor > t_x)
    samples_a <- samples_above_threshold |> dplyr::filter(response == "a")
    samples_b <- samples_above_threshold |> dplyr::filter(response == "b")
    if (nrow(samples_b) < 1 || nrow(samples_a) < 1) {
        return(NULL)
    }
    tmp <- wilcox.test(
        x = samples_a |> dplyr::pull(predictor),
        y = samples_b |> dplyr::pull(predictor),
        alternative = "two.sided"
    )
    return(tmp)
})
w_statistic <- unlist(lapply(wilcox_res, function(x) x$statistic))
w_pvalue <- unlist(lapply(wilcox_res, function(x) x$p.value))

rroc_perfs[["w.statistic"]] <- NA
rroc_perfs[["w.pvalue"]] <- NA
rroc_perfs[seq_along(w_statistic), "w.statistic"] <- w_statistic
rroc_perfs[seq_along(w_pvalue), "w.pvalue"] <- w_pvalue

rroc_perfs[["u_from_wilcox"]] <- with(rroc_perfs, positives_high * negatives_high - w.statistic)
rroc_perfs[["auroc_from_u"]] <- with(rroc_perfs, u_from_wilcox / (positives_high * negatives_high))

rroc_perfs[, c(1:8, 20:25)]
#> # A tibble: 111 × 14
#>    threshold auc_high positives_high negatives_high scaling_high auc_var_H0_high
#>        <dbl>    <dbl>          <dbl>          <dbl>        <dbl>           <dbl>
#>  1   -Inf       0.391             55             55         1            0.00306
#>  2      6.24    0.379             55             54         1.02         0.00309
#>  3      6.27    0.386             54             54         1.04         0.00311
#>  4      6.29    0.394             53             54         1.06         0.00314
#>  5      6.41    0.382             53             53         1.08         0.00317
#>  6      6.52    0.370             53             52         1.10         0.00321
#>  7      6.61    0.378             52             52         1.12         0.00324
#>  8      6.69    0.385             51             52         1.14         0.00327
#>  9      6.71    0.393             50             52         1.16         0.00330
#> 10      6.72    0.381             50             51         1.19         0.00333
#> # ℹ 101 more rows
#> # ℹ 8 more variables: rzAUC_high <dbl>, pval_asym_onesided_high <dbl>,
#> #   tpr_global <dbl>, fpr_global <dbl>, w.statistic <dbl>, w.pvalue <dbl>,
#> #   u_from_wilcox <dbl>, auroc_from_u <dbl>

rroc_perfs[["fpr_global"]]
#>   [1] 1.00000000 0.98181818 0.98181818 0.98181818 0.96363636 0.94545455
#>   [7] 0.94545455 0.94545455 0.94545455 0.92727273 0.92727273 0.92727273
#>  [13] 0.92727273 0.92727273 0.92727273 0.92727273 0.90909091 0.89090909
#>  [19] 0.89090909 0.87272727 0.87272727 0.87272727 0.85454545 0.85454545
#>  [25] 0.83636364 0.81818182 0.80000000 0.80000000 0.80000000 0.80000000
#>  [31] 0.80000000 0.78181818 0.78181818 0.76363636 0.74545455 0.74545455
#>  [37] 0.72727273 0.72727273 0.72727273 0.70909091 0.69090909 0.69090909
#>  [43] 0.69090909 0.67272727 0.67272727 0.65454545 0.65454545 0.65454545
#>  [49] 0.65454545 0.65454545 0.65454545 0.63636364 0.61818182 0.61818182
#>  [55] 0.61818182 0.60000000 0.58181818 0.56363636 0.54545455 0.54545455
#>  [61] 0.52727273 0.52727273 0.50909091 0.50909091 0.50909091 0.49090909
#>  [67] 0.47272727 0.47272727 0.45454545 0.43636364 0.43636364 0.43636364
#>  [73] 0.41818182 0.40000000 0.40000000 0.38181818 0.36363636 0.34545455
#>  [79] 0.34545455 0.34545455 0.34545455 0.32727273 0.32727273 0.32727273
#>  [85] 0.30909091 0.30909091 0.29090909 0.29090909 0.27272727 0.25454545
#>  [91] 0.23636364 0.23636364 0.21818182 0.21818182 0.20000000 0.18181818
#>  [97] 0.16363636 0.16363636 0.14545455 0.12727273 0.10909091 0.09090909
#> [103] 0.09090909 0.07272727 0.07272727 0.05454545 0.03636364 0.01818182
#> [109] 0.00000000 0.00000000 0.00000000
rroc_perfs[["w.pvalue"]] - rroc_perfs[["pval_asym_high"]]
#>   [1] 0.0003396897 0.0002304702 0.0003108431 0.0004146124 0.0002857465
#>   [6] 0.0001872807 0.0002569748 0.0003489478 0.0004683155 0.0003225519
#>  [11] 0.0004366585 0.0005835785 0.0007687320 0.0009963163 0.0012679749
#>  [16] 0.0015811224 0.0012833854 0.0014884505 0.0021854832 0.0014326423
#>  [21] 0.0021450978 0.0029315980 0.0021343097 0.0029511254 0.0021202277
#>  [26] 0.0012993356 0.0005918934 0.0011862297 0.0019520296 0.0028552880
#>  [31] 0.0038150164 0.0029027484 0.0039089297 0.0029524494 0.0019362525
#>  [36] 0.0029474325 0.0018911422 0.0029366558 0.0041033968 0.0029853726
#>  [41] 0.0018409164 0.0029654999 0.0042560044 0.0030141898 0.0043742524
#>  [46] 0.0030657910 0.0045011260 0.0059579880 0.0071326302 0.0076656332
#>  [51] 0.0072757507 0.0079660796 0.0080909831 0.0082478910 0.0072922626
#>  [56] 0.0084199745 0.0090313282 0.0089158073 0.0079886342 0.0095051961
#>  [61] 0.0086361252 0.0101483188 0.0093723530 0.0108431742 0.0108327744
#>  [66] 0.0115895438 0.0112752483 0.0123214773 0.0123129744 0.0109797200
#>  [71] 0.0133505485 0.0137890254 0.0145621721 0.0137669517 0.0157755667
#>  [76] 0.0153711960 0.0130347841 0.0091403003 0.0141600292 0.0186343847
#>  [81] 0.0200944018 0.0211010021 0.0212084593 0.0234064201 0.0214991123
#>  [86] 0.0274921390 0.0279998626 0.0276812676 0.0318385358 0.0343985838
#>  [91] 0.0343350001 0.0371438028 0.0423128966 0.0284212883 0.0377763522
#>  [96] 0.0482931603 0.0578873709 0.0336865760 0.0483456757 0.0674169901
#> [101] 0.0870925246 0.0982659643 0.0961396165 0.1490713064 0.0692558269
#> [106] 0.1167354833 0.2119980830 0.4459953047           NA           NA
#> [111]           NA
pdf(paste0(main_plotname, "rzAUC_vs_wilcox.pdf"), width = 6, height = 4)
print(
    ggplot(rroc_perfs, aes(x = auc_high, y = round(auroc_from_u - auc_high, 8))) +
        geom_point() +
        ggtitle(
            "Restricted rAUC == (rAUC from wilcoxon test)?",
            subtitle = "Sanity check if restricted AUC is equivalent to calculated AUC \nfrom multiple wilcoxon test on the corresponding sample subsets"
        ) +
        ggpubr::theme_pubr() +
        theme(
            plot.title = element_text(size = 6),
            plot.subtitle = element_text(size = 6)
        )
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs |>
            dplyr::select(fpr_global, pval_asym_high, w.pvalue) |>
            tidyr::pivot_longer(-fpr_global, names_to = "name", values_to = "p.value"),
        aes(x = fpr_global, y = (p.value), col = name)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs |>
            dplyr::select(fpr_global, pval_asym_high, w.pvalue) |>
            tidyr::pivot_longer(-fpr_global, names_to = "name", values_to = "p.value"),
        aes(x = fpr_global, y = (p.value), col = name)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs |>
            dplyr::select(fpr_global, pval_asym_high, w.pvalue) |>
            tidyr::pivot_longer(-fpr_global, names_to = "name", values_to = "p.value"),
        aes(x = fpr_global, y = (p.value), col = name)
    ) +
        geom_point() +
        geom_point(data = rroc_perfs[, c("fpr_global", "rzAUC_high")], aes(y = -abs(rzAUC_high), col = "-abs(rzAUC_high)")) +
        ggpubr::theme_pubr()
)
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).

# print(
#     ggplot(
#         rroc_perfs |>
#             dplyr::select(fpr_global, pval_asym_high, w.pvalue) |>
#             tidyr::pivot_longer(-fpr_global, names_to = "name", values_to = "p.value"),
#         aes(x = fpr_global, y = (p.value), col = name)
#     ) +
#         geom_line() +
#         geom_line(data = rroc_perfs[, c("fpr_global", "rzAUC_high")], aes(y = -abs(rzAUC_high), col = "-abs(rzAUC_high)")) +
#         ggpubr::theme_pubr()
# )

print(
    ggplot(
        rroc_perfs,
        aes(x = abs(rzAUC_high), y = pval_asym_high)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs,
        aes(x = abs(rzAUC_high), y = w.pvalue)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs,
        aes(x = fpr_global, y = w.pvalue)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs,
        aes(x = fpr_global, y = pval_asym_high)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs,
        aes(x = fpr_global, y = rzAUC_high)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
print(
    ggplot(
        rroc_perfs,
        aes(x = pval_asym_high, y = w.pvalue)
    ) +
        geom_point() +
        ggpubr::theme_pubr()
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).

dup_labels <- function(x) {
    tmp <- apply(rroc_perfs[, c("positives_high", "negatives_high")], 1, sum, na.rm = TRUE)
    tmp[sapply(x, function(y) {
        which.min(abs(rroc_perfs[["fpr_global"]] - y))
    })]
}
print(
    ggplot(
        rroc_perfs,
        aes(x = fpr_global, y = pval_asym_high - w.pvalue)
    ) +
        geom_point() +
        ggpubr::theme_pubr() +
        scale_x_continuous(sec.axis = dup_axis(labels = dup_labels, name = "Total number of samples"))
)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_point()`).
dev.off()
#> agg_png 
#>       2
```
