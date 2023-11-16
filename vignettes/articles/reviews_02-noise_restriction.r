---
title: "Restricting pure noise"
---

```{r, include = FALSE}
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)
```

```{r setup}
options(warn = 1)
library(restrictedROC)
library(ggplot2)

```


```{r, fig.fullwidth=TRUE, fig.width=5, fig.height=5}
main_plotname <- "res/paper/noise_restriction"
dir.create("res/paper", recursive = TRUE)

library(tibble)
df_flow <- dataMelanoma::glehr2023_cd4_cd8_relative
df_flow_hepatitis <- df_flow[!is.na(df_flow[["Hepatitis"]]), ]
df_flow_hepatitis_train <- df_flow_hepatitis[df_flow_hepatitis[["validation"]] == 0, ]
df_flow_oneFeature <- df_flow_hepatitis_train[, c("Hepatitis", "/AllCells/CD4+/CD8-/Tem")]
dependent_vars <- c("Hepatitis")

# Generate 10k random label permutations
set.seed(120938)
for (n_random_labels in seq_len(10000)) {
    dependent_vars <- c(dependent_vars, paste0("hep_random_", n_random_labels))
    df_flow_oneFeature[[dependent_vars[length(dependent_vars)]]] <- sample(df_flow_oneFeature[["Hepatitis"]], replace = FALSE)
}


# Do restriction
rroc_result <- restrictedROC::rROC(
    df_flow_oneFeature,
    dependent_vars = dependent_vars,
    independent_vars = "/AllCells/CD4+/CD8-/Tem",
    n_permutations = 100,
    save_path = "rroc_result_random_110samples.qs",
)
rroc_result <- qs::qread("rroc_result_random_110samples.qs")
library(restrictedROC)
s_rroc <- summary(rroc_result)

# pdf(paste0(main_plotname, ".pdf"), width = 4, height = 4)
print(
    ggplot(s_rroc[-1, ], aes(x = pval.twoside.global, y = pval.twoside.max)) +
        geom_point(size = .2) +
        ggpubr::theme_pubr()
)
print(
    ggplot(s_rroc[-1, ], aes(x = pval.twoside.global)) +
        geom_histogram(boundary = 0, binwidth = 0.1) +
        ggpubr::theme_pubr()
)
print(
    ggplot(s_rroc[-1, ], aes(x = pval.twoside.max)) +
        geom_histogram(boundary = 0, binwidth = 0.1) +
        ggpubr::theme_pubr()
)
# dev.off()

sum(s_rroc$pval.twoside.global < 0.1)
sum(s_rroc$pval.twoside.max < 0.1)

if (any(p.adjust(s_rroc$pval.twoside.max) < .1)) {
    stop("There are some significant restricted results")
}

if (any(p.adjust(s_rroc$pval.twoside.global) < .1)) {
    stop("There are some significant global results")
}
```
