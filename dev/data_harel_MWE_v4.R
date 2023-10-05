library(dplyr)
dir.create("intermediate_data")
## 1. Download relevant data (S1, S2, S4)
for (supplement_i in c(1, 2, 4)) {
    download.file(
        url = paste0("https://ars.els-cdn.com/content/image/1-s2.0-S0092867419309006-mmc", supplement_i, ".xlsx"),
        destfile = paste0("intermediate_data/harel2019-s", supplement_i, ".xlsx"),
        method = "wget"
    )
}

## 2. Read in data
### 2.1. Patient metadata (pheno)
harel_pheno <- readxl::read_excel("intermediate_data/harel2019-s1.xlsx", sheet = "S1A", skip = 1, na = "NaN")
harel_pheno <- harel_pheno |>
    # Add a column with the response as defined in the paper
    dplyr::mutate(response_paper = ifelse(Response == "PD", "non-responder", "responder"))

# Metadata contains spaces in the sample names
# the actual data always "_" instead of spaces
harel_pheno[["Sample ID"]] <- gsub(" ", "_", harel_pheno[["Sample ID"]])
outcome_full <- harel_pheno[["response_paper"]]
names(outcome_full) <- harel_pheno[["Sample ID"]]

# Patients were categorized into responders (complete or partial regression) and
# non-responders (progressive disease) according to RECIST v1.0 guidelines
# "Response" contains "PD", "PR", "CR" or "SD" (stable disease)
harel_pheno <- harel_pheno |>
    # Filter out stable disease
    dplyr::filter(Response %in% c("PD", "PR", "CR"))
outcome_noSD <- harel_pheno[["response_paper"]]
names(outcome_noSD) <- harel_pheno[["Sample ID"]]

### 2.2. aPD1 from tables S1B, S1D, S2B and S4A
data_aPD1_all <- list(
    s1b = readxl::read_excel("intermediate_data/harel2019-s1.xlsx", sheet = "S1B", skip = 1, na = "NaN"),
    s1d = readxl::read_excel("intermediate_data/harel2019-s1.xlsx", sheet = "S1D", skip = 1, na = "NaN"),
    s2b = readxl::read_excel("intermediate_data/harel2019-s2.xlsx", sheet = "S2B", skip = 1, na = "NaN"),
    s4a = readxl::read_excel("intermediate_data/harel2019-s4.xlsx", sheet = "S4A", skip = 1, na = "NaN")
)
data_aPD1_all[["s1b"]] <- data_aPD1_all[["s1b"]][, !grepl("^TIL_", colnames(data_aPD1_all[["s1b"]]))]
data_aPD1_all[["s1b_noSD"]] <- data_aPD1_all[["s1b"]][, c(
    colnames(data_aPD1_all[["s1b"]]) %in% names(outcome_noSD) |
        grepl("^T: ", colnames(data_aPD1_all[["s1b"]]))
)]
lapply(data_aPD1_all, colnames)
lapply(data_aPD1_all, dim)
lapply(data_aPD1_all, function(x) sum(grepl("PD1_", colnames(x))))


## 3. Calculate the mean differences between responder/non-responder
harel_means <- lapply(data_aPD1_all, function(datapart_x) {
    colnames_not_samples <- grep("^(PD1_)|(TIL_)", colnames(datapart_x), value = TRUE, invert = TRUE)
    datapart_x_samples <- datapart_x[, grepl("^(PD1_)|(TIL_)", colnames(datapart_x))]
    res_t <- apply(datapart_x_samples, 1, function(feature_x) {
        # names(x) are the column names of the matrix
        # The response needs to be ordered according to the column names
        if (!all(names(feature_x) %in% names(outcome_full))) {
            stop("Not all samples found")
        }
        x_NonResponder <- split(feature_x, outcome_full[names(feature_x)])
        x_means <- lapply(x_NonResponder, mean, na.rm = TRUE)
        return(c("diff_r_nr" = x_means[["responder"]] - x_means[["non-responder"]]))
    })
    tmp <- tibble::as_tibble(as.matrix(res_t), rownames = "gene")
    colnames(tmp) <- c("gene", "diff_r_nr")
    tmp <- tibble::add_column(datapart_x[colnames_not_samples], tmp)
    if (all(tmp[["gene"]] == as.character(1:nrow(tmp)))) {
        tmp[["gene"]] <- NULL
    }
    return(tmp)
})

lapply(harel_means, colnames)
lapply(harel_means, function(x) {
    ensg_column <- grep("ENSG", colnames(x), value = TRUE)
    selected <- x[grepl("ENSG00000149948", x[[ensg_column]], fixed = TRUE), ]
    return(selected)
})
library(ggplot2)
pdf("compare_mean_differences.pdf")
print(
    ggplot(harel_means[["s4a"]], aes(x = `Student's T-test Difference R-NR`, y = diff_r_nr, label = `Gene name`)) +
        geom_abline(slope = 1, intercept = 0, col = "red") +
        geom_point() +
        ggtitle("TIL mean difference of Responder - NonResponder", subtitle = "Data from Table S2A, R/NR-status from Table S1A") +
        ylab("Recalculated mean difference Responder - NonResponders") +
        geom_text(size = 1, color = "darkgreen")
)
print(
    ggplot(harel_means[["s2b"]], aes(x = `Student's T-test Difference R_NR`, y = diff_r_nr, label = `Gene name`)) +
        geom_abline(slope = 1, intercept = 0, col = "red") +
        geom_point() +
        ggtitle("TIL mean difference of Responder - NonResponder", subtitle = "Data from Table S2A, R/NR-status from Table S1A") +
        ylab("Recalculated mean difference Responder - NonResponders") +
        geom_text(size = 1, color = "darkgreen")
)
dev.off()



library(ggplot2)
if (!all(harel_means$til$gene == harel_til_s2_ttest[["Gene names"]])) {
    stop("Gene names do not match")
}
comparison_df_til <- tibble::as_tibble(cbind(harel_til_s2_ttest, harel_means$til))


if (!all(harel_means$aPD1$gene == harel_aPD1_s2_ttest[["Gene name"]])) {
    stop("Gene names do not match")
}
comparison_df_aPD1 <- tibble::as_tibble(cbind(harel_aPD1_s2_ttest, harel_means$aPD1))
library(ggplot2)
pdf("compare_mean_differences.pdf")
print(
    ggplot(comparison_df_til, aes(x = `Student's T-test Difference R_NR`, y = diff_r_nr)) +
        geom_abline(slope = 1, intercept = 0, col = "red") +
        geom_point() +
        ggtitle("TIL mean difference of Responder - NonResponder", subtitle = "Data from Table S2A, R/NR-status from Table S1A") +
        ylab("Recalculated mean difference Responder - NonResponders")
)
print(
    ggplot(comparison_df_aPD1, aes(x = `Student's T-test Difference R_NR`, y = diff_r_nr)) +
        geom_abline(slope = 1, intercept = 0, col = "red") +
        geom_point() +
        ggtitle("aPD1 mean difference of Responder - NonResponder", subtitle = "Data from Table S2B, R/NR-status from Table S1A, Removed 'SD' patients") +
        ylab("Recalculated mean difference Responder - NonResponders")
)
dev.off()
