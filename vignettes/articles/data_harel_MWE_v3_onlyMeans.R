library(dplyr)
dir.create("intermediate_data")
download.file(
    url = "https://ars.els-cdn.com/content/image/1-s2.0-S0092867419309006-mmc1.xlsx",
    destfile = "intermediate_data/harel2019-s1.xlsx",
    method = "wget"
)
harel_pheno <- readxl::read_excel("intermediate_data/harel2019-s1.xlsx", sheet = "S1A", skip = 1, na = "NaN")
# Patients were categorized into responders (complete or partial regression) and
# non-responders (progressive disease) according to RECIST v1.0 guidelines
# "Response" contains "PD", "PR", "CR" or "SD" (stable disease)
harel_pheno <- harel_pheno |>
    # Filter out stable disease
    dplyr::filter(Response %in% c("PD", "PR", "CR")) |>
    # Add a column with the response as defined in the paper
    dplyr::mutate(response_paper = ifelse(Response == "PD", "non-responder", "responder"))

outcome <- harel_pheno[["response_paper"]]
# Metadata contains spaces in the sample names
# the actual data always "_" instead of spaces
names(outcome) <- gsub(" ", "_", harel_pheno[["Sample ID"]])

download.file(
    url = "https://ars.els-cdn.com/content/image/1-s2.0-S0092867419309006-mmc2.xlsx",
    destfile = "intermediate_data/harel2019-S2.xlsx",
    method = "wget"
)
harel_til_s2_ttest <- readxl::read_excel("intermediate_data/harel2019-S2.xlsx", sheet = "S2A", skip = 1, na = "NaN")
harel_aPD1_s2_ttest <- readxl::read_excel("intermediate_data/harel2019-S2.xlsx", sheet = "S2B", skip = 1, na = "NaN")
# head(rownames(harel_til_mat))
# colnames(harel_aPD1_s2_ttest)
data_v2 <- list(
    "til" = as.matrix(harel_til_s2_ttest[, grepl("^TIL_", colnames(harel_til_s2_ttest))]),
    "aPD1" = as.matrix(harel_aPD1_s2_ttest[, grepl("^PD1_", colnames(harel_aPD1_s2_ttest))])
)
na_names_til_i <- which(is.na(harel_til_s2_ttest[["Gene names"]]))
harel_til_s2_ttest[["Gene names"]][na_names_til_i] <- paste0("NA_", seq_len(length(na_names_til_i)))

na_names_aPD1 <- which(is.na(harel_aPD1_s2_ttest[["Gene names"]]))
if (length(na_names_aPD1)) {
    stop("NA names in aPD1")
}
rownames(data_v2[["til"]]) <- harel_til_s2_ttest[["Gene names"]]
rownames(data_v2[["aPD1"]]) <- harel_aPD1_s2_ttest[["Gene name"]]


harel_means <- lapply(data_v2, function(datapart_x) {
    res_t <- apply(datapart_x, 1, function(feature_x) {
        # names(x) are the column names of the matrix
        # The response needs to be ordered according to the column names
        if (!all(names(feature_x) %in% names(outcome))) {
            stop("Not all samples found")
        }
        x_NonResponder <- split(feature_x, outcome[names(feature_x)])
        x_means <- lapply(x_NonResponder, mean, na.rm = TRUE)
        return(c("diff_r_nr" = x_means[["responder"]] - x_means[["non-responder"]]))
    })
    tmp <- tibble::as_tibble(as.matrix(res_t), rownames = "gene")
    colnames(tmp) <- c("gene", "diff_r_nr")
    return(tmp)
})
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
