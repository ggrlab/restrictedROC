outdir <- file.path("intermediate_data/2022-lee/results")
dir.create(outdir)
library(h2o)
# h2o.shutdown(prompt = FALSE)
h2o.init()

source("dev/lee2022-utils.R")
library(Biobase)
library(TreeSummarizedExperiment)

lee_tse <- dataMelanoma::lee2022_treesummarizedexperiment
data_lee <- t(dataMelanoma::lee2022_relative_abundance_preprocessed)
pheno_lee <- dataMelanoma::lee2022_pheno_curated
rownames(data_lee) == pheno_lee$subject_id

pheno_lee_pfs <- pheno_lee[!is.na(pheno_lee[["PFS12"]]), ]
data_lee_pfs <- data_lee[pheno_lee_pfs$subject_id, ]
lee_tse_pfs <- lee_tse[, pheno_lee_pfs$subject_id]
SummarizedExperiment::assay(lee_tse_pfs) <- t(data_lee_pfs)


outcome <- "PFS12"
table_lsubcohort_pfs <- table(
    pheno_lee_pfs[["lee_subcohort"]],
    factor(pheno_lee_pfs[[outcome]], levels = c("yes", "no"))
)

library(future)
plan(multisession)
repeated_cv_n <- 1
k_fold_cv <- 5
for (data_x in list(lee_tse_pfs)) {
    library(ggplot2)
    for (subcohort_XX in c("all", rownames(table_lsubcohort_pfs))) {
        # for (subcohort_XX in c("all", "PRIMM-NL", "PRIMM-UK")) {
        # pdf(
        #     file.path(
        #         outdir,
        #         paste0(
        #             "CrossValidate-",
        #             subcohort_XX,
        #             "_roccurvesBoundUnbound.pdf"
        #         )
        #     ),
        #     width = 15, height = 15
        # )
        # for (featureselection_type in c(2, 1, 0, 3)) {
        for (featureselection_type in c(0)) { # use all features, effectively no feature selection
            cat(
                "\n\nsubcohort: ", subcohort_XX,
                # "  datatype: ", datatype,
                " FS:", featureselection_type,
                "\n\n"
            )
            rocs <- sapply(c("bounded", "full"), function(datatype) {
                savepath <- paste0(
                    "intermediate_data/2022-lee/CrossValidate_v3/",
                    subcohort_XX,
                    "_",
                    datatype,
                    "_FS", featureselection_type
                )
                dir.create(dirname(savepath), recursive = TRUE, showWarnings = FALSE)
                repeated_cv_results_file <- paste0(savepath, "-repeated_cv_results.qrds")
                rocs_cv <- qs::qread(, file = paste0(repeated_cv_results_file, "-rocs_cv.qs"))
                return(rocs_cv)
            })
            # If alternative="two.sided", a two-sided test for difference in AUC is performed.
            # If alternative="less", the alternative is that the AUC of roc1 is smaller than
            # the AUC of roc2. For method="venkatraman", only “two.sided” test is available.
            print(pROC::roc.test(rocs[["full"]], rocs[["bounded"]], alternative = "less"))
            # dev.off()
        }
    }
}
