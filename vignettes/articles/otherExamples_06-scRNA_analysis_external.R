# Script from https://github.com/DomenicoSkyWalker89/CD8-T-lymphocytes-MAIT/blob/main/SCRIPT_scRNA-seq_CD8.R
# Modified by GG, 2023-10-12
#######################################
# scRNAseq script 2019 UMAP_SEURAT 3.0 #
######################################
#### set parallelization
library(future)
plan(multisession)

options(future.globals.maxSize = 4096 * 1024^3)
library(cowplot)
# for some reason, Matrix 1.6-1 seems to NOT work? https://github.com/satijalab/seurat/issues/6746
# Seurat::GetAssayData(object = s_obj, slot = "count")
# Error in validityMethod(as(object, superClass)) :
#   object 'Csparse_validate' not found
packageurl <- "https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-4.1.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")
library(Matrix)
library(Seurat)
library(ggplot2)

n_fun <- function(x) {
    return(data.frame(y = mean(x), label = paste0(signif(mean(x), 3))))
}

PercentAbove <- function(x, threshold) {
    return(length(x = x[x > threshold]) / length(x = x))
}

read_ddSEQ <- function(data.dirs) {
    all.data <- list()
    for (i in seq_along(data.dirs))
    {
        fdir <- data.dirs[i]

        # print(paste("working on",fdir,"."))

        if (substr(fdir, nchar(fdir), nchar(fdir)) != "/") {
            fdir <- paste0(fdir, "/")
        }

        if (!dir.exists(fdir)) {
            stop(paste("Directory does not exist:", fdir))
        }

        umiCounts <- list.files(path = fdir, pattern = "*.umiCounts.passingKneeFilter.table.csv")

        if (length(umiCounts) == 0) {
            stop(paste("No UMI counts found in", fdir))
        } else if (length(umiCounts) > 1) {
            stop(paste("Found UMI counts for multiple samples in", fdir))
        } else {
            umiData <- read.table(paste0(fdir, umiCounts), sep = ",", row.names = 1, header = T)
        }

        sampleId <- strsplit(umiCounts, split = "_")[[1]][1]
        rownames(umiData) <- paste(sampleId, rownames(umiData), sep = "_")

        all.data[[i]] <- t(umiData)
    }
    all.data <- as(do.call(cbind, all.data), "sparseMatrix")
    return(all.data)
}

read_ddSEQ_metaData <- function(data.dirs) {
    all.data <- list()
    for (i in seq_along(data.dirs))
    {
        fdir <- data.dirs[i]

        if (substr(fdir, nchar(fdir), nchar(fdir)) != "/") {
            fdir <- paste0(fdir, "/")
        }

        # read cell summary
        cellSummary <- list.files(path = fdir, pattern = "*.cell.summary.csv")

        if (length(cellSummary) != 1) {
            stop(paste("Need one cell summary file:", fdir))
        } else {
            cellSummaryData <- read.delim(paste0(fdir, cellSummary), sep = ",")
        }

        if ("AboveThreshold" %in% names(cellSummaryData)) {
            cellSummaryData <- cellSummaryData[cellSummaryData$AboveThreshold == "True", ]
        } else if ("Filter" %in% names(cellSummaryData)) {
            cellSummaryData <- cellSummaryData[cellSummaryData$Filter == "Pass", ]
        }

        # read in abundant read counts
        abundantReads <- list.files(path = fdir, pattern = "*.abundantReadCounts.csv")
        if (length(abundantReads) != 1) {
            stop(paste("Need one abundant read count summary file in:", fdir))
        } else {
            abundantReadData <- read.delim(paste0(fdir, abundantReads), sep = ",")
            abundantReadData <- abundantReadData[abundantReadData$CellId %in% cellSummaryData$CellId, ]

            chrM <- subset(abundantReadData, abundantReadData$AbundantFeature == "chrM")[, -2]
            rRNA <- subset(abundantReadData, abundantReadData$AbundantFeature == "rRNA")[, -2]
            sncRNA <- subset(abundantReadData, abundantReadData$AbundantFeature == "sncRNA")[-2]

            cellSummaryData$pct.mito <- round((chrM$Count[match(cellSummaryData$CellId, chrM$CellId)] / cellSummaryData$GeneReadCount) * 100, 2)
            cellSummaryData$pct.rRNA <- round((rRNA$Count[match(cellSummaryData$CellId, rRNA$CellId)] / cellSummaryData$GeneReadCount) * 100, 2)
            cellSummaryData$pct.sncRNA <- round((sncRNA$Count[match(cellSummaryData$CellId, sncRNA$CellId)] / cellSummaryData$GeneReadCount) * 100, 2)

            sampleId <- strsplit(cellSummary, split = "_")[[1]][1]
            rownames(cellSummaryData) <- cellSummaryData$CellId
            rownames(cellSummaryData) <- paste(sampleId, rownames(cellSummaryData), sep = "_")

            cellSummaryData <- cellSummaryData[, which(colnames(cellSummaryData) %in% c("GeneReadCount", "rRNAReads", "ErccCorrelation", "pct.mito", "pct.rRNA", "pct.sncRNA"))]
        }
        all.data[[i]] <- cellSummaryData
    }
    all.data <- do.call(rbind, all.data)
    all.data[, c("pct.mito", "pct.rRNA", "pct.sncRNA")][is.na(all.data[, c("pct.mito", "pct.rRNA", "pct.sncRNA")])] <- 0
    if (all(is.na(all.data$ErccCorrelation))) {
        all.data <- all.data[, -which(colnames(all.data) == "ErccCorrelation")]
    }
    return(all.data)
}

##################################################################################################################################
##################################################################################################################################


##########################
# CHANGED 2023-10-12: Download data #
##########################
gse <- GEOquery::getGEO("GSE166181", getGPL = TRUE)
bd <- "intermediate_data/2021-Biasi"
# supfiles <- GEOquery::getGEOSuppFiles("GSE166181", fetch_files = TRUE, baseDir = bd)
supfiles <- GEOquery::getGEOSuppFiles("GSE166181", fetch_files = FALSE, baseDir = bd)
loaded_supfiles <- lapply(supfiles[["fname"]], function(x) data.table::fread(file.path(bd, "GSE166181", x)))
names(loaded_supfiles) <- supfiles[["fname"]]

rownames(loaded_supfiles[[2]]) <- loaded_supfiles[[2]][["V1"]]
loaded_supfiles[[2]][, 1] <- NULL
rownames(loaded_supfiles[[3]]) <- loaded_supfiles[[3]][["V1"]]
loaded_supfiles[[3]][, 1] <- NULL

loaded_supfiles[[1]] <- as.data.frame(loaded_supfiles[[1]])
rownames(loaded_supfiles[[1]]) <- loaded_supfiles[[1]][["cell_ID"]]
lapply(loaded_supfiles, dim)

loaded_supfiles_mats <- lapply(
    loaded_supfiles[2:3], function(x) {
        x_mat <- as.matrix(x)
        rownames(x_mat) <- rownames(x)
        return(x_mat)
    }
)

library(tibble)
checking_names <- tibble(
    celln_meta = loaded_supfiles[["GSE166181_Metadata.tsv.gz"]][[1]],
    celln_mat = colnames(loaded_supfiles[["GSE166181_Raw_UMI_CountMatrix_unnormalized_.tsv.gz"]])
) |>
    dplyr::mutate(are_equal = celln_meta == celln_mat)
if (!all(checking_names$are_equal)) {
    stop("Cell names in metadata and matrix are not equal")
}
# qs::qsave(
#     list(
#         loaded_supfiles_mats = loaded_supfiles_mats,
#         loaded_supfiles = loaded_supfiles
#     ),
#     file = "removeme.qrds"
# )
s_obj <- Seurat::CreateSeuratObject(
    # Either a matrix-like object with unnormalized data with cells as columns and features as rows or an Assay-derived object
    counts = loaded_supfiles_mats[["GSE166181_Raw_UMI_CountMatrix_unnormalized_.tsv.gz"]],
    # A data frame with cells as rows and metadata as columns
    meta.data = loaded_supfiles[["GSE166181_Metadata.tsv.gz"]],
    assay = "RNA", project = "GSE166181"
)
Seurat::SetAssayData(
    object = s_obj,
    assay = "RNA",
    slot = "data",
    new.data = loaded_supfiles_mats[["GSE166181_Normalized_UMI_CountMatrix.tsv.gz"]]
)


# ###############
# # Import Data #
# ###############

# ### Each sample is in its own directory,
# ### each directory contains *.cell.summary.csv, *.abundantReadCounts.csv, *.umiCounts.passingKneeFilter.table.csv


# #########################################################
# ##### MELA T0 FILES #####################################
# #########################################################

# data.dirs <- list.files("C:/Users/Utente/Documents/UNIVERSITA/LAB/scRNA seq/T0_ALL", full.names = TRUE)

# # Function to read data

# data.input <- read_ddSEQ(data.dirs)

# # Function to read metadata

# metadata.input <- read_ddSEQ_metaData(data.dirs)

# # Clean Matrix delete row with 0 value
# sum(rowSums(data.input) == 0)
# data.input <- data.input[rowSums(data.input) > 0, ]

# #########################
# # Create Seurat Objects #
# ########################

# s1 <- CreateSeuratObject(data.input, meta.data = metadata.input)

# #########################################################
# ##### MELA T1 FILES #####################################
# ########################################################

# data.dirs <- list.files("C:/Users/Utente/Documents/UNIVERSITA/LAB/scRNA seq/T1_ALL", full.names = TRUE)

# # Function to read data

# data.input <- read_ddSEQ(data.dirs)

# # Function to read metadata

# metadata.input <- read_ddSEQ_metaData(data.dirs)

# #  Clean Matrix delete row with 0 value
# sum(rowSums(data.input) == 0)
# data.input <- data.input[rowSums(data.input) > 0, ]

# #########################
# # Create Seurat Objects #
# ########################

# s2 <- CreateSeuratObject(data.input, meta.data = metadata.input)

# #########################################################
# ##### MELA T2 FILES #####################################
# ########################################################
# data.dirs <- list.files("C:/Users/Utente/Documents/UNIVERSITA/LAB/scRNA seq/T2_ALL", full.names = TRUE)

# # Function to read data

# data.input <- read_ddSEQ(data.dirs)

# # Function to read metadata

# metadata.input <- read_ddSEQ_metaData(data.dirs)

# #  Clean Matrix delete row with 0 value
# sum(rowSums(data.input) == 0)
# data.input <- data.input[rowSums(data.input) > 0, ]

# #########################
# # Create Seurat Objects #
# ########################

# s3 <- CreateSeuratObject(data.input, meta.data = metadata.input)

# # Merge Seurat OBJECT
# dat.combined <- merge(s1, y = c(s2, s3))
dat.combined <- s_obj

# Add new metadata

dat.combined@meta.data$time <- unlist(lapply(strsplit(as.character(dat.combined@meta.data$orig.ident), split = "-"), "[[", 1))
dat.combined@meta.data$replicate <- unlist(lapply(strsplit(as.character(dat.combined@meta.data$orig.ident), split = "-"), "[[", 2))
dat.combined@meta.data$group <- unlist(lapply(strsplit(as.character(dat.combined@meta.data$time), split = " "), "[[", 2))
dat.combined@meta.data$response <- unlist(lapply(strsplit(as.character(dat.combined@meta.data$time), split = " "), "[[", 1))
dat.combined@meta.data$patients <- unlist(lapply(strsplit(as.character(dat.combined@meta.data$replicate), split = " "), "[[", 1))

# Subset OBJ by group
s1 <- subset(x = dat.combined, subset = group == "T0")
s2 <- subset(x = dat.combined, subset = group == "T1")
s3 <- subset(x = dat.combined, subset = group == "T2")

# Cell count (using different metadata)
Counts <- as.data.frame(table(dat.combined@meta.data$time))
pdf("removeme.pdf")
ggplot(dat.combined@meta.data, aes(unlist(time))) +
    geom_bar(aes(fill = unlist(time))) +
    xlab("Sample") +
    ylab("Cells") +
    geom_text(data = Counts, aes(x = Var1, y = Freq), label = Counts$Freq, vjust = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
dev.off()
# Cell count by replicate
s1Counts <- as.data.frame(table(s1@meta.data$replicate))
s2Counts <- as.data.frame(table(s2@meta.data$replicate))
s3Counts <- as.data.frame(table(s3@meta.data$replicate))
pdf("removeme.pdf")
ggplot(s1@meta.data, aes(unlist(replicate))) +
    geom_bar(aes(fill = unlist(replicate))) +
    xlab("Sample") +
    ylab("Cells") +
    geom_text(data = s1Counts, aes(x = Var1, y = Freq), label = s1Counts$Freq, vjust = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
ggplot(s2@meta.data, aes(replicate)) +
    geom_bar(aes(fill = factor(replicate))) +
    xlab("Sample") +
    ylab("Cells") +
    geom_text(data = s2Counts, aes(x = Var1, y = Freq), label = s2Counts$Freq, vjust = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
ggplot(s3@meta.data, aes(replicate)) +
    geom_bar(aes(fill = factor(replicate))) +
    xlab("Sample") +
    ylab("Cells") +
    geom_text(data = s3Counts, aes(x = Var1, y = Freq), label = s3Counts$Freq, vjust = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
dev.off()
# # Quality Control and Filtration {.tabset}
# 1. nGene = Number of unique genes observed in that barcode ( >= 500 );
# 2. nUMI = Number of UMIs observed in that barcode ( >= 1000 )
# 3. pct.rRNA = Percentage of total reads mapped to rRNA (potential factor for biased clustering; < 10% )
# 4. pct.mito = Percentage of total reads mapped to mitochondrial genome (marker of cell stress during sample prep; < 10% )
# 5. pct.sncRNA = Percentage of total reads mapped to small non-coding RNA ( < 5% )
# 6. GeneReadCount = Number of genic reads per cell ( > 2000 )



# Before Filtering
x1 <- ggplot(s1@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
x2 <- ggplot(s2@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
x3 <- ggplot(s3@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
pdf("removeme.pdf", width = 50)
plot_grid(x1, x2, x3)
dev.off()
y1 <- VlnPlot(s1, features = c("nFeature_RNA", "nCount_RNA", "pct.mito", "pct.rRNA"), ncol = 4, group.by = "group", pt.size = 0.1)
y2 <- VlnPlot(s2, features = c("nFeature_RNA", "nCount_RNA", "pct.mito", "pct.rRNA"), ncol = 4, group.by = "group", pt.size = 0.1)
y3 <- VlnPlot(s3, features = c("nFeature_RNA", "nCount_RNA", "pct.mito", "pct.rRNA"), ncol = 4, group.by = "group", pt.size = 0.1)
pdf("removeme.pdf", width = 50, height = 25)
plot_grid(y1, y2, y3, nrow = 3)
dev.off()
# Filter cells
# 2023-10-12: Update: "novelty" does not exist, so I removed it
# s1 <- subset(s1, subset = nFeature_RNA > 130 & nFeature_RNA < 1500 & pct.mito < 10 & nCount_RNA < 2500 & novelty > 0.2 & pct.rRNA < 10)
dim(s1)
s1_new <- subset(s1, subset = nFeature_RNA > 130 & nFeature_RNA < 1500 & pct.mito < 10 & nCount_RNA < 2500 & pct.rRNA < 10)
s2_new <- subset(s2, subset = nFeature_RNA > 130 & nFeature_RNA < 1500 & pct.mito < 10 & nCount_RNA < 2500 & pct.rRNA < 10)
s3_new <- subset(s3, subset = nFeature_RNA > 130 & nFeature_RNA < 1500 & pct.mito < 10 & nCount_RNA < 2500 & pct.rRNA < 10)
if (identical(s1_new, s1) & identical(s2_new, s2) & identical(s3_new, s3)) {
    rm(s1_new, s2_new, s3_new)
} else {
    stop("There were additional cells filtered out - why would that be the case for the reported count matrix?")
}
# After Filtering

x1 <- ggplot(s1@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
x2 <- ggplot(s2@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
x3 <- ggplot(s3@meta.data, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(aes(color = replicate))
pdf("removeme.pdf", height = 25, width = 25)
plot_grid(x1, x2, x3, nrow = 3)
dev.off()

#  Number of filtered cells
# Here the reported number of filtered cells should always be 0 after we already use the normalized data!
s1_1 <- subset(x = dat.combined, subset = group == "T0")
s2_2 <- subset(x = dat.combined, subset = group == "T1")
s3_3 <- subset(x = dat.combined, subset = group == "T2")
print(paste("Filtered", dim(GetAssayData(object = s1_1, slot = "counts"))[2] - dim(GetAssayData(object = s1))[2], "of", dim(GetAssayData(object = s1_1, slot = "counts"))[2], "cells."))
print(paste("Filtered", dim(GetAssayData(object = s2_2, slot = "counts"))[2] - dim(GetAssayData(object = s2))[2], "of", dim(GetAssayData(object = s2_2, slot = "counts"))[2], "cells."))
print(paste("Filtered", dim(GetAssayData(object = s3_3, slot = "counts"))[2] - dim(GetAssayData(object = s3))[2], "of", dim(GetAssayData(object = s3_3, slot = "counts"))[2], "cells."))


# Normalize and find variables
s1_new <- NormalizeData(s1, verbose = FALSE)
s2_new <- NormalizeData(s2, verbose = FALSE)
s3_new <- NormalizeData(s3, verbose = FALSE)
if (identical(s1_new, s1)) {
    stop("Why are s1_new and s1 identical? They shouldn't be")
}
if (Matrix::all.equal(
    Seurat::GetAssayData(s1_new, slot = "data"),
    Seurat::GetAssayData(s1, slot = "data")
)) {
    stop("Why are s1_new and s1 identical? They shouldn't be.")
    # Explanation:
    # s1 has been normalized and PROBABLY integrated across s1, s2 and s3
    # Therefore using the INTEGRATED count data to renormalize
    # is not the same as using the ORIGINAL count data to renormalize
}
s1 <- FindVariableFeatures(s1, selection.method = "vst", nfeatures = 2000)
s2 <- FindVariableFeatures(s2, selection.method = "vst", nfeatures = 2000)
s3 <- FindVariableFeatures(s3, selection.method = "vst", nfeatures = 2000)

# Create list of gene names from all 6 Seurat objects
total.genes <- list(
    rownames(s1),
    rownames(s2),
    rownames(s3)
)

# Get common gene names
common.genes <- Reduce(f = intersect, x = total.genes)


# # Intergration  # HAS BEEN DONE ALREADY!
immune.combined <- s_obj
s1
immune.combined <- FindVariableFeatures(immune.combined, selection.method = "vst", nfeatures = 2000)
# immune.anchors <- FindIntegrationAnchors(object.list = list(s1, s2, s3), dims = 1:20, verbose = TRUE)
# FingIntegrationAnchors:
#   anchor.features <- Reduce(f = intersect, x = all.rownames)
#   anchor.features <- SelectIntegrationFeatures(
#             object.list = object.list,
#             nfeatures = anchor.features,
#             assay = assay
#         )
#       # FindVariableFeatures on EACH element in object.list
#       # Sort the features how often they were variable features
#       # the variable features are intersected with the features present in all objects
#       # Sort the feature ranks per object, finally select nFeatures (=2000 default) with the highest median rank

# immune.combined <- IntegrateData(anchorset = immune.anchors, dims = 1:20, features.to.integrate = common.genes)

Seurat::SetAssayData(
    object = immune.combined,
    assay = "integrated", slot = "data",
    new.data = Seurat::GetAssayData(immune.combined, slot = "data")
)
# Cell count for 'time' after filtering
Counts_2 <- as.data.frame(table(immune.combined@meta.data$time))
pdf("removeme.pdf")
ggplot(immune.combined@meta.data, aes(unlist(time))) +
    geom_bar(aes(fill = unlist(time))) +
    xlab("Sample") +
    ylab("Cells") +
    geom_text(data = Counts_2, aes(x = Var1, y = Freq), label = Counts_2$Freq, vjust = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
dev.off()


# DefaultAssay(immune.combined) <- "integrated"

# Run scaling and PCA
immune.combined <- ScaleData(immune.combined, vars.to.regress = c("nCount_RNA", "pct.mito"), verbose = TRUE)
qs::qsave(immune.combined, "intermediate_data/2021-Biasi/immune.combined.qrds")

# 2023-10-12: Here comes now the problem:
#   RunPCA uses the most variable genes, but IntegrateData establishes them differently
#   than just taking the most variable genes _after_ integration. Therefore we
#   will necessarily have some differences in the PCA results AND ALL FOLLOW-UP analyses

immune.combined <- RunPCA(immune.combined, npcs = 30, verbose = FALSE)
qs::qsave(immune.combined, "intermediate_data/2021-Biasi/immune.combined.qrds")
immune.combined <- qs::qread("intermediate_data/2021-Biasi/immune.combined.qrds")
# Number of PCs to choose
pdf("removeme.pdf")
DimHeatmap(immune.combined, dims = 1:15, cells = 500, balanced = TRUE)
ElbowPlot(immune.combined)
dev.off()
# UMAP and Clustering

set.seed <- 123
immune.combined <- RunUMAP(immune.combined, reduction = "pca", dims = 1:9)
# Seurat version 3.0 (used in the publication) still used umap from the python package umap-learn
# which used "correlation" as distance, therefore we expect additional differences here
immune.combined <- FindNeighbors(immune.combined, reduction = "pca", dims = 1:9)

res.try <- c(0.05, 0.1, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
immune.combined <- FindClusters(immune.combined, resolution = res.try)

# Deciding what resolution to use

library(clustree)
pdf("removeme_2.pdf")
# clustree(immune.combined, prefix = "integrated_snn_res.")
clustree(immune.combined, prefix = "RNA_snn_res.")
dev.off()

# Plot the best resolution 'res'

# Idents(immune.combined) <- "integrated_snn_res.0.5"
Idents(immune.combined) <- "RNA_snn_res.0.5"
p2 <- DimPlot(immune.combined, reduction = "umap", label = FALSE, pt.size = 1)
pdf("removeme.pdf", height = 10, width = 10)
plot_grid(p2)
dev.off()
# Cell number within each cluster

# table(immune.combined@meta.data$integrated_snn_res.0.4)
table(immune.combined@meta.data$RNA_snn_res.0.5)
table(immune.combined@meta.data$RNA_snn_res.0.4)


# Heatmap of top 10 expressed genes
qs::qsave(immune.combined, "intermediate_data/2021-Biasi/immune.combined_postClustering.qrds")
immune.combined <- qs::qread("intermediate_data/2021-Biasi/immune.combined_postClustering.qrds")
library(viridis)
library(dplyr)
#' FindAllMarkers:
#' @param slot Slot to pull data from; note that if \code{test.use} is "negbinom", "poisson", or "DESeq2",
#' \code{slot} will be set to "counts". [Default: "data"]
immune.combined.markers <- FindAllMarkers(immune.combined, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.5)
immune.combined.markers_v2 <- FindAllMarkers(immune.combined, only.pos = TRUE, min.pct = 0.1, logfc.threshold = 0.5)
immune.combined.markers_v3 <- FindAllMarkers(immune.combined, only.pos = TRUE, min.pct = 0, logfc.threshold = 0.5)
# r$> length(unique(immune.combined.markers_v3$gene))
# [1] 176
# r$> length(unique(immune.combined.markers$gene))
# [1] 176

### 2023-10-12: Customization: Use only logfc.threshold = 0.5, not min.pct as the calculation is potentially different?
for (n_permutations in c(1e3, 10e3, 25e3)) {
    rroc.markers <- NULL
    for (cluster in levels(Seurat::Idents(immune.combined))) {
        # seu_cluster <- immune.combined[, immune.combined[["RNA_snn_res.0.5"]] == cluster]
        # seu_NOTcluster <- immune.combined[, immune.combined[["RNA_snn_res.0.5"]] != cluster]

        # data_cluster <- Seurat::GetAssayData(seu_cluster, slot = "data")
        # data_NOTcluster <- Seurat::GetAssayData(seu_NOTcluster, slot = "data")

        cells.1 <- colnames(immune.combined)[immune.combined[["RNA_snn_res.0.5"]] == cluster]
        cells.2 <- colnames(immune.combined)[immune.combined[["RNA_snn_res.0.5"]] != cluster]
        fc.result <- Seurat::FoldChange(
            object = immune.combined,
            ident.1 = cluster
        )
        relevant_features <- rownames(fc.result |> dplyr::filter(avg_log2FC > .5))

        rroc_res_cluster <- sapply(relevant_features, function(f_x) {
            restrictedROC::simple_rROC_permutation(
                response = (immune.combined[["RNA_snn_res.0.5"]])[, 1, drop = TRUE],
                predictor = as.vector(Seurat::GetAssayData(immune.combined, slot = "data")[f_x, ]),
                positive_label = cluster,
                direction = "<",
                n_permutations = n_permutations,
                parallel_permutations = TRUE
            )
        }, simplify = FALSE, USE.NAMES = TRUE)
        qs::qsave(rroc_res_cluster, file = paste0("intermediate_data/2021-Biasi/rroc_res_cluster_", cluster, "_nperm", n_permutations, ".qrds"))
        rroc_res_cluster_markers <- do.call(rbind, lapply(rroc_res_cluster, function(x) {
            c(x$permutation_pval, "auc.global" = x$global$auc, "auc.max_total" = x$max_total$auc)
        }))
        if (is.null(rroc.markers)) {
            rroc.markers <- rroc_res_cluster_markers
        } else {
            rroc.markers <- rbind(rroc.markers, rroc_res_cluster_markers)
        }
    }
    qs::qsave(rroc.markers, file = paste0("intermediate_data/2021-Biasi/rroc.markers", "_nperm", n_permutations, ".qrds"))
}



immune.combined.markers %>%
    group_by(cluster) %>%
    top_n(n = 2, wt = avg_log2FC)
top10 <- immune.combined.markers %>%
    group_by(cluster) %>%
    top_n(n = 10, wt = avg_log2FC)
pdf("removeme.pdf")
DoHeatmap(immune.combined, features = top10$gene, size = 2, angle = 90) #+ scale_color_viridis(option = 'B')
dev.off()

# # Cluster QC plots
# To assess cluster quality, we map metadata onto the clusters.
# This approach allows us to determine whether clusters exist for
# biological reasons or if they are defined by some experimental or technical factor (UMI count, read count, etc.).
# Should clusters be driven by an artifact, repeating the analysis and regressing out that trait is appropriate.


FeaturePlot(immune.combined, features = c("nCount_RNA", "pct.mito"), min.cutoff = "q9", cols = c("grey", "red"))


# Heatmap of top 10 expressed genes

library(viridis)
library(dplyr)
immune.combined.markers <- FindAllMarkers(immune.combined, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.5)
immune.combined.markers %>%
    group_by(cluster) %>%
    top_n(n = 2, wt = avg_logFC)
top10 <- immune.combined.markers %>%
    group_by(cluster) %>%
    top_n(n = 10, wt = avg_logFC)
DoHeatmap(immune.combined, features = top10$gene, size = 2, angle = 90) #+ scale_color_viridis(option = 'B')


# Identify conserved cell-type markers

### CLUSTER_0
cluster_0.markers <- FindMarkers(immune.combined, ident.1 = 0, verbose = FALSE)
head(cluster_0.markers)
write.table(cluster_0.markers, file = "cluster_0.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_1
cluster_1.markers <- FindMarkers(immune.combined, ident.1 = 1, verbose = FALSE)
head(cluster_1.markers)
write.table(cluster_1.markers, file = "cluster_1.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_2
cluster_2.markers <- FindMarkers(immune.combined, ident.1 = 2, verbose = FALSE)
head(cluster_2.markers)
write.table(cluster_2.markers, file = "cluster_2.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_3
cluster_3.markers <- FindMarkers(immune.combined, ident.1 = 3, verbose = FALSE)
head(cluster_3.markers)
write.table(cluster_3.markers, file = "cluster_3.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_4
cluster_4.markers <- FindMarkers(immune.combined, ident.1 = 4, verbose = FALSE)
head(cluster_4.markers)
write.table(cluster_4.markers, file = "cluster_4.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_5
cluster_5.markers <- FindMarkers(immune.combined, ident.1 = 5, verbose = FALSE)
head(cluster_5.markers)
write.table(cluster_5.markers, file = "cluster_5.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_6
cluster_6.markers <- FindMarkers(immune.combined, ident.1 = 6, verbose = FALSE)
head(cluster_6.markers)
write.table(cluster_6.markers, file = "cluster_6.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_7
cluster_7.markers <- FindMarkers(immune.combined, ident.1 = 7, verbose = FALSE)
head(cluster_7.markers)
write.table(cluster_7.markers, file = "cluster_7.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_8
cluster_8.markers <- FindMarkers(immune.combined, ident.1 = 8, verbose = FALSE)
head(cluster_8.markers)
write.table(cluster_8.markers, file = "cluster_8.markers.txt", sep = "\t", quote = FALSE, row.names = T)

### CLUSTER_9
cluster_9.markers <- FindMarkers(immune.combined, ident.1 = 9, verbose = FALSE)
head(cluster_9.markers)
write.table(cluster_9.markers, file = "cluster_9.markers.txt", sep = "\t", quote = FALSE, row.names = T)

# find all markers distinguishing cluster X from clusters Y
clusterXY.markers <- FindMarkers(immune.combined, ident.1 = X, ident.2 = Y, min.pct = 0.25)
head(cluster93.markers, n = 5)

# Plot Known Markers
FeaturePlot(
    immune.combined,
    features = c("SELL", "CCR7", "TCF7", "GZMB", "GZMH", "S100A4", "GNLY", "CD69", "NKG7", "TYROBP", "LYZ"),
    min.cutoff = "q10", max.cutoff = "q90", ncol = 5
)
