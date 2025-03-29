## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----geneNR, eval=FALSE-------------------------------------------------------
# # Install the geneNR package (when published on CRAN)
# install.packages("geneNR")
# 
# or
# # Installation from GitHub (if not available on CRAN)
# # Uncomment and run the following commands manually:
# # if (!requireNamespace("devtools", quietly = TRUE)) {
# #   install.packages("devtools")
# # }
# # devtools::install_github("Nirmalaruban/geneNR")
# 

## ----geneQTL, eval=FALSE------------------------------------------------------
# result <- geneQTL("sample_data_wheat_qtl", crop="wheat")
# result <- geneQTL("sample_data_rice_qtl", crop="rice")
# 

## ----geneSNP, eval=FALSE------------------------------------------------------
# result <- geneSNP("sample_data_wheat", 10000, 10000, crop = "wheat")
# result <- geneSNP("sample_data_rice", 10000, 10000, crop = "rice")
# 

## ----geneSNPcustom, eval=FALSE------------------------------------------------
# result <- geneSNPcustom("sample_data_wheat_custom", crop = "wheat")
# 

## ----plot_SNP, eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.height=5, fig.width=12----
library(geneNR)
chromosome_details <- read.csv(system.file("extdata", "chromosome_details.csv", package = "geneNR"))
data <- read.csv(system.file("extdata", "identified_SNP.csv", package = "geneNR"))
chromosome_plot <- plot_SNP(chromosome_details = chromosome_details, data = data,  chromosome_color = "steelblue",  title = "Chromosome map with SNPs", label_color = "black", image_width = 15, image_height = 10)
chromosome_plot


## ----summariseSNP, eval=TRUE, echo=FALSE--------------------------------------
demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
 data <- import_hmp(demo_SNP)
snp_distribution <- summariseSNP(data)
snp_distribution

## ----plot_summariseSNP, eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.height=5, fig.width=8----
demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
data <- import_hmp(demo_SNP)
snp_distribution <- summariseSNP(data)
plot <- plot_summariseSNP(snp_distribution, bar_color = "skyblue",
label_size = 3, label_color = "red")
plot


