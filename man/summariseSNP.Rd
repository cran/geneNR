% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseSNP.R
\name{summariseSNP}
\alias{summariseSNP}
\title{Distribution of SNPs Across Chromosomes}
\usage{
summariseSNP(data)
}
\arguments{
\item{data}{A data frame containing a column named \code{Chr}}
}
\value{
A data frame with chromosome names and the count of SNPs for each chromosome
}
\description{
Distribution of SNPs Across Chromosomes
}
\examples{
\donttest{
demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
data <- import_hmp(demo_SNP)
snp_distribution <- summariseSNP(data)
print(snp_distribution)
}
}
