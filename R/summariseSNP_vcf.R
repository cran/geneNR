#' Distribution of SNPs Across Chromosomes from VCF
#'
#' @param vcf_data A `vcfR` object containing VCF data.
#' @returns A data frame with chromosome names and the count of SNPs for each chromosome.
#' @export
#'
#' @examples
#' \donttest{
#' demo_SNP <- system.file("extdata", "demo_SNP.vcf", package = "geneNR")
#' vcf_data <- import_vcf(demo_SNP)
#' snp_distribution <- summariseSNP_vcf(vcf_data)
#' print(snp_distribution)
#' }
summariseSNP_vcf <- function(vcf_data) {
  # Ensure the input data is a vcfR object
  if (!inherits(vcf_data, "vcfR")) {
    stop("The input data must be a vcfR object.")
  }

  # Extract chromosome information from the VCF data
  chrom_data <- vcf_data@fix[, "CHROM"]

  # Convert chromosome information to factor for consistent grouping
  chrom_data <- as.factor(chrom_data)

  # Count the number of SNPs per chromosome
  snp_counts <- table(chrom_data)

  # Convert the table to a data frame
  result <- as.data.frame(snp_counts)

  # Rename the columns for clarity
  colnames(result) <- c("Chr", "SNP_Count")

  # Return the result as a data frame
  return(result)
}



