#' Distribution of SNPs Across Chromosomes
#'
#' @param data A data frame containing a column named `Chr`
#' @returns A data frame with chromosome names and the count of SNPs for each chromosome
#' @export
#'
#' @examples
#' \donttest{
#' demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
#' data <- import_hmp(demo_SNP)
#' snp_distribution <- summariseSNP(data)
#' print(snp_distribution)
#' }
summariseSNP <- function(data) {
  # Ensure the Chr column exists in the input data frame
  if (!"Chr" %in% colnames(data)) {
    stop("The input data frame does not contain a column named 'Chr'.")
  }

  # Convert Chr column to factor for consistent grouping
  data$Chr <- as.factor(data$Chr)

  # Count the number of entries per factor (chromosome)
  snp_counts <- table(data$Chr)

  # Convert the table to a data frame
  result <- as.data.frame(snp_counts)

  # Rename the columns for clarity
  colnames(result) <- c("Chr", "SNP_Count")

  # Return the result as a data frame
  return(result)
}

