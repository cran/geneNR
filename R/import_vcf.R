#' Imports VCF (Variant Call Format) data file
#'
#' @param file_path Provide the actual path of the VCF file
#'
#' @returns A `vcfR` object containing the imported data
#' @export
#'
#' @examples
#' \donttest{
#' demo_SNP <- system.file("extdata", "demo_SNP.vcf", package = "geneNR")
#' vcf_data <- import_vcf(demo_SNP)
#' vcf_data
#' }
import_vcf <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Load the vcfR package
  if (!requireNamespace("vcfR", quietly = TRUE)) {
    stop("The 'vcfR' package is required but is not installed. Install it using install.packages('vcfR').")
  }

  # Import the VCF file using vcfR
  vcf_data <- vcfR::read.vcfR(file_path)

  # Return the imported VCF data
  return(vcf_data)
}

