
#' Imports Hapmap genotypic data file
#'
#' @param file_path Provide the actual path of Hapmap genotypic data file
#' @param header by default it will be True
#' @param sep by default it will be tab separated
#' @param stringsAsFactors by default it will be False
#'
#' @returns Hampmap genotypic data
#' @export
#'
#' @examples
#' \donttest{
#' demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
#' hapmap_data <- import_hmp(demo_SNP)
#' head(hapmap_data)
#' }
import_hmp <- function(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Read the HapMap file using read.delim from utils
  hmp_data <- utils::read.delim(file = file_path,
                                header = header,
                                sep = sep,
                                stringsAsFactors = stringsAsFactors)

  # Return the imported data
  return(hmp_data)
}
