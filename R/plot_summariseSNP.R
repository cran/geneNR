#' Plot SNP Distribution Across Chromosomes
#'
#' @description Creates a bar chart representing the distribution of SNPs across chromosomes.
#' Allows customization of bar color, label size, and label color. Saves the plot to a user-specified directory or a temporary directory.
#'
#' @param snp_distribution A data frame with columns `Chr` and `SNP_Count`.
#' @param file_name The name of the file to save the plot (default: "snp_bar_chart.jpeg").
#' @param output_dir The directory to save the file (default: `tempdir()`).
#' @param bar_color The color of the bars (default: "lightblue").
#' @param label_size The size of the text labels on the bars (default: 3).
#' @param label_color The color of the text labels on the bars (default: "black").
#' @return A `ggplot` object for the created bar chart.
#' @export
#'
#' @examples
#' \donttest{
#' demo_SNP <- system.file("extdata", "demo_SNP.hmp.txt", package = "geneNR")
#' data <- import_hmp(demo_SNP)
#' snp_distribution <- summariseSNP(data)
#' plot <- plot_summariseSNP(snp_distribution, bar_color = "skyblue",
#' label_size = 3, label_color = "red")
#' print(plot)
#' }
plot_summariseSNP <- function(snp_distribution, file_name = "snp_bar_chart.jpeg", output_dir = tempdir(),
                              bar_color = "lightblue", label_size = 3, label_color = "black") {


  data <- snp_distribution

  # Check if the input data has the required columns
  required_cols <- c("Chr", "SNP_Count")
  if (!all(required_cols %in% colnames(snp_distribution))) {
    stop("The input data frame must contain columns 'Chr' and 'SNP_Count'.")
  }

  Chr <- data$Chr
  SNP_Count <- data$SNP_count

  # Create the bar chart using ggplot2 with rlang::data
  bar_chart <- ggplot2::ggplot(snp_distribution, ggplot2::aes(x = Chr, y = SNP_Count)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_color, color = "black", width = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = SNP_Count), vjust = -0.5, size = label_size, color = label_color) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(snp_distribution$SNP_Count) + 50)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "SNP Distribution Across Chromosomes",
      x = "Chromosomes",
      y = "Number of SNPs"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text.x = ggplot2::element_text(hjust = 0.5, size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      legend.position = "none"
    )

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist.")
  }

  # Construct the full file path
  file_path <- file.path(output_dir, file_name)

  # Save the plot as a JPEG file
  ggplot2::ggsave(file_path, bar_chart, dpi = 320, height = 5, width = 10, units = "in")
  message("Bar chart successfully saved at: ", file_path)

  # Return the ggplot object
  return(bar_chart)
}
