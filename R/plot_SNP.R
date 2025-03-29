#' Plot SNP Distribution on Chromosome Map
#'
#' @description Plots SNP positions across chromosomes with centromere markers using given chromosome details and SNP data.
#'
#' @param chromosome_details A data frame containing chromosome details with columns `Chr`, `start` and `stop`
#' @param data A data frame containing SNP data with columns `Chr`, `Pos`, and `SNP`.
#' @param chromosome_color Color of the chromosome bars (default: "skyblue").
#' @param title Title of the chromosome plot depicting the identified SNPs
#' @param label_color Color of the SNP labels (default: "black").
#' @param image_width width of the chromosome plot
#' @param image_height height of the chromosome plot
#' @return A `ggplot` object for the SNP distribution plot.
#' @export
#'
#' @examples
#' \donttest{
#' chromosome_details <- read.csv(system.file("extdata", "chromosome_details.csv", package = "geneNR"))
#' data <- read.csv(system.file("extdata", "identified_SNP.csv", package = "geneNR"))
#' chromosome_plot <- plot_SNP(chromosome_details = chromosome_details, data = data,
#' chromosome_color = "steelblue" ,title = "Chromosome map with SNPs", label_color = "black",
#' image_width = 15, image_height = 10)
#' print(chromosome_plot)
#' }
plot_SNP <- function(chromosome_details, data,
                     chromosome_color = "steelblue",
                     title = "Chromosome map with SNPs",
                     label_color = "black",
                     image_width = 10, image_height = 10) {

  # Ensure required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed. Please install it using install.packages('ggplot2').")
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("The 'ggrepel' package is required but not installed. Please install it using install.packages('ggrepel').")
  }

  # Validate input data
  required_chromosome_cols <- c("Chr", "start", "stop")
  required_data_cols <- c("Chr", "Pos", "SNP")

  if (!all(required_chromosome_cols %in% colnames(chromosome_details))) {
    stop("`chromosome_details` must contain the columns: Chr, start, stop, Centromere.")
  }
  if (!all(required_data_cols %in% colnames(data))) {
    stop("`data` must contain the columns: Chr, Pos, SNP.")
  }




  # Normalize chromosome and SNP data
  chromosome_details$Centromere <- chromosome_details$stop/2
  chromosome_details$ChrNumeric <- as.numeric(as.factor(chromosome_details$Chr))
  data$ChrNumeric <- chromosome_details$ChrNumeric[match(data$Chr, chromosome_details$Chr)]
  ChrNumeric <- data$ChrNumeric




  chromosome_details$startNorm <- chromosome_details$start - chromosome_details$Centromere
  chromosome_details$stopNorm <- chromosome_details$stop - chromosome_details$Centromere
  chromosome_details$CentromereNorm <- 0
  CentromereNorm <- chromosome_details$CentromereNorm
  startNorm <- chromosome_details$startNorm
  stopNorm <- chromosome_details$stopNorm


  data$PosNorm <- data$Pos - chromosome_details$Centromere[match(data$Chr, chromosome_details$Chr)]
  data$ChrNormX <- chromosome_details$ChrNumeric[match(data$Chr, chromosome_details$Chr)]
  PosNorm <-data$PosNorm
  ChrNormX <- data$ChrNormX
  SNP <- data$SNP



  # Dynamically set linewidth and point size based on image dimensions
  segment_width <- max(image_width, image_height) / 10  # Adjust scaling factor as needed
  point_size <- max(image_width, image_height) / 70     # Adjust scaling factor as needed

  # Generate the plot
  plot <- ggplot2::ggplot() +
    # Plot chromosome bars
    ggplot2::geom_segment(
      data = chromosome_details,
      ggplot2::aes(
        x = ChrNumeric, xend = ChrNumeric,
        y = startNorm, yend = stopNorm
      ),
      color = chromosome_color, linewidth = segment_width, lineend = "round"
    ) +
    # Add centromeres
    ggplot2::geom_point(
      data = chromosome_details,
      ggplot2::aes(x = ChrNumeric, y = CentromereNorm),
      color = chromosome_color, size = point_size
    ) +
    # Add SNP positions
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(
        x = ChrNormX-0.1, xend = ChrNormX + 0.1,
        y = PosNorm, yend = PosNorm
      ),
      color = "black", linewidth = segment_width / 8
    ) +
    # Add SNP labels
    ggrepel::geom_text_repel(
      data = data,
      ggplot2::aes(x = ChrNormX + 0.4, y = PosNorm, label = SNP),
      color = label_color, size = segment_width,
      box.padding = 0.1, direction = "y",
      segment.color = "black", segment.size = 0.5,
      max.overlaps = Inf
    ) +
    # Customize plot appearance
    ggplot2::labs(
      title = title,
      x = "Chromosome",
      y = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = unique(chromosome_details$ChrNumeric),
      labels = unique(chromosome_details$Chr)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  return(plot)
}

