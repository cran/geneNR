#' Identifies Candidate Genes based on identified Quantitative Trati Loci (QTL) analysis
#'
#' @param data_file The input data in .csv format. (sample_data_wheat_qtl or sample_data_rice_qtl for demo purpose)
#' @param crop Either "wheat" or "rice". (by default it will be wheat)
#'
#' @returns A data frame containing traits, QTL, gene_id, gene_size, and gene_type.
#' @export
#'
#' @examples
#' load(system.file("extdata", "precomputed_sample_results_qtl.rda", package = "geneNR"))
#' message(sample_results)
#' \donttest{
#' result <- geneQTL("sample_data_wheat_qtl", crop="wheat")
#' result <- geneQTL("sample_data_rice_qtl", crop="rice")
#' #result <- geneQTL("your_results.csv", crop="wheat")
#' }
geneQTL <- function(data_file,  crop = "wheat") {
  # Save current working directory and options, ensure they are reset
  parent_dir <- getwd()
  on.exit(setwd(parent_dir), add = TRUE)  # Reset working directory

  # Save current options and ensure they are reset
  old_options <- options()  # Save all current options
  on.exit(options(old_options), add = TRUE)  # Reset all options to their original state

  # Check for required packages
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("The 'readr' package is required but not installed. Please install it using install.packages('readr').")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required but not installed. Please install it using install.packages('stringr').")
  }
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("The 'utils' package is required but not installed. Please install it using install.packages('utils').")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required but not installed. Please install it using install.packages('httr').")
  }
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("The 'rvest' package is required but not installed. Please install it using install.packages('rvest').")
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("The 'xml2' package is required but not installed. Please install it using install.packages('xml2').")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("The 'writexl' package is required but not installed. Please install it using install.packages('writexl').")
  }

  # Define file paths for sample data included in the package
  sample_data_wheat_qtl <- sample_data_wheat_qtl
  sample_data_rice_qtl <- sample_data_rice_qtl


  # Load data
  if (data_file == "sample_data_wheat_qtl") {
    data <- sample_data_wheat_qtl
  }else if (data_file == "sample_data_rice_qtl") {
    data <- sample_data_rice_qtl
  } else {
    if (!file.exists(data_file)) {
      stop("The specified data file does not exist.")
    }
    data <- readr::read_csv(data_file)
  }

  # Print column names for verification
  message(colnames(data))

  # Check if essential columns exist
  required_columns <- c("start","stop", "Chr", "traits")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Rest of your code...
  base_url <- switch(crop,
                     "wheat" = "https://plants.ensembl.org/Triticum_aestivum/Export/Output/Location?db=core;flank3_display=0;flank5_display=0;output=csv;r='a';strand=feature;param=gene;_format=Text",
                     "rice" = "https://plants.ensembl.org/Oryza_sativa/Export/Output/Location?db=core;flank3_display=0;flank5_display=0;output=csv;r='a';strand=feature;param=gene;_format=Text",
                     stop("Unsupported crop type"))

  data$search_window <- paste0(data$Chr, ":", data$start, "-", data$stop)
  data$url <- stringr::str_replace(base_url, "'a'", data$search_window)
  data$file_name <- paste0(data$traits, "_", data$Chr, "_", data$start, "_", data$stop)

  message(utils::head(data))

  temp_dir <- tempdir()
  new_folder <- file.path(temp_dir, "rough_results")
  if (!dir.exists(new_folder)) {
    dir.create(new_folder)
  }
  setwd(new_folder)

  results <- data.frame(file_name = character(), search_window = character(), text_content = character(), stringsAsFactors = FALSE)

  retry_request <- function(url, max_attempts = 3, delay = 5) {
    attempt <- 1
    while (attempt <= max_attempts) {
      tryCatch({
        response <- httr::GET(url, httr::timeout(5))
        if (httr::status_code(response) == 200) {
          return(response)
        } else {
          stop("Server returned non-200 status code")
        }
      }, error = function(e) {
        if (attempt == max_attempts) {
          stop("Max attempts reached. Error: ", e)
        } else {
          Sys.sleep(delay)
          attempt <- attempt + 1
        }
      })
    }
  }

  for (i in 1:nrow(data)) {
    url <- data$url[i]
    tryCatch({
      response <- retry_request(url)
      webpage <- xml2::read_html(response)
      text_content <- rvest::html_text(rvest::html_nodes(webpage, "body"))
      file_name <- paste0(data$file_name[i], ".txt")
      results <- rbind(results, data.frame(file_name = file_name, search_window = data$search_window[i], text_content = text_content, stringsAsFactors = FALSE))
      writeLines(text_content, file_name)
    }, error = function(e) {
      message(paste("Error processing URL:", url, "Error:", e))
    })
  }

  files <- list.files(pattern = "\\.txt$")
  data_list <- lapply(files, function(file) {
    df <- readr::read_delim(file, delim = ",")
    df$file_name <- file
    return(df)
  })

  data_data <- do.call(rbind, data_list)

  if (!all(c("file_name", "seqname", "start", "end", "gene_id", "gene_type") %in% colnames(data_data))) {
    stop("The data does not contain the expected columns.")
  }

  data_data$file_name <- as.character(data_data$file_name)
  data_data$seqname <- as.character(data_data$seqname)
  data_data$start <- as.character(data_data$start)
  data_data$stop <- as.character(data_data$end)
  data_data$gene_size <- paste0(data_data$seqname, ":", data_data$start, "-", data_data$stop)

  data_data <- data_data[, c("file_name", "gene_size", "gene_id", "gene_type")]

  df <- data_data
  filtered_df <- df[grepl("^[^_]+\\d+$", df$gene_id) & !grepl("LC", df$gene_id), ]
  filtered_df <- filtered_df[!duplicated(filtered_df$gene_id), ]

  split_columns <- stringr::str_match(filtered_df$file_name, "^(.*?)_(.*)\\.txt$")
  filtered_df$traits <- split_columns[, 2]
  filtered_df$QTL <- split_columns[, 3]


  filtered_df <- filtered_df[, c("traits", "QTL", "gene_size", "gene_id", "gene_type")]

  message(filtered_df)
  setwd(parent_dir)
  output_file <- tempfile("filtered_gene_id", fileext = ".xlsx")
  writexl::write_xlsx(filtered_df, output_file)
  message(paste("Filtered gene IDs saved to", output_file))
  unlink(new_folder, recursive = TRUE)

  # Return the filtered data frame to the environment
  return(filtered_df)
}
