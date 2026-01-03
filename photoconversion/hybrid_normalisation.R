library(dplyr)

# Function to process a single CSV file
process_csv <- function(file_path) {
  print(paste("Processing:", file_path))  # Feedback
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Identify green and red columns (assumed to be last two columns)
  green_col <- ncol(data) - 1
  red_col <- ncol(data)
  
  # Check for minimum number of rows
  if (nrow(data) < 4) {
    warning(paste("Skipping", file_path, "- not enough rows"))
    return(NULL)
  }
  
  # Compute preconversion mean (rows 1–3)
  green_preconversion_mean <- mean(data[1:3, green_col])
  
  # Normalize green signal
  data <- data %>%
    mutate(
      Green_normalized = .[[green_col]] / green_preconversion_mean
    )
  
  # Get normalized green value at row 4
  green_normalized_baseline <- data$Green_normalized[4]
  
  # Apply hybrid normalization: subtract baseline from all normalized values
  data <- data %>%
    mutate(
      Green_hybrid = Green_normalized - green_normalized_baseline,
      Red_normalized = .[[red_col]] / data[4, red_col]
    )
  
  # Save to new file
  new_file_path <- sub("\\.csv$", "_normalized.csv", file_path)
  write.csv(data, new_file_path, row.names = FALSE)
}

# Function to process all CSV files in the current working directory
process_all_csvs <- function(directory = getwd()) {
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  lapply(files, process_csv)
}

# Run the batch processing
process_all_csvs()