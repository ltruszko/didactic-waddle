library(dplyr)

# Function to process a single CSV file
process_csv <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Calculate normalization factors
  X <- mean(data[1:3, ncol(data) - 1])  # Green: mean of first 3 rows
  Y <- data[4, ncol(data)]              # Red: value in 4th row
  
  # Append new columns
  data <- data %>%
    mutate(Green_normalized = .[[ncol(data) - 1]] / X,
           Red_normalized = .[[ncol(data)]] / Y)
  
  # Create new file name
  new_file_path <- sub("\\.csv$", "_normalized.csv", file_path)
  
  # Save the processed data to a new CSV file
  write.csv(data, new_file_path, row.names = FALSE)
}

# Function to process all CSV files in the current working directory
process_all_csvs <- function(directory = getwd()) {
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  lapply(files, process_csv)
}

# Example usage
process_all_csvs()
