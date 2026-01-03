# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)

# Function to read the third column from a file
extract_third_column <- function(filename) {
  data <- read_csv(filename, show_col_types = FALSE)
  print(paste("Reading file:", filename))  # Print filename being read
  if (ncol(data) >= 3) {
    return(data[, 3, drop = FALSE])  # Select the third column
  } else {
    warning(paste("File does not have at least 3 columns:", filename))
    return(data.frame(Third_Column = NA))  # Return NA if there are less than 3 columns
  }
}

# Function to create the final output file for each prefix group
create_final_output <- function(prefix) {
  patterns <- list(
    green = paste0(prefix, "_green.csv"),
    green_bg = paste0(prefix, "_green_background.csv"),
    red = paste0(prefix, "_red.csv"),
    red_bg = paste0(prefix, "_red_background.csv")
  )
  
  columns_list <- list()
  
  for (pattern in names(patterns)) {
    files <- list.files(pattern = patterns[[pattern]])
    if (length(files) > 0) {
      third_cols <- lapply(files, extract_third_column)
      combined_third_col <- bind_rows(third_cols)
      columns_list[[pattern]] <- combined_third_col
    }
  }
  
  final_data <- bind_cols(columns_list)
  colnames(final_data) <- c("Mean_green", "Mean_green_bg", "Mean_red", "Mean_red_bg")
  output_filename <- paste0(prefix, "_final_output.csv")
  write_csv(final_data, output_filename)
}

# Get list of unique prefixes
all_files <- list.files()
unique_prefixes <- unique(str_extract(all_files, ".*_FRAP_\\d{3}"))

# Loop through each unique prefix and create the final output files
for (prefix in unique_prefixes) {
  create_final_output(prefix)
}

