library(dplyr)
library(readr)

process_final_output <- function(filename) {
  data <- read_csv(filename, show_col_types = FALSE)
  
  # Print column names for debugging
  print(paste("Processing:", filename))
  print(colnames(data))
  
  required_cols <- c("Mean_green", "Mean_green_bg", "Mean_red")
  
  if (all(required_cols %in% colnames(data))) {
    data <- data %>%
      mutate(Green_corrected = (Mean_green - 0.5) / (Mean_green_bg - 0.5),
             Red_corrected = (Mean_red - 0.5) / (Mean_green_bg - 0.5))
    write_csv(data, filename)
    print(paste("Processed file:", filename))
  } else {
    missing <- setdiff(required_cols, colnames(data))
    warning(paste("Skipping file:", filename, "- missing columns:", paste(missing, collapse = ", ")))
  }
}

final_output_files <- list.files(pattern = "_final_output.csv")
lapply(final_output_files, process_final_output)
