library(dplyr)

#' Save Data and Log Summary Statistics
#'
#' This function saves a data frame to a CSV or RDS file and logs summary statistics
#' to a specified log file. If the dataset exceeds 5 million rows or 30 variables, 
#' only the number of observations and list of variables are logged.
#'
#' @param data A data frame to be saved.
#' @param file_path Path to the output file. Supports CSV or RDS format depending on file extension.
#' @param log_file Path to the log file where summary statistics will be written.
#'
#' @return None.
#' @examples
#' savedataR(mtcars, "output.rds", "log.txt")
#'
#' @export
savedataR <- function(data, file_path, log_file) {
  # Check that input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  num_rows <- nrow(data)
  num_vars <- ncol(data)

  print("Logging summary statistics to file...")
  
  # Buffer log output to minimize file I/O
  log_buffer <- c(
    paste("Number of observations:", num_rows),
    paste("Number of variables:", num_vars),
    "Variables:",
    paste(colnames(data), collapse = ", ")
  )
  
  # Add detailed summary if dataset is small
  if (num_rows <= 5e6 && num_vars <= 30) {
    # Select numeric columns and compute the mean
    numeric_data <- data %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(), mean, na.rm = TRUE))
    
    log_buffer <- c(
      log_buffer,
      "Detailed summary statistics:\n",
      capture.output(summary(data)),
      capture.output(print(numeric_data))
    )
  } else {
    log_buffer <- c(log_buffer, "\nDataset is too large for detailed summary.")
  }

  # Write the entire log buffer in one go
  writeLines(log_buffer, log_file)

  # Determine file format from file extension and save appropriately
  file_ext <- tools::file_ext(file_path)

  print("Saving data to file...") 
  if (file_ext == "csv") {
    # Save data as CSV using write_csv from readr (efficient)
    readr::write_csv(data, file_path)
    print("Data saved to CSV and summary stats logged to file.")
  } else if (file_ext == "rds") {
    # Save data as RDS using saveRDS
    saveRDS(data, file_path)
    print("Data saved to RDS and summary stats logged to file.")
  } else {
    stop("Unsupported file format. Please use .csv or .rds extension.")
  }
}
