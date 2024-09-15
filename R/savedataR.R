#' Save Data and Log Summary Statistics
#'
#' This function saves a data frame to a CSV file and logs summary statistics
#' to a specified log file. Optionally, it can run in parallel to improve
#' performance on large datasets.
#'
#' @param data A data frame to be saved.
#' @param csv_file Path to the output CSV file.
#' @param log_file Path to the log file where summary statistics will be written.
#' @param parallel Logical. If TRUE, uses parallel processing for summary computations. Default is FALSE.
#'
#' @return None.
#' @examples
#' savedataR(mtcars, "output.csv", "log.txt", parallel = FALSE)
#'
#' @export
savedataR <- function(data, csv_file, log_file, parallel = FALSE) {
  # Check that input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Load necessary libraries
  library(dplyr)

  # Optionally set up parallel processing
  if (parallel) {
    library(future.apply)
    plan(multisession)  # Use parallel workers
  }
  
  # Redirect output to log file
  sink(log_file)
  
  # Print summary stats to log
  cat("Summary statistics for the dataset:\n")
  print(summary(data))
  cat("\n")

  # Detailed summary with optional parallelization
  if (parallel) {
    cat("Running in parallel mode...\n")
    means <- future_sapply(data, function(col) mean(col, na.rm = TRUE))
  } else {
    cat("Running in sequential mode...\n")
    means <- data %>% summarize(across(everything(), mean, na.rm = TRUE))
  }
  
  print(means)

  # Stop redirecting output to log file
  sink()
  
  # Save data as CSV
  write.csv(data, csv_file, row.names = FALSE)
  
  message("Data saved to CSV and summary stats logged to file.")
}
