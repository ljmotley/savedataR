library(data.table)

#' Save Data and Log Summary Statistics
#'
#' This function saves a data frame to a CSV file and logs summary statistics
#' to a specified log file. It uses data.table for fast and efficient operations
#' on large datasets.
#'
#' @param data A data frame to be saved.
#' @param csv_file Path to the output CSV file.
#' @param log_file Path to the log file where summary statistics will be written.
#'
#' @return None.
#' @examples
#' savedataR(mtcars, "output.csv", "log.txt")
#'
#' @export
savedataR <- function(data, csv_file, log_file) {
  # Check that input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Convert to data.table for faster operations
  data <- as.data.table(data)
  
  # Redirect output to log file
  sink(log_file)

  # Print summary stats to log
  cat("Summary statistics for the dataset:\n")
  print(summary(data))
  cat("\n")
  
  # Calculate means using data.table
  means <- data[, lapply(.SD, mean, na.rm = TRUE)]
  
  # Print the calculated means
  print(means)

  # Stop redirecting output to log file
  sink()

  # Save data as CSV using fwrite (more efficient than write.csv)
  fwrite(data, csv_file)
  
  message("Data saved to CSV and summary stats logged to file.")
}
