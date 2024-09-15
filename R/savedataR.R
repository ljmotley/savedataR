#' Save Data and Log Summary Statistics
#'
#' This function saves a data frame to a CSV file and logs summary statistics
#' to a specified log file.
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
  
  # Redirect output to log file
  sink(log_file)
  
  # Print summary stats to log
  cat("Summary statistics for the dataset:\n")
  print(summary(data))
  cat("\n")
  
  # Optionally, add more detailed summaries (e.g., via dplyr)
  library(dplyr)
  data %>%
    summarize(across(everything(), mean, na.rm = TRUE)) %>%
    print()
  
  # Stop redirecting output to log file
  sink()
  
  # Save data as CSV
  write.csv(data, csv_file, row.names = FALSE)
  
  message("Data saved to CSV and summary stats logged to file.")
}
