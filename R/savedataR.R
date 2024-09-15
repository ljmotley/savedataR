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
