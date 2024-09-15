# Load the testthat package and your package
library(testthat)
library(savedataR)

# Test: Check if the function works with valid inputs
test_that("savedataR works with valid inputs", {
  # Create temporary file paths for CSV and log
  csv_file <- tempfile(fileext = ".csv")
  log_file <- tempfile(fileext = ".txt")
  
  # Sample data frame
  data <- mtcars
  
  # Run the function (expect no errors)
  expect_silent(savedataR(data, csv_file, log_file))
  
  # Check if the CSV and log files are created
  expect_true(file.exists(csv_file))
  expect_true(file.exists(log_file))
  
  # Check if the CSV file contains data
  saved_data <- read.csv(csv_file)
  expect_equal(nrow(saved_data), nrow(data))
  expect_equal(ncol(saved_data), ncol(data))
  
  # Check if the log file contains summary statistics
  log_content <- readLines(log_file)
  expect_true(any(grepl("Summary statistics for the dataset", log_content)))
  expect_true(any(grepl("Min.", log_content)))  # This indicates summary output
})

# Test: Check error handling for non-data frame input
test_that("savedataR throws error for non-data frame input", {
  csv_file <- tempfile(fileext = ".csv")
  log_file <- tempfile(fileext = ".txt")
  
  # Invalid input (numeric vector instead of data frame)
  invalid_data <- 1:10
  
  # Expect an error message when running the function
  expect_error(savedataR(invalid_data, csv_file, log_file), 
               "Input must be a data frame.")
})

# Test: Check behavior when files already exist
test_that("savedataR overwrites existing files", {
  # Create temporary file paths for CSV and log
  csv_file <- tempfile(fileext = ".csv")
  log_file <- tempfile(fileext = ".txt")
  
  # Create dummy files with some content
  write.csv(data.frame(x = 1:5), csv_file, row.names = FALSE)
  writeLines("Existing log content", log_file)
  
  # Sample data frame
  data <- mtcars
  
  # Run the function
  savedataR(data, csv_file, log_file)
  
  # Check if the existing content is overwritten in both files
  saved_data <- read.csv(csv_file)
  expect_equal(nrow(saved_data), nrow(data))
  expect_equal(ncol(saved_data), ncol(data))
  
  log_content <- readLines(log_file)
  expect_true(any(grepl("Summary statistics for the dataset", log_content)))
  expect_false(any(grepl("Existing log content", log_content)))  # Old content should be gone
})

# Test: Check if mean summary works correctly
test_that("savedataR calculates correct mean values", {
  csv_file <- tempfile(fileext = ".csv")
  log_file <- tempfile(fileext = ".txt")
  
  # Sample data frame
  data <- mtcars
  
  # Run the function
  savedataR(data, csv_file, log_file)
  
  # Read log content and check for the correct mean
  log_content <- readLines(log_file)
  
  # For example, check the mean for the "mpg" column
  mpg_mean <- mean(mtcars$mpg)
  expect_true(any(grepl(paste0("mpg[ ]*", format(mpg_mean, digits = 4)), log_content)))
})

# Test: Check behavior with missing values
test_that("savedataR handles missing values correctly", {
  csv_file <- tempfile(fileext = ".csv")
  log_file <- tempfile(fileext = ".txt")
  
  # Sample data frame with missing values
  data_with_na <- mtcars
  data_with_na[1, 1] <- NA  # Introduce an NA in the first column
  
  # Run the function
  savedataR(data_with_na, csv_file, log_file)
  
  # Read log content and check for the summary of missing data
  log_content <- readLines(log_file)
  
  # Check that it still calculates means correctly (ignoring NA values)
  mpg_mean_with_na <- mean(data_with_na$mpg, na.rm = TRUE)
  expect_true(any(grepl(paste0("mpg[ ]*", format(mpg_mean_with_na, digits = 4)), log_content)))
})
