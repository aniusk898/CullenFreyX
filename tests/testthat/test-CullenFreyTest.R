# Test for calculate_statistics function with valid data
test_that("calculate_statistics works correctly for a simple dataset", {
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_statistics(data)

  expect_equal(result$min, 1)
  expect_equal(result$max, 5)
  expect_equal(result$median, 3)
  expect_true(is.numeric(result$skewness_squared))
  expect_true(is.numeric(result$kurtosis))
})

# Test that calculate_statistics throws an error for small datasets (<4 data points)
test_that("calculate_statistics throws error for small datasets", {
  data <- c(1, 2, 3)
  expect_error(calculate_statistics(data),
               "Skewness and Kurtosis require at least 4 data points.")
})

# Test that calculate_statistics handles NA values by throwing an error
test_that("calculate_statistics throws error for NA values", {
  data <- c(1, 2, NA, 4, 5)
  expect_error(calculate_statistics(data),
               "Error: Invalid numeric values or NA values are not allowed in the dataset.")
})

# Test that calculate_statistics handles non-numeric elements by throwing an error
test_that("calculate_statistics throws an error for non-numeric values", {
  data <- c(1, 2, "a", 4)
  expect_error(calculate_statistics(data), "Error: Invalid input detected. Input must be numeric.")
})

# Test that calculate_statistics handles non-numeric input like '0.5'
test_that("calculate_statistics throws an error for malformed numeric values like 'o.5'", {
  data <- c(1, "o.5", 3, 4)
  expect_error(calculate_statistics(as.numeric(data)), "Error: Invalid input detected. Input must be numeric.")
})

# Test that calculate_statistics handles infinite values by throwing an error
test_that("calculate_statistics throws error for Inf values", {
  data <- c(1, 2, Inf, 4, 5)
  expect_error(calculate_statistics(data),
               "Inf values are not allowed in the dataset.")
})

# Test that calculate_statistics works correctly for larger datasets
test_that("calculate_statistics works correctly for larger datasets", {
  data <- rnorm(1000)  # A normal distribution
  result <- calculate_statistics(data)

  expect_true(is.numeric(result$min))
  expect_true(is.numeric(result$max))
  expect_true(is.numeric(result$median))
  expect_true(is.numeric(result$skewness_squared))
  expect_true(is.numeric(result$kurtosis))
})

# Test that calculate_statistics works for zero-variance datasets
test_that("calculate_statistics works for zero-variance datasets", {
  data <- rep(1, 100)
  result <- calculate_statistics(data)

  expect_equal(result$min, 1)
  expect_equal(result$max, 1)
  expect_equal(result$median, 1)
  expect_equal(result$skewness_squared, 0)
  expect_equal(result$kurtosis, 0)
})

# Test that calculate_statistics works with negative values
test_that("calculate_statistics works with negative values", {
  data <- c(-10, -5, 0, 5, 10)
  result <- calculate_statistics(data)

  expect_equal(result$min, -10)
  expect_equal(result$max, 10)
  expect_equal(result$median, 0)
  expect_true(is.numeric(result$skewness_squared))
  expect_true(is.numeric(result$kurtosis))
})

# Test for empty dataset
test_that("calculate_statistics throws error for empty dataset", {
  data <- numeric(0)
  expect_error(calculate_statistics(data), "Dataset must not be empty.")
})

# Test that calculate_statistics throws an error for NULL input
test_that("calculate_statistics throws error for NULL input", {
  expect_error(calculate_statistics(NULL), "Dataset must not be empty.")
})

# Tests for cullenfrey_x function

# Test for a simple dataframe input
test_that("cullenfrey_x works for a dataframe input", {
  data <- data.frame(A = rnorm(10), B = rnorm(10))
  expect_silent(cullenfrey_x(data))
})

# Test for a list of vectors input
test_that("cullenfrey_x works for a list of vectors", {
  data <- list(a = rnorm(100), b = runif(100))
  expect_silent(cullenfrey_x(data))
})

# Test for a list of dataframes input
test_that("cullenfrey_x works for a list of dataframes", {
  data <- list(df1 = data.frame(A = rnorm(100), B = runif(100)),
               df2 = data.frame(C = rpois(100, 5), D = rexp(100)))
  expect_silent(cullenfrey_x(data))
})

# Test for a single vector input
test_that("cullenfrey_x works for a single vector input", {
  data <- rnorm(10)
  expect_silent(cullenfrey_x(data))
})

# Test for input with missing names in a list
test_that("cullenfrey_x assigns names to unnamed list elements", {
  data <- list(rnorm(100), rexp(100))
  processed_data <- cullenfrey_x(data)
  expect_true(all(names(processed_data) != ""))
})

# Test for input with a matrix as input
test_that("cullenfrey_x works for matrix input", {
  data_matrix <- matrix(rnorm(100), nrow = 10)
  expect_silent(cullenfrey_x(data_matrix))
})
