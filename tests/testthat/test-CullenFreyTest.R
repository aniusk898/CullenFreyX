library(testthat)

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
  expect_error(calculate_statistics(data), "Skewness and Kurtosis require at least 4 data points.")
})

# Test that calculate_statistics handles NA values by throwing an error
test_that("calculate_statistics throws error for NA values", {
  data <- c(1, 2, NA, 4, 5)
  expect_error(calculate_statistics(data), "NA values are not allowed in the dataset.")
})

# Test that calculate_statistics handles non-numeric elements by throwing an error
test_that("calculate_statistics throws an error for non-numeric values", {
  # Test for non-numeric input
  data <- c(1, 2, "a", 4)
  expect_error(calculate_statistics(data), "Data must be numeric.")
})

# Test that calculate_statistics handles infinite values by throwing an error
test_that("calculate_statistics throws error for Inf values", {
  data <- c(1, 2, Inf, 4, 5)
  expect_error(calculate_statistics(data), "Inf values are not allowed in the dataset.")
})

# Test that calculate_statistics throws an error if input is not a vector
test_that("calculate_statistics throws error for non-vector input", {
  data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  expect_error(calculate_statistics(data), "Data must be a numeric vector.")
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
