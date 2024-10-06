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
               "NA values are not allowed in the dataset.")
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
  expect_error(calculate_statistics(data),
               "Inf values are not allowed in the dataset.")
})

# Test that calculate_statistics throws an error if input is not a vector
test_that("calculate_statistics throws error for non-vector input", {
  data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  expect_error(calculate_statistics(data), "Data must be numeric.")
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
  expect_error(calculate_statistics(NULL), "Data must be provided.")
})

# Tests for launch_cullen_frey_app function

# Test for a simple dataframe input
test_that("launch_cullen_frey_app works for a dataframe input", {
  data <- data.frame(A = rnorm(10), B = rnorm(10))
  expect_silent(launch_cullen_frey_app(data))
})

# Test for a list of vectors input
test_that("launch_cullen_frey_app works for a list of vectors", {
  data <- list(a = rnorm(100), b = runif(100))
  expect_silent(launch_cullen_frey_app(data))
})

# Test for a list of dataframes input
test_that("launch_cullen_frey_app works for a list of dataframes", {
  data <- list(df1 = data.frame(A = rnorm(100), B = runif(100)),
               df2 = data.frame(C = rpois(100, 5), D = rexp(100)))
  expect_silent(launch_cullen_frey_app(data))
})

# Test for a single vector input
test_that("launch_cullen_frey_app works for a single vector input", {
  data <- rnorm(10)
  expect_silent(launch_cullen_frey_app(data))
})

# Test for input with missing names in a list
test_that("launch_cullen_frey_app assigns names to unnamed list elements",
          {
            data <- list(rnorm(100), rexp(100))
            processed_data <- launch_cullen_frey_app(data)
            expect_true(all(names(processed_data) != ""))
          })

# Test that launch_cullen_frey_app throws an error for unsupported input types
test_that("launch_cullen_frey_app throws error for unsupported input types",
          {
            unsupported_data <- matrix(rnorm(100), nrow = 10)
            expect_error(
              launch_cullen_frey_app(unsupported_data),
              "Input must be a list, vector, or dataframe."
            )
          })
