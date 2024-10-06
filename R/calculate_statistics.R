#' Calculate the k-th data_moment
#'
#' This function calculates the k-th data_moment of a given dataset.
#'
#' @param data A numeric vector representing the dataset.
#' @param k An integer representing the order of the data_moment to be calculated.
#'
#' @return A numeric value representing the k-th data_moment of the dataset.
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' data_moment(data, 2) # Calculate the second data_moment (The variance)
data_moment <- function(data, k) {
  m1 <- mean(data)
  return(sum((data - m1) ^ k) / length(data))
}

#' Calculate Statistics for a Given Dataset
#'
#' This function is a generic method to calculate various statistics such as mean, variance, skewness, kurtosis, and range for a given numeric dataset.
#'
#' The function dispatches based on the class of the input data and calculates statistics accordingly.
#'
#' @param data A numeric vector representing the dataset.
#'
#' @return A list containing calculated statistics such as minimum, maximum, mean, variance, standard deviation, skewness, and kurtosis.
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' calculate_statistics(data)
calculate_statistics <- function(data) {
  if (!is.numeric(data)) {
    stop("Data must be numeric.")
  }
  UseMethod("calculate_statistics")
}

#' Calculate Statistics for Numeric Data
#'
#' This function calculates key statistics such as skewness, kurtosis, minimum, maximum, mean, variance, and standard deviation for a numeric dataset.
#' The skewness and kurtosis are computed using standard formulas, with kurtosis representing the Fisher kurtosis (excess kurtosis).
#'
#' @param data A numeric vector for which the statistics will be calculated. Must contain at least 4 data points.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{min}{The minimum value of the data.}
#'   \item{max}{The maximum value of the data.}
#'   \item{mean}{The mean (average) of the data.}
#'   \item{variance}{The variance (second data_moment) of the data.}
#'   \item{sd}{The standard deviation of the data.}
#'   \item{median}{The median of the data.}
#'   \item{skewness}{The skewness of the data.}
#'   \item{skewness_squared}{The square of the skewness.}
#'   \item{kurtosis}{The kurtosis (excess kurtosis) of the data.}
#' }
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' calculate_statistics(data)
calculate_statistics.numeric <- function(data) {
  # Check if data is NULL before anything else
  if (is.null(data)) {
    stop("Data must be provided.")
  }

  # Check if the dataset is empty
  if (length(data) == 0) {
    stop("Dataset must not be empty.")
  }

  # Ensure the data is a numeric vector
  if (!is.numeric(data) || !is.vector(data)) {
    stop("Data must be a numeric vector.")
  }

  # Check for NA or Inf values in the data
  if (any(is.na(data))) {
    stop("NA values are not allowed in the dataset.")
  }

  if (any(is.infinite(data))) {
    stop("Inf values are not allowed in the dataset.")
  }

  # Ensure at least 4 data points for skewness and kurtosis calculations
  n <- length(data)
  if (n < 4) {
    stop("Skewness and Kurtosis require at least 4 data points.")
  }

  # Handle zero-variance datasets (all values are the same)
  if (length(unique(data)) == 1) {
    return(list(
      min = min(data),
      max = max(data),
      mean = mean(data),
      variance = 0,
      sd = 0,
      median = median(data),
      skewness = 0,
      skewness_squared = 0,
      kurtosis = 0
    ))
  }

  method.numeric <- method.unbiased
  calculate_data = method(data)

  # Calculate data_moments
  mean_data = mean(data) # First data_moment
  variance_data = data_moment(data, 2) # Second data_moment (Variance)
  skewness_data <-calculate_data$skewness  # Third data_moment (Skewness)
  kurtosis_data <-calculate_data$kurtosis # Fourth data_moment (Excess kurtosis)

  # Other statistics
  min_data <- min(data)
  max_data <- max(data)
  median_data <- median(data)
  skewness_squared <- skewness_data ^ 2

  # Create result list with the "statistics" class
  result <- list(
    min = min_data,
    max = max_data,
    mean = mean_data,
    median = median_data,
    sd = sqrt(variance_data),
    variance = variance_data,
    skewness = skewness_data,
    skewness_squared = skewness_squared,
    kurtosis = kurtosis_data
  )

  class(result) <- "statistics"

  return(result)
}

#' Print Method for Statistics Object
#'
#' This function prints the calculated statistics (minimum, maximum, mean, variance, standard deviation, skewness, and kurtosis) for a numeric dataset.
#'
#' @param x An object of class "statistics" created by the \code{calculate_statistics} function.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return This function prints the statistics but does not return a value.
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' stats <- calculate_statistics(data)
#' print(stats)
print.statistics <- function(x, ...) {
  cat("Statistics for the data:\n")
  cat("Minimum: ", x$min, "\n")
  cat("Maximum: ", x$max, "\n")
  cat("Mean: ", x$mean, "\n")
  cat("Median: ", x$median, "\n")
  cat("Standard Deviation: ", x$sd, "\n")
  cat("Variance: ", x$variance, "\n")
  cat("Skewness: ", x$skewness, "\n")
  cat("Skewness Squared: ", x$skewness_squared, "\n")
  cat("Kurtosis: ", x$kurtosis, "\n")
}
