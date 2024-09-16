#' Calculate the k-th Moment
#'
#'This function calculates the k-th moment of a given dataset.
#'
#' @param data A numeric vector representing the dataset.
#' @param k An integer representing the order of the moment to be calculated.
#'
#' @return A numeric value representing the k-th moment of the dataset.
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' moment(data, 2) # Calculate the second moment (The variance)
moment <- function(data, k) {
  m1 <- mean(data)
  return(sum((data - m1) ^ k) / length(data))
}

#' Calculate Statistics for a Given Dataset
#'
#' This function is a generic method to calculate various statistics such as skewness, kurtosis, median, and range for a given numeric dataset.
#'
#' The function dispatches based on the class of the input data and calculates statistics accordingly.
#'
#' @param data A numeric vector representing the dataset.
#'
#' @return A list containing calculated statistics such as minimum, maximum, median, skewness, and kurtosis.
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
#' This function calculates key statistics such as skewness, kurtosis, minimum, maximum, and median for a numeric dataset.
#' The skewness and kurtosis are computed using standard formulas, with kurtosis representing the Fisher kurtosis (excess kurtosis).
#'
#' @param data A numeric vector for which the statistics will be calculated. Must contain at least 4 data points.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{min}{The minimum value of the data.}
#'   \item{max}{The maximum value of the data.}
#'   \item{median}{The median of the data.}
#'   \item{skewness_squared}{The square of the skewness of the data.}
#'   \item{kurtosis}{The kurtosis (excess kurtosis) of the data.}
#' }
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' calculate_statistics(data)
calculate_statistics.numeric <- function(data) {
  # Check that data is numeric and a vector
  if (!is.numeric(data) || !is.vector(data)) {
    stop("Data must be a numeric vector.")
  }

  # Check for NA or Inf values
  if (any(is.na(data))) {
    stop("NA values are not allowed in the dataset.")
  }
  if (any(is.infinite(data))) {
    stop("Inf values are not allowed in the dataset.")
  }

  n <- length(data)

  if (n < 4) {
    stop("Skewness and Kurtosis require at least 4 data points.")
  }

  # Skewness calculation
  skewness <- function(data) {
    sd <- sqrt(moment(data, 2))
    return(moment(data, 3) / sd^3)
  }

  # Kurtosis calculation
  kurtosis <- function(data) {
    var <- moment(data, 2)
    return(moment(data, 4) / var^2)
  }

  # Calculate skewness and kurtosis
  skewness_data <- skewness(data)
  kurtosis_data <- kurtosis(data)

  # Other statistics
  min_data <- min(data)
  max_data <- max(data)
  median_data <- median(data)
  skewness_squared <- skewness_data^2

  # Create result list with the "statistics" class
  result <- list(
    min = min_data,
    max = max_data,
    median = median_data,
    skewness_squared = skewness_squared,
    kurtosis = kurtosis_data
  )

  class(result) <- "statistics"

  return(result)
}

#' Print Method for Statistics Object
#'
#' This function prints the calculated statistics (minimum, maximum, median, skewness squared, and kurtosis) for a numeric dataset.
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
  cat("Median: ", x$median, "\n")
  cat("Skewness Squared: ", x$skewness_squared, "\n")
  cat("Kurtosis: ", x$kurtosis, "\n")
}
