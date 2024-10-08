#' Generic Function for Data Type Processing
#'
#' @param data A numeric vector or list of numeric vectors representing the dataset to process.
#' @param method A string indicating whether to use "sample" or "unbiased" method for calculating statistics.
#'
#' @return A list containing the filtered data, theoretical points, and distribution lines for plotting.
#' @export
data_type <- function(data, method) {
  UseMethod("data_type")
}

#' Method for Continuous Data
#'
#' @param data A numeric vector containing the continuous dataset to process.
#' @param method A string indicating whether to use "sample" or "unbiased" method for calculating statistics.
#'
#' @return A list of filtered data, theoretical points, and distribution lines.
#' @export
data_type.continuous <- function(data, method) {
  # Calculate basic statistics for the data (min, max, skewness, kurtosis, etc.)
  stats_test <- calculate_statistics(data, method = method)

  # Set the maximum values for the skewness and kurtosis on the plot
  xmax <- max(4, ceiling(stats_test$skewness_squared))
  ymax <- max(10, ceiling(stats_test$kurtosis))

  # Create the Beta distribution polygon data based on theoretical calculations
  lq <- seq(-100, 100, 0.1)
  s2a <- (4 * (exp(lq) - exp(-100)) ^ 2 * (exp(-100) + exp(lq) + 1)) / ((exp(-100) + exp(lq) + 2) ^ 2 * exp(-100) * exp(lq))
  ya <- (3 * (exp(-100) + exp(lq) + 1) * (exp(-100) * exp(lq) * (exp(-100) + exp(lq) - 6) + 2 * (exp(-100) + exp(lq))^2)) /
    (exp(-100) * exp(lq) * (exp(-100) + exp(lq) + 2) * (exp(-100) + exp(lq) + 3))
  s2b <- (4 * (exp(lq) - exp(10)) ^ 2 * (exp(10) + exp(lq) + 1)) / ((exp(10) + exp(lq) + 2) ^ 2 * exp(10) * exp(lq))
  yb <- (3 * (exp(10) + exp(lq) + 1) * (exp(10) * exp(lq) * (exp(10) + exp(lq) - 6) + 2 * (exp(10) + exp(lq))^2)) /
    (exp(10) * exp(lq) * (exp(10) + exp(lq) + 2) * (exp(10) + exp(lq) + 3))

  # Combine data for Beta distribution polygon into a single dataframe
  polygon_data <- data.frame(s2 = c(s2a, s2b), y = c(ya, yb))

  # Theoretical points for observed, normal, uniform, logistic, and exponential distributions
  theoretical_points <- data.frame(
    skewness_squared = c(stats_test$skewness_squared, 0, 0, 0, 2^2),
    kurtosis = c(stats_test$kurtosis, 3, 1.8, 4.2, 9),
    label = c("Observed", "Normal", "Uniform", "Logistic", "Exponential"),
    shape = c(1, 8, 2, 3, 7),
    color = c("blue", "red", "black", "green", "purple")
  )

  # Generate gamma and lognormal lines for the plot
  skewness_sq_values <- seq(0, 4.5, length.out = 100)
  kurtosis_gamma_values <- (3 / 2) * skewness_sq_values + 3
  gamma_line <- data.frame(skewness_squared = skewness_sq_values, kurtosis = kurtosis_gamma_values)

  # Lognormal line calculation
  es2 <- exp(exp(seq(-2, 2, length.out = 100))^2)
  s2_line <- (es2 + 2)^2 * (es2 - 1)
  y_line <- es2^4 + 2 * es2^3 + 3 * es2^2 - 3
  lognormal_line <- data.frame(skewness_squared = s2_line[s2_line <= xmax], kurtosis = y_line[s2_line <= xmax])
  polygon_data$distribution <- "Beta Distribution"

  # Return a list of the filtered data, theoretical points, and distribution lines for plotting
  return(
    list(
      polygon_data = polygon_data,
      theoretical_points = theoretical_points,
      gamma_line = gamma_line,
      lognormal_line = lognormal_line
    )
  )
}

#' Method for Discrete Data
#'
#' @param data A numeric vector containing the discrete dataset to process.
#' @param method A string indicating whether to use "sample" or "unbiased" method for calculating statistics.
#'
#' @return A list of filtered data, theoretical points, and distribution lines.
#' @export
data_type.discrete <- function(data, method) {
  # Calculate basic statistics for the discrete data (min, max, skewness, kurtosis, etc.)
  stats_test <- calculate_statistics(data, method = method)

  # Set the maximum values for the skewness and kurtosis on the plot
  xmax <- max(4, ceiling(stats_test$skewness_squared))
  ymax <- max(10, ceiling(stats_test$kurtosis))

  # Theoretical points for observed and normal distributions
  theoretical_points <- data.frame(
    skewness_squared = c(stats_test$skewness_squared, 0),
    kurtosis = c(stats_test$kurtosis, 3),
    label = c("Observed", "Normal"),
    shape = c(1, 8),
    color = c("blue", "red")
  )

  # Generate data for the Negative Binomial (NegBin) distribution
  lr <- seq(-100, 100, 0.1)
  s2a <- (2 - exp(-10))^2 / (exp(lr) * (1 - exp(-10)))
  ya <- (3 + 6 / exp(lr) + exp(-10)^2 / (exp(lr) * (1 - exp(-10))))

  lr <- seq(100, -100, -0.1)
  s2b <- (2 - (1 - exp(-10)))^2 / (exp(lr) * (1 - (1 - exp(-10))))
  yb <- (3 + 6 / exp(lr) + (1 - exp(-10))^2 / (exp(lr) * (1 - (1 - exp(-10)))))

  # Combine data for the Negative Binomial polygon into a single dataframe
  polygon_data <- data.frame(s2 = c(s2a, s2b), y = c(ya, yb))

  # Generate the Poisson distribution line
  lambda <- exp(seq(-100, 100, 0.1))
  s2_line <- 1 / lambda
  y_line <- (3 + 1 / lambda)
  poisson_line <- data.frame(skewness_squared = s2_line[s2_line <= xmax], kurtosis = y_line[s2_line <= xmax])
  polygon_data$distribution <- "NegBin Distribution"

  # Return a list of the filtered data, theoretical points, and Poisson line for plotting
  return(
    list(
      polygon_data = polygon_data,
      theoretical_points = theoretical_points,
      poisson_line = poisson_line
    )
  )
}

