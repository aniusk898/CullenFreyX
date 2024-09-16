#' Launch the Cullen-Frey Graph Shiny Application
#'
#' This function calculates key statistics (skewness, kurtosis, etc.) for a given dataset and
#' prepares data for the Cullen-Frey graph. It then launches a Shiny application that
#' provides an interactive visualization of the Cullen-Frey graph based on the calculated statistics.
#'
#' @param data A numeric vector representing the dataset to be analyzed.
#' The dataset must contain at least 4 data points.
#'
#' @return This function does not return any value. It launches the Shiny application
#' displaying the Cullen-Frey graph.
#' @export
#'
#' @examples
#' data <- rnorm(100)
#' launch_cullen_frey_app(data)
launch_cullen_frey_app <- function(data) {

  # Calculate statistics for the provided data
  stats_test <- calculate_statistics(data)

  kurtmax <- max(10, ceiling(stats_test$kurtosis))
  xmax <- max(4, ceiling(stats_test$skewness_squared))
  ymax <- kurtmax - 1

  calc_s2_y <- function(p) {
    lq <- seq(-100, 100, 0.1)
    q <- exp(lq)
    s2 <- (4 * (q - p)^2 * (p + q + 1)) / ((p + q + 2)^2 * p * q)
    y <- (3 * (p + q + 1) * (p * q * (p + q - 6) + 2 * (p + q)^2)) /
      (p * q * (p + q + 2) * (p + q + 3))
    return(list(s2 = s2, y = y))
  }

  results_a <- calc_s2_y(exp(-100))
  results_b <- calc_s2_y(exp(100))

  s2 <- c(results_a$s2, results_b$s2)
  y <- c(results_a$y, results_b$y)

  polygon_data <- data.frame(s2 = s2, y = y)
  data_filtered <- subset(polygon_data, s2 >= 0 & s2 <= 5 & y >= 0 & y <= 10)

  theoretical_points <- data.frame(
    skewness_squared = c(stats_test$skewness_squared, 0, 0, 0, 2^2),
    kurtosis = c(stats_test$kurtosis, 3, 1.8, 4.2, 9),
    label = c("Observed", "Normal", "Uniform", "Logistic", "Exponential"),
    shape = c(1, 8, 2, 3, 7),
    color = c("blue", "red", "black", "green", "purple")
  )

  skewness_sq_values <- seq(0, 4.5, length.out = 100)
  kurtosis_gamma_values <- (3 / 2) * skewness_sq_values + 3
  gamma_line <- data.frame(skewness_squared = skewness_sq_values, kurtosis = kurtosis_gamma_values)

  lshape <- seq(-2, 2, length.out = 100)
  shape <- exp(lshape)
  es2 <- exp(shape^2)
  s2_line <- (es2 + 2)^2 * (es2 - 1)
  y_line <- es2^4 + 2 * es2^3 + 3 * es2^2 - 3
  lognormal_line <- data.frame(skewness_squared = s2_line[s2_line <= xmax], kurtosis = y_line[s2_line <= xmax])

  gamma_line <- subset(gamma_line, skewness_squared <= xmax & kurtosis <= ymax)
  lognormal_line <- subset(lognormal_line, skewness_squared <= xmax & kurtosis <= ymax)
  data_filtered$distribution <- "Beta Distribution"

  # Launch the Shiny app from the app_definition.R script
  run_app()
}


