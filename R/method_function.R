#' Dispatch Method for Statistical Calculation
#'
#' This function dispatches the calculation of skewness and kurtosis based on the class of the input data.
#' The method can be 'sample' or 'unbiased'.
#'
#' @param data A numeric vector or data for which skewness and kurtosis are calculated.
#' @return A list containing skewness and kurtosis.
#' @export
method <- function(data) {
  UseMethod("method")
}


#' Sample Method for Calculating Skewness and Kurtosis
#'
#' This function calculates skewness and kurtosis for a numeric dataset using the 'sample' method.
#'
#' @param data A numeric vector for which skewness and kurtosis will be calculated.
#' @return A list containing skewness and kurtosis.
#' @export
method.sample <- function(data) {
  if (!is.numeric(data))
    stop("Data must be numeric.")

  skewness <- function(data) {
    sd <- sqrt(moment(data, 2))
    return(moment(data, 3) / sd ^ 3)
  }

  kurtosis <- function(data) {
    var <- moment(data, 2)
    return(moment(data, 4) / var ^ 2)
  }

  return(list(skewness = skewness(data), kurtosis = kurtosis(data)))
}


#' Unbiased Method for Calculating Skewness and Kurtosis
#'
#' This function calculates skewness and kurtosis for a numeric dataset using the 'unbiased' method.
#'
#' @param data A numeric vector for which skewness and kurtosis will be calculated.
#' @return A list containing skewness and kurtosis.
#' @export
method.unbiased <- function(data) {
  if (!is.numeric(data))
    stop("Data must be numeric.")

  skewness <- function(data) {
    n <- length(data)
    sd <- sqrt(moment(data, 2))
    gamma1 <- moment(data, 3) / sd ^ 3
    return(sqrt(n * (n - 1)) * gamma1 / (n - 2))
  }

  kurtosis <- function(data) {
    n <- length(data)
    var <- moment(data, 2)
    gamma2 <- moment(data, 4) / var ^ 2
    return((n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * gamma2 - 3 * (n - 1)) + 3)
  }

  return(list(skewness = skewness(data), kurtosis = kurtosis(data)))
}
