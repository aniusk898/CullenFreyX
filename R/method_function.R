#' Dispatch Method for Statistical Calculation
#'
#' This is a generic function for calculating statistical measures such as skewness and kurtosis.
#' It dispatches to specific methods like \code{method.sample} or \code{method.unbiased}, depending on the class of the input data.
#'
#' @param data A numeric vector for which skewness and kurtosis will be calculated.
#'
#' @return The function returns a list containing the calculated skewness and kurtosis for the given data, depending on the method used.
#' @export
#'
#' @examples
#' data <- rnorm(100)
#' result <- method(data)
method <- function(data) {
  UseMethod("method")
}

#' Sample Method for Skewness and Kurtosis Calculation
#'
#' This function calculates the skewness and kurtosis for a given dataset using the sample method.
#' The skewness and kurtosis are computed based on moments of the data, with kurtosis representing the Fisher kurtosis (excess kurtosis).
#'
#' @param data A numeric vector for which the skewness and kurtosis will be calculated.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{skewness}{The skewness of the data.}
#'   \item{kurtosis}{The kurtosis (excess kurtosis) of the data.}
#' }
#' @export
#'
#' @examples
#' data <- rnorm(100)
#' result <- method.sample(data)
#' print(result)
method.sample <- function(data) {

  # Use the moment function defined earlier
  skewness <- function(data) {
    sd <- sqrt(moment(data, 2))
    return(moment(data, 3) / sd^3)
  }

  kurtosis <- function(data) {
    var <- moment(data, 2)
    return(moment(data, 4) / var^2)
  }

  return(list(skewness = skewness(data), kurtosis = kurtosis(data)))
}


#' Unbiased Method for Skewness and Kurtosis Calculation
#'
#' This function calculates skewness and kurtosis for a given dataset using the unbiased method.
#' The skewness and kurtosis are computed using unbiased estimators, adjusting for sample size.
#'
#' @param data A numeric vector for which the unbiased skewness and kurtosis will be calculated.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{skewness}{The unbiased skewness of the data.}
#'   \item{kurtosis}{The unbiased kurtosis of the data.}
#' }
#' @export
#'
#' @examples
#' data <- rnorm(100)
#' result <- method.unbiased(data)
#' print(result)
method.unbiased <- function(data) {

  # Use the moment function defined earlier
  skewness <- function(data) {
    sd <- sqrt(moment(data, 2))
    n <- length(data)
    gamma1 <- moment(data, 3) / sd^3
    unbiased_skewness <- sqrt(n * (n - 1)) * gamma1 / (n - 2)
    return(unbiased_skewness)
  }

  kurtosis <- function(data) {
    n <- length(data)
    var <- moment(data, 2)
    gamma2 <- moment(data, 4) / var^2
    unbiased_kurtosis <- (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * gamma2 - 3 * (n - 1)) + 3
    return(unbiased_kurtosis)
  }

  return(list(skewness = skewness(data), kurtosis = kurtosis(data)))
}
