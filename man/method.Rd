% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_function.R
\name{method}
\alias{method}
\title{Dispatch Method for Statistical Calculation}
\usage{
method(data)
}
\arguments{
\item{data}{A numeric vector or data for which skewness and kurtosis are calculated.}
}
\value{
A list containing skewness and kurtosis.
}
\description{
This function dispatches the calculation of skewness and kurtosis based on the class of the input data.
The method can be 'sample' or 'unbiased'.
}
