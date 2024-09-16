#include <Rcpp.h>
using namespace Rcpp;

// Bootstrap Method for Skewness and Kurtosis Calculation
//
// This function performs bootstrapping on a dataset to calculate the skewness and kurtosis
// for each bootstrap sample. The method can be either 'sample' or 'unbiased', determining how
// the skewness and kurtosis are computed.
//
// @param data A numeric vector representing the dataset to bootstrap.
// @param boot_samples An integer specifying the number of bootstrap samples.
// @param method A character string specifying the method to calculate skewness and kurtosis.
// Must be either 'sample' or 'unbiased'.
// @param replace A logical indicating whether sampling should be with replacement. Defaults to \code{TRUE}.
//
// @return A list with two components:
// \describe{
//   \item{skewness}{A numeric vector of skewness values for each bootstrap sample.}
//   \item{kurtosis}{A numeric vector of kurtosis values for each bootstrap sample.}
// }
// @export
//
// @examples
// data <- rnorm(100)
// result <- bootstrap_method(data, boot_samples = 1000, method = "sample")
// print(result$skewness)
// print(result$kurtosis)
// [[Rcpp::export]]
List bootstrap_method(NumericVector data, int boot_samples, std::string method, bool replace = true) {
  int n = data.size();

  // Create vectors to store bootstrap results
  NumericVector skewness_boot(boot_samples);
  NumericVector kurtosis_boot(boot_samples);

  // Reference to R function "method"
  Rcpp::Function method_r("method");

  for (int i = 0; i < boot_samples; i++) {
    // Generate bootstrap sample
    NumericVector bootstrap_sample = Rcpp::sample(data, n, replace);

    // Assign class to the bootstrap sample based on the method
    if (method == "sample") {
      bootstrap_sample.attr("class") = "sample";
    } else if (method == "unbiased") {
      bootstrap_sample.attr("class") = "unbiased";
    } else {
      stop("The method must be 'sample' or 'unbiased'.");
    }

    // Call the R function "method" for each bootstrap sample
    List result = method_r(bootstrap_sample);

    // Extract skewness and kurtosis from the result
    skewness_boot[i] = as<double>(result["skewness"]);
    kurtosis_boot[i] = as<double>(result["kurtosis"]);
  }

  // Return skewness and kurtosis results
  return List::create(Named("skewness") = skewness_boot,
                      Named("kurtosis") = kurtosis_boot);
}

