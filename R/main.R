utils::globalVariables(
  c(
    "skewness_squared",
    'label',
    "squared_skewness",
    "kurtosis",
    "data_filtered",
    "s2",
    "y",
    "theoretical_points",
    "lognormal_line",
    "gamma_line",
    "xmax",
    "ymax"
  )
)
#' Run the Cullen-Frey Interactive Graph Application
#'
#' This function launches a Shiny web application that allows users to interactively explore
#' the Cullen-Frey graph. It requires preprocessed data inputs for plotting, which should be
#' provided as arguments to the function.
#'
#' @param data A numeric vector, list or dataframe that contains the datasets to be analyzed.
#' @return Launches the Shiny web application.
#' @export
launch_cullen_frey_app <- function(data) {
    if (!is.vector(data) && !is.list(data) && !is.data.frame(data)) {
        stop("Input must be a list, vector, or dataframe.")
    }

    process_input_data <- function(data) {
        # Check if the data is a vector (but not a list)
        if (is.vector(data) && !is.list(data)) {
            # Convert single vector to list with one named element
            data <- list(data)
            names(data) <- "dataset1"
        }

        # Check if data is a list
        if (is.list(data)) {
            # If names are null, initialize with empty names
            if (is.null(names(data))) {
                names(data) <- rep("", length(data))
            }

            # Identify unnamed elements
            unnamed_elements <- names(data) == ""

            # Assign names "dataset1", "dataset2", etc., to unnamed elements
            names(data)[unnamed_elements] <- paste0("dataset", seq_along(data)[unnamed_elements])
        }

        # Return processed data
        return(data)
    }

    # Process the input data
    processed_data <- process_input_data(data)
    # Call the existing app function with the processed data
    run_app(processed_data)
}

