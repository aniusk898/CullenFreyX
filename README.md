# CullenFreyX

CullenFreyX is an R package designed to create **interactive Cullen-Frey plots** for analyzing data distribution characteristics, specifically skewness and kurtosis. Unlike traditional static plots, CullenFreyX allows users to explore distributions dynamically, making it an ideal tool for data analysts and statisticians working with complex data.

## Features

- **Interactive Cullen-Frey Plot**: Adjust parameters in real time for enhanced data exploration.
- **Multiple Data Types**: Supports vectors, lists, matrices, and data frames.
- **Enhanced Performance**: Integrates `Rcpp` for efficient bootstrapping and faster calculations with large datasets.
- **Customizable Visuals**: Offers options to modify colors, labels, and plot aesthetics.
- **Comparison with Other Tools**: CullenFreyX improves upon static tools like `fitdistrplus` by adding interactivity and flexibility.

## Installation
You can install the development version of the package from GitHub using:


```{r, eval=FALSE}
devtools::install_github("https://github.com/aniusk898/CullenFreyX")
```
## Cullen and Frey Plot
The Cullen-Frey plot is a visual tool based on concepts and techniques developed by Cullen and Frey in their 1999 work [@CullenFrey1999]. This plot is used in statistical analysis to identify the appropriate distribution of a dataset by comparing its measurements (skewness and kurtosis) with the known characteristics of theoretical distributions. Skewness measures the symmetry of the distribution: positive skewness indicates that the right tail is heavier than the left tail, while negative skewness indicates the opposite. Kurtosis measures the ``sharpness" of the distribution: high kurtosis indicates heavier tails and higher peaks, while low kurtosis indicates a flatter distribution with lighter tails.

## Objectives
The primary objective is to develop an R package that creates an interactive, customizable, and exportable Cullen-Frey plot. 

## User expectations
The idea is that the proposed package will provide efficient and user-friendly implementations on Cullen-Frey plot allowing import multiple samples, export and customize the plot, and also explore relevant information with a simple click.

## cullenfrey_x Function 
```{r}
data_frame_example <- data.frame(
  X = c(1, 2, 3, 4),
  Y = c(4, 5, 6, 7)
)
cullenfrey_x(data_frame_example)
```
This will launch the Cullen-Frey interactive graph application in a new window. In this app, you can interact with datasets, adjust graph settings, and customize the appearance of the graph. Once the app is running, you will be able to:

- **Select a Dataset**: Choose datasets from a dropdown menu that automatically populates based on the data input.
- **Select Data Type**: Toggle between continuous or discrete distributions, depending on your dataset.
- **Select Method Type**: Toggle between sample or ubiased calculations, depends on the user preferences.
- **Bootstrap Sampling Options**: Choose from different bootstrap methods (or none) and specify the number of samples.
- **Customize Distribution Colors**: Use the colourInput feature or select from pre-defined colorblind-friendly palettes to change the colors of the displayed distributions.
- **Insert Comments**: Add custom comments for each distribution, which will be displayed on hover in the graph.
- **Adjust Display Settings**: You can dynamically adjust the point size, transparency (alpha), and font size for the plot.
- **Enable/Disable Distributions**: Toggle the visibility of specific distributions to focus on certain types of data or analysis.
- **Download Graph**: Export the graph as a PNG image or download a PDF report containing the graph, statistical summaries, and any added comments, making it easy to share insights and results.

# Distribution and Aesthetic Controls Panel

- This panel provides controls for customizing the appearance and behavior of the plot:
  - **Select a Continuous Distribution**: A dropdown allows users to choose which distribution they want to analyze or compare.
  - **Choose Font Family**: A dropdown to select the font style used in the plot's labels.
  - **Insert a Comment**: A text input where users can add comments to their plot for reference.
  - **Choose Distribution Color**: A color picker to change the color assigned to the selected distribution.
  - **Colorblind Friendly Palette**: A set of color buttons that provide preset color options for colorblind users.
  - **Point Size**: A slider to adjust the size of the data points in the plot.
  - **Point Transparency**: A slider to control the transparency of the points plotted in the Cullen-Frey graph.
  - **Font Size**: A slider to adjust the font size used in the plot.
  - **Text Size**: A slider to adjust the text size for the entire app interface.
  - **Save Changes**: A button to save the chosen settings and apply them to the plot.

# Interactive Plot Display Panel

- This central panel displays the Cullen-Frey graph, which includes the observed distribution plotted alongside theoretical distributions (Normal, Uniform, Logistic, Exponential, etc.). 
- The plot has a labeled legend on the right-hand side, indicating each distribution's name and corresponding color.
- Users can visually compare the skewness and kurtosis of the observed data against theoretical distributions.
- The plot responds dynamically based on user-selected data, color, point size, and transparency.
# Examples of usage
```{r}
# Numeric vector
vector_example <- c(1, 2, 3, 4, 5, 6)

# Numeric matrix
data_matrix <- matrix(rnorm(20), nrow = 10, ncol = 2)

# Matrix (which can be treated as a list of columns)
data_matrix <- matrix(rnorm(20), nrow = 10, ncol = 2)

# Unnamed list of numeric vectors
data_unnamed <- list(rpois(100, lambda = 5), rexp(100))
```

```{r}
set.seed(10)

#Generate Continuous Distributions
normal_data <- rnorm(100, mean = 0, sd = 1)
uniform_data <- runif(1000, min = 0, max = 1)
logistic_data <- rlogis(1000, location = 0, scale = 1)
gamma_data <- rgamma(10000, shape = 2, rate = 1)
exp_data <- rexp(100000, rate = 1)
lognormal_data <- rlnorm(100000, meanlog = 0, sdlog = 0.5)
beta_data <- rbeta(1000, shape1 = 2, shape2 = 5)


continuous_data_list <- list(
  normal = normal_data,
  uniform = uniform_data,
  logistic = logistic_data,
  gamma = gamma_data,
  exponential = exp_data,
  lognormal = lognormal_data,
  beta = beta_data
)

#cullenfrey_x(continuous_data_list)
```

```{r}
set.seed(10)
# Generate Discerete Distributions
poisson_data <- rpois(1000, lambda = 3)
negative_binomial_data <- rnbinom(100000, size = 10, prob = 0.5)

discrete_data_list <- list(
  poisson = poisson_data,
  negative_binomial = negative_binomial_data
)
#cullenfrey_x(discrete_data_list)
```

```{r}
set.seed(10)

data_small <- rnorm(10)  # Small dataset with 10 points
data_medium <- rnorm(1000)  # Medium dataset with 1000 points
data_large <- rnorm(50000)  # Large dataset with 10000 points
data_very_large <- rnorm(1e6)  # Very large dataset with 1 million points

list_of_datasets_varied <- list(
  Small = data_small,
  Medium = data_medium,
  Large = data_large,
  Very_Large = data_very_large
)
#cullenfrey_x(list_of_datasets_varied)
```


# Common Errors and Solutions
```{r}
# Vector with fewer than 4 points (insufficient for skewness and kurtosis calculations)
small_data_example <- c(1, 2, 3) # Expect an error indicating a minimum of 4 data points is required.

# Data frame with mixed types (unsupported in numeric calculations)
mixed_data_example <- data.frame(
  X = c(1, 2, 3, 4),
  Y = c("a", "b", "c", "d")
)
# This will raise an error, as the package expects purely numeric data.

# Data with NA values (unsupported due to missing values)
data_with_NA <- c(1, 2, NA, 4, 5)
# This will result in an error indicating NA values are not allowed.

# Data with malformed values (e.g., a string where a number is expected)
data_malformed <- c(1, "o.5", 3, 4)
# This raises an error, as "o.5" is not interpretable as numeric.

# Data with Inf values (unsupported as Inf values break statistical calculations)
data_with_Inf <- c(1, 2, Inf, 4, 5)
# This will throw an error indicating Inf values are not allowed.

# Empty data frame (does not allow statistical calculations, though the app might load)
data_empty <- data.frame()
# This will likely show the app but without any data to analyze or display in the distributions.

# Unsupported data scenarios to illustrate errors in `cullenfrey_x()`
# Run these tests individually to ensure error handling is functional and informative.

```

# License
CullenFreyX is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more details.

# Acknowledgments
CullenFreyX is inspired by the Cullen-Frey plot concept from the [`fitdistrplus`](https://cran.r-project.org/package=fitdistrplus) package.

# Support
For support or questions, feel free to reach out via email at [aniusk898@gmail.com](mailto:aniusk898@gmail.com).

