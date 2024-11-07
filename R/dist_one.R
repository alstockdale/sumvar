#' Examine the distribution of a single continous variable
#' @description
#' Summarises the median, interquartile range, mean, standard deviation, confidence intervals of the mean and plots the density and a histogram. Default option for a density plot and histogram can be overriden with the options "bee" and "box".
#'
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#'
#' @param data The data frame or tibble
#' @param ... The variable you would like to summarise
#' - Default, with no additional arguments: density plot and histogram
#' - Add the option "box" to show a boxplot and violin plot eg. data %>% dist_one(var, box)
#' - Add the option "bee" to show a beeswarm plot eg. data %>% dist_one(var, bee)
#'
#' @return A tibble with a summary of the variable median, interquartile range, mean, SD, confidence intervals of the mean (using the Z distribution), and plots.
#'
#' @import patchwork
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_violin
#'   geom_histogram geom_density labs theme_minimal
#' @importFrom stats median qt quantile sd qnorm
#' @importFrom rlang enquos
#' @importFrom ggbeeswarm geom_beeswarm
#' @examples
#' example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10))
#' dist_one(example_data, age) # Default, with histogram and density plot
#' dist_one(example_data, age, box) # Option to show a boxplot and violin plot
#' dist_one(example_data, age, bee) # Option to show a beeswarm plot (using ggbeeswarm)
#' summary <- dist_one(example_data, age) # Save summary statistics as a tibble.
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% dist_one(age)
#' @export
dist_one <- function(data, ...) {
  # Capture arguments and check if "box" or "bee" is passed
  args <- rlang::enquos(...)
  plot_type <- if ("box" %in% sapply(args, rlang::as_label)) {
    "box"
  } else if ("bee" %in% sapply(args, rlang::as_label)) {
    "bee"
  } else {
    "hist"
  }

  # Filter out "box" and "bee" arguments if present
  variables <- args[!sapply(args, rlang::as_label) %in% c("box", "bee")]

  # Check that exactly one variable is provided
  if (length(variables) != 1) {
    stop("Please enter only one variable to summarize")
  }

  variable <- substitute(...)
  variable_name <- deparse(variable)

  # Access the specified column as a numeric vector
  values <- data %>% dplyr::pull(!!variables[[1]])

  # Calculate summary statistics
  median_value <- median(values, na.rm = TRUE)
  p25_value <- quantile(values, 0.25, na.rm = TRUE)
  p75_value <- quantile(values, 0.75, na.rm = TRUE)
  mean_value <- mean(values, na.rm = TRUE)
  min_value <- min(values, na.rm = TRUE)
  max_value <- max(values, na.rm = TRUE)
  n <- sum(!is.na(values))
  n_missing <- sum(is.na(values))
  sd_value <- sd(values, na.rm = TRUE)
  z_value <- qnorm(0.975)
  margin_error <- z_value * (sd_value / sqrt(n))
  ci_lower <- mean_value - margin_error
  ci_upper <- mean_value + margin_error
  plot_data <- dplyr::tibble(values = values)

  # Create plots based on the selected plot type
  if (plot_type == "box") {
    # Boxplot and Violin plot
    box_plot <- ggplot(data = plot_data, aes(x = "", y = values)) +
      geom_boxplot(color = "black", alpha = 0.7) +
      labs(title = "Boxplot", y = variable_name, x = "") +
      theme_minimal()

    violin_plot <- ggplot(data = plot_data, aes(x = "", y = values)) +
      geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Violin Plot", y = variable_name, x = "") +
      theme_minimal()

    plot <- box_plot + violin_plot
  } else if (plot_type == "bee") {
    # Beeswarm plot
    beeswarm_plot <- ggplot(data = plot_data, aes(x = "", y = values)) +
      geom_beeswarm(color = "steelblue", alpha = 0.7) +
      labs(title = "Beeswarm Plot", y = variable_name, x = "") +
      theme_minimal()

    plot <- beeswarm_plot
  } else {
    # Histogram and Density plot
    histo_plot <- ggplot(data = plot_data, aes(x = values)) +
      geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram", x = deparse(substitute(variable)), y = "Density") +
      theme_minimal()

    density_plot <- ggplot(data = plot_data, aes(x = values)) +
      geom_density(color = "black", alpha = 0.7) +
      labs(title = "Density Plot", x = deparse(substitute(variable)), y = "Density") +
      theme_minimal()

    plot <- histo_plot + density_plot
  }

  print(plot)  # Print the plot

  # Return a tibble with the results
  tibble(
    n = n,
    median = median_value,
    p25 = p25_value,
    p75 = p75_value,
    mean = mean_value,
    sd = sd_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    min = min_value,
    max = max_value
  )
}
