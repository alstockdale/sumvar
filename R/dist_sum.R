#' Explore a continuous variable, stratified by a grouping variable
#'
#' @description
#' Summarises the median, interquartile range, mean, standard deviation, confidence intervals of the mean and produces a density plot, stratified by a second grouping variable.
#'
#' Provides frequentist hypothesis tests for comparison between the groups: T test and Wilcoxon rank sum for 2 groups,
#' Anova and Kruskall wallis test for 3 or more groups.
#'
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#'
#' @param data The data frame or tibble
#' @param var The variable you would like to summarise
#' @param by The grouping variable
#'
#' @return A tibble with a summary of the variable frequency (n), number of missing observations (n_miss), median,
#' interquartile range, mean, SD, 95% confidence intervals of the mean (using the Z distribution), and density plots.
#'
#' Shows the T test (p_ttest) and Wilcoxon rank sum (p_wilcox) hypothesis tests when there are two groups
#' And an Anova test (p_anova) and Kruskal-Wallis test (p_kruskal) when there are three or more groups.
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull distinct as_tibble if_else arrange group_by summarize mutate
#'   enquo bind_rows row_number
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_violin
#'   geom_histogram geom_density labs theme_minimal scale_color_discrete scale_fill_discrete
#' @importFrom stats median qt quantile sd qnorm aov as.formula chisq.test
#'   fisher.test kruskal.test t.test wilcox.test
#' @importFrom rlang enquos as_label ':='
#' @importFrom ggbeeswarm geom_beeswarm
#'
#' @examples
#' example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10),
#'                               group = sample(c("a", "b", "c", "d"),
#'                               size = 100, replace = TRUE))
#' dist_sum(example_data, age, group)
#' example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10),
#'                              sex = sample(c("male", "female"),
#'                              size = 100, replace = TRUE))
#' dist_sum(example_data, age, sex)
#' summary <- dist_sum(example_data, age, sex) # Save summary statistics as a tibble.
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% dist_sum(age, group)
#' @export
dist_sum <- function(data, var, by = NULL) {
  # Check if grouping variable is missing
  if (missing(by)) {
    # Access the specified column as a numeric vector using curly-curly
    values <- data %>% dplyr::pull({{ var }})

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

    # Create a histogram and density plot
    plot_data <- tibble(values = values)
    histo_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = values)) +
      ggplot2::geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) +
      ggplot2::labs(title = "Histogram", x = deparse(substitute(var)), y = "Density") +
      ggplot2::theme_minimal()
    density_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = values)) +
      ggplot2::geom_density(color = "black", alpha = 0.7) +
      ggplot2::labs(title = "Density Plot", x = deparse(substitute(var)), y = "Density") +
      ggplot2::theme_minimal()

    print(histo_plot + density_plot)  # Print the combined plot

    # Return a tibble with the results
    return(tibble(
      n = n,
      n_miss = n_missing,
      median = median_value,
      p25 = p25_value,
      p75 = p75_value,
      mean = mean_value,
      sd = sd_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      min = min_value,
      max = max_value
    ))
  } else {
    # Grouped summary statistics
    var <- rlang::enquo(var)
    by <- rlang::enquo(by)
    summary_data <- data %>%
      dplyr::group_by(!!by) %>%
      dplyr::summarize(
        n = sum(!is.na(!!var)),
        n_miss = sum(is.na(!!var)),
        median = median(!!var, na.rm = TRUE),
        p25 = quantile(!!var, 0.25, na.rm = TRUE),
        p75 = quantile(!!var, 0.75, na.rm = TRUE),
        mean = mean(!!var, na.rm = TRUE),
        sd = sd(!!var, na.rm = TRUE),
        min = min(!!var, na.rm = TRUE),
        max = max(!!var, na.rm = TRUE),
        ci_lower = mean(!!var, na.rm = TRUE) - (qnorm(0.975) * (sd(!!var, na.rm = TRUE) / sqrt(n))),
        ci_upper = mean(!!var, na.rm = TRUE) + (qnorm(0.975) * (sd(!!var, na.rm = TRUE) / sqrt(n))),
        .groups = 'drop'
      )

    # Density plot by group
    density_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!var, color = as.factor(!!by), fill = as.factor(!!by))) +
      ggplot2::geom_density(alpha = 0.3) +
      ggplot2::labs(title = "Density Plot by Group", x = rlang::as_label(var), y = "Density") +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_discrete(name = rlang::as_label(by)) +
      ggplot2::scale_color_discrete(name = rlang::as_label(by))

    print(density_plot)

    # Calculate the number of unique groups
    num_groups <- dplyr::n_distinct(data %>% dplyr::pull(!!by))

    # Add hypothesis tests based on the number of groups
    if (num_groups == 2) {
      # Perform t-test and Wilcoxon rank-sum test
      t_test_result <- t.test(as.formula(paste(rlang::as_label(var), "~", rlang::as_label(by))), data = data)
      wilcox_test_result <- wilcox.test(as.formula(paste(rlang::as_label(var), "~", rlang::as_label(by))), data = data)

      summary_data <- summary_data %>%
        dplyr::mutate(
          p_ttest = ifelse(row_number() == 1, t_test_result$p.value, NA_real_),
          p_wilcox = ifelse(row_number() == 1, wilcox_test_result$p.value, NA_real_)
        )

    } else if (num_groups > 2) {
      # Perform ANOVA and Kruskal-Wallis test
      aov_result <- aov(as.formula(paste(rlang::as_label(var), "~", rlang::as_label(by))), data = data)
      p_anova <- summary(aov_result)[[1]]$`Pr(>F)`[1]

      kruskal_result <- kruskal.test(as.formula(paste(rlang::as_label(var), "~", rlang::as_label(by))), data = data)
      p_kruskal <- kruskal_result$p.value

      summary_data <- summary_data %>%
        dplyr::mutate(
          p_anova = ifelse(row_number() == 1, p_anova, NA_real_),
          p_kruskal = ifelse(row_number() == 1, p_kruskal, NA_real_)
        )
    } else {
      # If there are fewer than 2 groups, set p-values to NA
      summary_data <- summary_data %>%
        dplyr::mutate(
          p_ttest = NA_real_,
          p_wilcox = NA_real_,
          p_anova = NA_real_,
          p_kruskal = NA_real_
        )
    }

    # Return the summary data with hypothesis test results
    return(summary_data)
  }
}
