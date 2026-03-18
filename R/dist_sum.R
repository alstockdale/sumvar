#' Explore a continuous variable
#'
#' @description
#' Summarises a continuous variable, returning a tibble of descriptive statistics and a plot.
#' When a grouping variable is supplied, results are stratified by group.
#'
#' For two groups, a t-test and Wilcoxon rank-sum test are reported. For three or more groups,
#' a one-way ANOVA and Kruskal-Wallis test are reported.
#'
#' @param data The data frame or tibble
#' @param var The continuous variable to summarise
#' @param by Optional grouping variable
#'
#' @return A tibble with one row per group (or one row when ungrouped) containing
#' the following columns:
#' \describe{
#'   \item{\code{n}}{Number of non-missing observations.}
#'   \item{\code{n_miss}}{Number of missing (NA) values.}
#'   \item{\code{median}}{Median value.}
#'   \item{\code{p25}, \code{p75}}{25th and 75th percentiles (interquartile range boundaries).}
#'   \item{\code{mean}}{Arithmetic mean.}
#'   \item{\code{sd}}{Standard deviation.}
#'   \item{\code{ci_lower}, \code{ci_upper}}{Lower and upper bounds of the 95% confidence
#'     interval for the mean. Uses the t-distribution when n < 30, and the
#'     Z-distribution when n >= 30.}
#'   \item{\code{min}, \code{max}}{Minimum and maximum observed values.}
#'   \item{\code{n_outliers}}{Count of values more than 1.5 x IQR below Q1 or above Q3
#'     (Tukey fence method).}
#'   \item{\code{shapiro_p}}{P-value from the Shapiro-Wilk test of normality. Returns
#'     \code{NA} when n < 3 or n > 5000 (outside the valid range of the test).}
#'   \item{\code{normal}}{Logical. \code{TRUE} if \code{shapiro_p > 0.05}, indicating no
#'     significant departure from normality at the 5% level.}
#'   \item{\code{p_ttest}}{Shown when two groups are compared. P-value from an independent
#'     samples t-test, testing whether the means of the two groups differ. Assumes
#'     approximately normal distributions or large samples. All p-values are reported on
#'     the first row only; remaining rows contain \code{NA}.}
#'   \item{\code{p_wilcox}}{Shown when two groups are compared. P-value from the Wilcoxon
#'     rank-sum test (Mann-Whitney U test), a non-parametric alternative to the t-test.
#'     Preferred over \code{p_ttest} when data are skewed, ordinal, or contain outliers,
#'     as it compares ranks rather than means and makes no distributional assumptions.}
#'   \item{\code{p_anova}}{Shown when three or more groups are compared. P-value from a
#'     one-way analysis of variance (ANOVA) F-test, testing whether at least one group mean
#'     differs from the others. Assumes approximately normal distributions and equal
#'     variances across groups.}
#'   \item{\code{p_kruskal}}{Shown when three or more groups are compared. P-value from the
#'     Kruskal-Wallis test, a non-parametric alternative to one-way ANOVA. Preferred over
#'     \code{p_anova} when data are skewed or the normality assumption is not met, as it
#'     compares rank distributions and makes no distributional assumptions.}
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull distinct as_tibble if_else arrange group_by summarize mutate
#'   enquo bind_rows row_number
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_violin
#'   geom_histogram geom_density labs theme_minimal scale_color_discrete scale_fill_discrete
#' @importFrom stats median qt quantile sd qnorm aov as.formula chisq.test
#'   fisher.test kruskal.test t.test wilcox.test shapiro.test
#' @importFrom rlang enquo as_label quo_is_null
#' @importFrom patchwork wrap_plots
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
#' @export
dist_sum <- function(data, var, by = NULL) {
  var       <- rlang::enquo(var)
  by        <- rlang::enquo(by)
  var_label <- rlang::as_label(var)
  var_data  <- dplyr::pull(data, !!var)
  var_class <- class(var_data)
  if (inherits(var_data, c("Date", "POSIXt"))) {
    stop(paste("Variable is a date, please use dist_date() instead."))
  }
  if (!is.numeric(var_data)) {
    stop(paste("Variable is ", var_class, " and must be numeric for dist_sum."))
  }
  if (rlang::quo_is_null(by)) {
    values <- var_data

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
    crit_value <- if (n < 30) qt(0.975, df = n - 1) else qnorm(0.975)
    margin_error <- crit_value * (sd_value / sqrt(n))
    ci_lower <- mean_value - margin_error
    ci_upper <- mean_value + margin_error

    iqr_val    <- p75_value - p25_value
    n_outliers <- sum(values < (p25_value - 1.5 * iqr_val) |
                      values > (p75_value + 1.5 * iqr_val), na.rm = TRUE)
    shap_p     <- if (n >= 3 && n <= 5000)
                    tryCatch(shapiro.test(na.omit(values))$p.value,
                             error = function(e) NA_real_)
                  else NA_real_
    normal     <- shap_p > 0.05

    # Create a histogram and density plot
    plot_data <- tibble(values = values)
    histo_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = values)) +
      ggplot2::geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, bins = 30) +
      ggplot2::labs(title = "Histogram", x = var_label, y = "Frequency") +
      ggplot2::theme_minimal()
    density_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = values)) +
      ggplot2::geom_density(color = "black", alpha = 0.7) +
      ggplot2::labs(title = "Density Plot", x = var_label, y = "Density") +
      ggplot2::theme_minimal()

    print(patchwork::wrap_plots(histo_plot, density_plot))  # Print the combined plot


    # Return a tibble with the results
    return(tibble(
      n          = n,
      n_miss     = n_missing,
      median     = median_value,
      p25        = p25_value,
      p75        = p75_value,
      mean       = mean_value,
      sd         = sd_value,
      ci_lower   = ci_lower,
      ci_upper   = ci_upper,
      min        = min_value,
      max        = max_value,
      n_outliers = n_outliers,
      shapiro_p  = shap_p,
      normal     = normal
    ))
  } else {
    # Grouped summary statistics
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
        n_outliers = {
          q1    <- quantile(!!var, 0.25, na.rm = TRUE)
          q3    <- quantile(!!var, 0.75, na.rm = TRUE)
          iqr_v <- q3 - q1
          sum(!is.na(!!var) & (!!var < (q1 - 1.5 * iqr_v) | !!var > (q3 + 1.5 * iqr_v)))
        },
        shapiro_p = {
          vals <- (!!var)[!is.na(!!var)]
          n_v  <- length(vals)
          if (n_v >= 3 && n_v <= 5000)
            tryCatch(shapiro.test(vals)$p.value, error = function(e) NA_real_)
          else NA_real_
        },
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        ci_lower = mean - dplyr::if_else(n < 30, qt(0.975, df = n - 1), qnorm(0.975)) * sd / sqrt(n),
        ci_upper = mean + dplyr::if_else(n < 30, qt(0.975, df = n - 1), qnorm(0.975)) * sd / sqrt(n),
        normal   = shapiro_p > 0.05
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
    num_groups <- dplyr::n_distinct(data %>% dplyr::pull(!!by), na.rm = TRUE)

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
