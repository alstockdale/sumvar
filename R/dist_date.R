utils::globalVariables("values")
#' Summarize and visualize a date variable
#' @description
#' Summarises the minimum, maximum, median, and interquartile range of a date variable,
#' optionally stratified by a grouping variable. Produces a histogram and (optionally) a density plot.
#'
#' @param data A data frame or tibble.
#' @param var The date variable to summarise.
#' @param by Optional grouping variable.
#'
#' @return A tibble with summary statistics for the date variable.
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull distinct as_tibble if_else arrange group_by summarize mutate
#'   enquo bind_rows row_number
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_violin
#'   geom_histogram geom_density labs theme_minimal scale_color_discrete scale_fill_discrete
#' @importFrom stats median qt quantile sd qnorm aov as.formula
#' @importFrom rlang enquos as_label ':='
#' @importFrom utils globalVariables
#' @examples
#' # Example ungrouped
#' df <- tibble::tibble(
#'   dt = as.Date("2020-01-01") + sample(0:1000, 100, TRUE)
#' )
#' dist_date(df, dt)
#'
#' # Example grouped
#' df2 <- tibble::tibble(
#'   dt = as.Date("2020-01-01") + sample(0:1000, 100, TRUE),
#'   grp = sample(1:2, 100, TRUE)
#' )
#' dist_date(df2, dt, grp)
#' # Note this function accepts a pipe from dplyr eg. df %>% dist_date(date_var, group_var)
#' @seealso \code{\link{dist_sum}} for continuous variables.
#' @export
dist_date <- function(data, var, by = NULL) {
  var_enquo <- rlang::enquo(var)
  by_enquo <- rlang::enquo(by)
  var_data <- dplyr::pull(data, !!var_enquo)
  var_class <- class(var_data)
  if (!inherits(var_data, c("Date", "POSIXt"))) {
    stop(paste("Variable is", paste(var_class, collapse = "/"),
               "and must be Date or POSIXt for dist_date."))
  }

  var_name <- rlang::as_label(var_enquo)
  if (!rlang::quo_is_null(by_enquo)) {
    group_name <- rlang::as_label(by_enquo)
    summary_data <- data %>%
      dplyr::group_by(!!by_enquo) %>%
      dplyr::summarise(
        n = sum(!is.na(!!var_enquo)),
        n_miss = sum(is.na(!!var_enquo)),
        min = min(!!var_enquo, na.rm = TRUE),
        p25 = quantile(!!var_enquo, 0.25, na.rm = TRUE, type = 1),
        median = median(!!var_enquo, na.rm = TRUE),
        p75 = quantile(!!var_enquo, 0.75, na.rm = TRUE, type = 1),
        max = max(!!var_enquo, na.rm = TRUE),
        .groups = 'drop'
      )
    # Grouped density plot
    density_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!var_enquo, fill= as.factor(!!by_enquo))) +
      ggplot2::geom_density(color = "black", alpha = 0.5) +
      ggplot2::labs(title = paste("Density Plot of", var_name), x = var_name, y = "Density") +
      ggplot2::theme_minimal()
    print(density_plot)
    return(summary_data)
  } else {
    n <- sum(!is.na(var_data))
    n_missing <- sum(is.na(var_data))
    min_value <- min(var_data, na.rm = TRUE)
    max_value <- max(var_data, na.rm = TRUE)
    median_value <- median(var_data, na.rm = TRUE)
    p25_value <- quantile(var_data, 0.25, na.rm = TRUE, type = 1)
    p75_value <- quantile(var_data, 0.75, na.rm = TRUE, type = 1)
    plot_data <- tibble::tibble(values = var_data)
    histo_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = values)) +
      ggplot2::geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, bins = 30) +
      ggplot2::labs(
        title = paste("Histogram of", var_name),
        x = var_name, y = "Count"
      ) +
      ggplot2::theme_minimal()
    print(histo_plot)
    return(tibble::tibble(
      n = n,
      n_miss = n_missing,
      min = min_value,
      p25 = p25_value,
      median = median_value,
      p75 = p75_value,
      max = max_value
    ))
  }
}
