#' Summarise a categorial variable
#' @description
#' Summarises frequencies and percentages for a categorical variable.
#'
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#' eg. example_data %>% tab1(variable)
#'
#' @param data The data frame or tibble
#' @param variable The categorical variable you would like to summarise
#' @param dp The number of decimal places for percentages (default=2)
#' @return A tibble with frequencies and percentages
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull count mutate arrange bind_rows n desc rename
#' @importFrom rlang enquos
#' @importFrom ggplot2 aes ggplot geom_col geom_text coord_flip theme_minimal labs theme margin
#' @examples
#' example_data <- dplyr::tibble(id = 1:100, group = sample(c("a", "b", "c", "d"),
#'                                                   size = 100, replace = TRUE))
#' example_data$group[sample(1:100, size = 10)] <- NA  # Replace 10 with missing
#' tab1(example_data, group)
#' summary <- tab1(example_data, group) # Save summary statistics as a tibble.
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% tab1(group)
#' @export
tab1 <- function(data, variable, dp = 1) {
  var_name <- rlang::as_label(rlang::enquo(variable))
  tab_data <- data %>%
    dplyr::count({{ variable }}) %>%
    dplyr::mutate(
      Percent = n / sum(n) * 100
    ) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(Frequency = n, Category = {{ variable }}) %>%
    dplyr::mutate(Category = as.character(Category))
  total <- tibble::tibble(
    Category = "Total",
    Frequency = sum(tab_data$Frequency),
    Percent = sum(tab_data$Percent)
  )
  result <- dplyr::bind_rows(tab_data, total) %>%
    dplyr::mutate(Percent = sprintf(paste0("%.", dp, "f"), Percent))  # separate column
  freq_plot <- ggplot2::ggplot(tab_data, ggplot2::aes(x = reorder(Category, Percent), y = Percent)) +
    ggplot2::geom_col(fill = "steelblue", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Frequency, " (", sprintf("%.1f", Percent), "%)")),
              hjust = -0.1, size = 3.5) +
    ggplot2::coord_flip(clip="off") +
    ggplot2::labs(x = NULL, y = "Percent (%)", title = "Distribution of HBsAg") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 60, 5, 5))  # top, right, bottom, left (in points)
  print(freq_plot)
  print(result)
}
