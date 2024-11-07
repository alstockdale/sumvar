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
  # Calculate frequencies and percentages for categorical data
  tab_data <- data %>%
    count({{ variable }}) %>%
    mutate(
      Percent = n / sum(n) * 100  # Calculate as numeric first
    ) %>%
    arrange(desc(n)) %>%
    rename(Frequency = n, Category = {{ variable }}) %>%
    mutate(Category = as.character(Category))
  total <- tibble(
    Category = "Total",
    Frequency = sum(tab_data$Frequency),
    Percent = sum(tab_data$Percent)
  )
  result <- bind_rows(tab_data, total) %>%
    mutate(Percent = sprintf(paste0("%.", dp, "f"), Percent))
  result
}
