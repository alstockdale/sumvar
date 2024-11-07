#' Count the number of duplicates in a variable
#' @description
#' Provides an integer value for the number of duplicates found wihtin a variable
#'
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#'
#' eg. example_data %>% dup(variable)
#'
#' @param data The data frame or tibble
#' @param var The variable to assess
#' @return A tibble with the number of duplicates found, and the number of missing values (NA), together with percentages.
#' Missing values are not considered as duplicates, and are excluded from the calculation of percentage of duplicate values.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull count mutate arrange bind_rows n desc rename group_by ungroup
#' @importFrom stats na.omit
#' @examples
#' example_data <- dplyr::tibble(id = 1:200, age = round(rnorm(200, mean = 30, sd = 5), digits=0))
#' example_data$age[sample(1:200, size = 15)] <- NA  # Replace 15 values with missing.
#' dup(example_data, age)
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% dup(group)
#' @export
dup <- function(data, var) {
  values <- data %>% pull({{ var }}) %>% na.omit()  # Remove NA values before counting duplicates
  n_duplicate <- sum(duplicated(values))  # Count duplicates without NA values
  n_missing <- sum(is.na(data %>% pull({{ var }})))  # Count the number of missing values separately
  total <- nrow(data)
  pc_duplicate <- n_duplicate/(total-n_missing)*100
  pc_missing <- n_missing/total*100
  # Return the result as a tibble
  tibble(n_duplicate = n_duplicate, percent_duplicate= pc_duplicate, n_missing = n_missing, percent_missing=pc_missing)
}
