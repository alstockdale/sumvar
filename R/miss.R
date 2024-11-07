#' Count missing values
#' @description
#' Provides the frequency and percentage of missing values. Counts both NA, and "" for character variables (empty character variables) as missing.
#'
#' The function accepts an input from a dplyr pipe %>% and returns results as a tibble. If no variables are specified, returns
#' a list of missing values for the entire data frame/ tibble.
#'
#' eg. data %>% miss(variable1, variable2)
#'
#' @param data The data frame or tibble
#' @param ... The variable(s) to assess for missing data
#' @param dp The number of decimal places for percentages
#' @return A tibble with the number of valid values (n_valid), missing values (n_missing), together with percentages.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull count mutate arrange bind_rows n desc rename group_by ungroup summarise
#' @importFrom stats na.omit
#' @importFrom rlang enquos
#' @examples
#' example_data <- dplyr::tibble(id = 1:200, age = round(rnorm(200, mean = 30, sd = 5), digits=0),
#'                                           sex = sample(c("male", "female"),
#'                                           size = 200, replace = TRUE),)
#' example_data$age[sample(1:200, size = 15)] <- NA  # Replace 15 values with missing.
#' example_data$sex[sample(1:200, size = 30)] <- ""  # Replace 30 values with empty character vectors.
#' miss(example_data, id, age, sex, dp=1)
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% miss()
#' @export
miss <- function(data, ..., dp = 1) {
  vars <- rlang::enquos(...)
  if (length(vars) == 0) {
    vars <- rlang::syms(names(data))
  }
  total <- nrow(data)
  results <- tibble()
  for (var in vars) {
    var_name <- rlang::as_label(var)
    values <- data %>% dplyr::pull(!!var)
    n_missing <- sum(is.na(values) | values == "")
    pc_missing <- (n_missing / total) * 100
    results <- results %>%
      bind_rows(tibble(
        variable = var_name,
        total = total,
        n_missing = n_missing,
        pc_missing = sprintf(paste0("%.", dp, "f"), pc_missing),
      ))
  }
  return(results)
}
