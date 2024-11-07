#' Create a cross-tabulation of two categorial variables
#' @description
#' Creates a "n x n" cross-tabulation of two categorical variables, with row percentages.
#' Includes options for adding frequentist hypothesis testing.
#'
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#'
#' eg. example_data %>% tab(variable1, variable2)
#'
#' @param data The data frame or tibble
#' @param variable1 The first categorical variable
#' @param variable2 The second categorical variable
#' @param test Optional frequentist hypothesis test, use test=exact for Fisher's exact or test=chi for Chi squared
#' @return A tibble with a cross-tabulation of frequencies and row percentages
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull count mutate arrange bind_rows n desc rename group_by
#'  where select
#' @importFrom tidyr pivot_wider
#' @examples
#' example_data <- dplyr::tibble(id = 1:100, group1 = sample(c("a", "b", "c", "d"),
#'                                                   size = 100, replace = TRUE),
#'                                                   group2= sample(c("male", "female"),
#'                                                   size = 100, replace = TRUE))
#' example_data$group1[sample(1:100, size = 10)] <- NA  # Replace 10 with missing
#' tab(example_data, group1, group2)
#' summary <- tab(example_data, group1, group2) # Save summary statistics as a tibble.
#' # Note that this function accepts a pipe input
#' # eg. example_data %>% tab(group)
#' @export
tab <- function(data, variable1, variable2, test = "none") {
  test <- as.character(substitute(test))
  if (!test %in% c("none", "chi", "exact")) {
    stop("Invalid test argument. Use 'chi', 'exact', or leave blank.")
  }
  table_data <- data %>%
    count({{ variable1 }}, {{ variable2 }}) %>%
    group_by({{ variable1 }}) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ungroup() %>%
    rename(Frequency = n) %>%
    select({{ variable1 }}, {{ variable2 }}, Frequency, Percent)
  table_wide <- table_data %>%
    pivot_wider(names_from = {{ variable2 }}, values_from = c(Frequency, Percent), names_sep = "_") %>%
    arrange({{ variable1 }})
  table_wide <- table_wide %>%
    rename(!!as_label(enquo(variable1)) := {{ variable1 }})
  test_name <- NA_character_
  statistic_value <- NA_real_
  p_value <- NA_real_
  if (test == "chi" || test == "exact") {
    contingency_table <- table(data[[as_label(enquo(variable1))]], data[[as_label(enquo(variable2))]])

    if (test == "chi") {
      chi_result <- chisq.test(contingency_table)
      test_name <- "Chi-Squared"
      statistic_value <- chi_result$statistic
      p_value <- chi_result$p.value

    } else if (test == "exact") {
      fisher_result <- tryCatch({
        fisher.test(contingency_table)
      }, error = function(e) {
        fisher.test(contingency_table, simulate.p.value = TRUE, B = 1e4)
      })
      test_name <- ifelse(inherits(fisher_result, "htest"), "Fisher's Exact", "Fisher's Exact (Simulated)")
      statistic_value <- if (!is.null(fisher_result$statistic)) fisher_result$statistic else NA_real_
      p_value <- fisher_result$p.value
    }
  }
  table_wide <- table_wide %>%
    mutate(
      Test = if_else(row_number() == 1, test_name, NA_character_),
      Statistic = if_else(row_number() == 1, statistic_value, NA_real_),
      P_Value = if_else(row_number() == 1, p_value, NA_real_)
    )
  table_wide <- table_wide %>%
    select(where(~ any(!is.na(.))))
  table_wide
}
