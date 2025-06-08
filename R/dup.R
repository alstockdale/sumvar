utils::globalVariables(c(
  "n_unique", "n_duplicate", "n_missing", "Variable", "Count", "Proportion", "ymax", "ymin", "ymid", "percent_label"
))
#' Explore duplicate and missing data
#' @description
#' Provides an integer value for the number of duplicates found within a variable
#' The function accepts an input from a dplyr pipe "%>%" and outputs the results as a tibble.
#'
#' eg. example_data %>% dup(variable)
#'
#' @param data The data frame or tibble
#' @param var The variable to assess
#' @return A tibble with the number and percentage of duplicate values found, and the number of missing values (NA), together with percentages.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble pull count mutate arrange bind_rows n desc rename group_by ungroup
#' @importFrom stats na.omit
#' @importFrom purrr map_dfr
#' @importFrom rlang enexpr enquo as_label
#' @examples
#' example_data <- dplyr::tibble(id = 1:200, age = round(rnorm(200, mean = 30, sd = 50), digits=0))
#' example_data$age[sample(1:200, size = 15)] <- NA  # Replace 15 values with missing.
#' dup(example_data, age)
#' # It is also possible to pass a whole database to dup and it will explore all variables.
#' example_data <- dplyr::tibble(age = round(rnorm(200, mean = 30, sd = 50), digits=0),
#'                               sex = sample(c("Male", "Female"), 200, TRUE),
#'                               favourite_colour = sample(c("Red", "Blue", "Purple"), 200, TRUE))
#' example_data$age[sample(1:200, size = 15)] <- NA  # Replace 15 values with missing.
#' example_data$sex[sample(1:200, size = 32)] <- NA  # Replace 32 values with missing.
#' dup(example_data)
#' @export
dup <- function(data, var = NULL) {
  # Helper for single variable
  dup_one <- function(vec, varname) {
    n_total <- length(vec)
    n_missing <- sum(is.na(vec))
    vec_no_na <- vec[!is.na(vec)]
    n_unique <- dplyr::n_distinct(vec_no_na)
    n_duplicate <- sum(duplicated(vec_no_na))
    n_nonmissing <- n_total - n_missing
    tibble::tibble(
      Variable = varname,
      n = n_total,
      n_unique = n_unique,
      n_duplicate = n_duplicate,
      percent_duplicate = round(n_duplicate / max(1, n_nonmissing) * 100, 2),
      n_missing = n_missing,
      percent_missing = round(n_missing / n_total * 100, 2)
    )
  }

  if (is.null(rlang::enexpr(var))) {
    # Whole data frame mode
    results <- purrr::map_dfr(
      names(data),
      ~dup_one(data[[.x]], .x)
    )
    # Data for plot
    df_bar <- results %>%
      tidyr::pivot_longer(
        cols = c(n_unique, n_duplicate, n_missing),
        names_to = "Category", values_to = "Count"
      ) %>%
      dplyr::group_by(Variable) %>%
      dplyr::mutate(Proportion = Count / n[1]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        Category = dplyr::recode(Category,
                                 n_unique = "Unique",
                                 n_duplicate = "Duplicate",
                                 n_missing = "Missing")
      )

    # To keep order of stacking: Unique (bottom), Duplicate, Missing (top)
    df_bar$Category <- factor(df_bar$Category, levels = c("Unique", "Duplicate", "Missing"))

    # Calculate ymid for label positions, per variable
    df_bar <- df_bar %>%
      dplyr::group_by(Variable) %>%
      dplyr::arrange(Category, .by_group = TRUE) %>%
      dplyr::mutate(
        percent_label = Count,
        ymax = cumsum(Proportion),
        ymin = dplyr::lag(ymax, default = 0),
        ymid = (ymin + ymax) / 2
      ) %>%
      dplyr::ungroup()

    plot <- ggplot2::ggplot(df_bar, ggplot2::aes(x = Variable, y = Proportion, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = 1-ymid,
          label = percent_label
        ),
        size = 3,
        color = "white"
      ) +
      ggplot2::labs(
        x = "Variable", y = "Proportion",
        title = "Proportion of unique, duplicate, and missing values for each variable"
      ) +
      ggplot2::scale_fill_manual(values = c("Unique" = "#63B3ED", "Duplicate" = "#F56565", "Missing" = "#A0AEC0")) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    print(plot)
    return(results)
  } else {
    # As before for single variable
    var_quo <- rlang::enquo(var)
    vec <- dplyr::pull(data, !!var_quo)
    varname <- rlang::as_label(var_quo)
    out <- dup_one(vec, varname)
    # Reuse your bar code for single variable as before, if desired
    n_total <- out$n
    df_bar <- tibble::tibble(
      Category = factor(c("Unique", "Duplicate", "Missing"), levels = c("Unique", "Duplicate", "Missing")),
      Count = c(out$n_unique, out$n_duplicate, out$n_missing),
      Proportion = c(out$n_unique / n_total, out$n_duplicate / n_total, out$n_missing / n_total)
    ) %>%
      dplyr::mutate(
        percent_label = paste0(Count, " (", sprintf("%.1f", Proportion * 100), "%)"),
        ymax = cumsum(Proportion),
        ymin = dplyr::lag(ymax, default = 0),
        ymid = (ymin + ymax) / 2
      )
    plot <- ggplot2::ggplot(df_bar, ggplot2::aes(x = "", y = Proportion, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = 1-ymid,
          label = percent_label
        ),
        size = 4,
        color = "black"
      ) +
      ggplot2::labs(
        x = NULL, y = "Proportion",
        title = paste("Proportion of unique, duplicate, and missing values for", rlang::as_label(rlang::enquo(var)))
      ) +
      ggplot2::scale_fill_manual(values = c("Unique" = "#63B3ED", "Duplicate" = "#F56565", "Missing" = "#A0AEC0")) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    print(plot)
    return(out)
  }
}
