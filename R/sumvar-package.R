#' @title sumvar: Summarise Continuous and Categorical Variables in R
#'
#' @description
#' The sumvar package explores continuous and categorical variables.
#' sumvar brings the ease and simplicity of the "sum" and "tab" functions from Stata to R.
#'
#' \itemize{
#'   \item To explore a continuous variable, use \code{dist_sum()}. You can stratify by a grouping variable: \code{df \%>\% dist_sum(var, group)}
#'   \item To explore dates, use \code{dist_date()}; usage is the same as \code{dist_sum()}.
#'   \item To summarise a single categorical variable use \code{tab1()}, e.g. \code{df \%>\% tab1(var)}. For a two-way table, use \code{tab()}, e.g. \code{df \%>\% tab(var1, var2)}. Both include options for frequentist hypothesis tests.
#'   \item Explore duplicates and missing values with with \code{dup()}.
#' }
#'
#' All functions are tidyverse/dplyr-friendly and accept the \code{\%>\%} pipe, outputting results as a tibble. You can save outputs for further manipulation, e.g. \code{summary <- df \%>\% dist_sum(var)}.
#'
#' @name sumvar
"_PACKAGE"
