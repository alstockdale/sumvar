#' Explore all variables and generate an HTML or PDF summary report
#' @description
#' Analyses a data frame or tibble, summarising all continuous, date and
#' categorical variables, missing data and duplicate values, and produces an
#' HTML or PDF report.
#'
#' @param data A data frame or tibble to explore.
#' @param output_file The name of the output file. Default uses `<df_name>_report.html`
#'   or `.pdf`
#' @param id_var Character vector of column names to treat as IDs (not summarised).
#' @param format Output format, either `"html"` (default) or `"pdf"`.
#' @param progress If `TRUE` (default), show a progress bar while building the report.
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec
#' @return Outputs an html or PDF summary. Output in PDF typically takes longer.
#'
#' For PDF output, a LaTeX distribution must be installed. TinyTeX is recommended. To install, run:
#' ```r
#' install.packages("tinytex")
#' tinytex::install_tinytex()
#' ```
#' @export
#' @examples
#' \dontrun{
#' # Build example data from mtcars with some factors and a date column:
#'   cars_example <- mtcars %>%
#'     dplyr::mutate(
#'       across(c(vs, am, gear, carb, cyl), as.factor),
#'       date_var = as.Date("2025-06-01") +
#'         sample(-300:300, nrow(mtcars), replace = TRUE),
#'       id = dplyr::row_number()
#'     )
#' # To run explorer:
#'   explorer(mtcars)                  # with progress bar
#'   explorer(mtcars, progress = FALSE) # omit progress bar
#'   explorer(mtcars, format = "pdf")   # PDF output
#'   explorer(mtcars, format = "pdf", id_var = "id")   # Identify ID variable
#' }
explorer <- function(data,
                    output_file = NULL,
                    format = c("html", "pdf"),
                    progress = TRUE,
                    id_var = NULL) {
  # dependencies
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required. Please install it.")
  }
  if (progress && !requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' is required for progress output. Please install it.")
  }

  # choose format and file extension
  format <- match.arg(format)
  df_name <- deparse(substitute(data))
  ext     <- if (format == "html") "html" else "pdf"
  if (is.null(output_file)) {
    output_file <- paste0(df_name, "_report.", ext)
  }

  # --- validate id_vars ---
  if (!is.null(id_var)) {
    if (!all(id_var %in% names(data))) {
      stop("All elements of id_var must be column names in data")
    }
  } else {
    id_var <- character(0)
  }

  # metadata
  all_vars   <- names(data)
  var_names  <- setdiff(all_vars, id_var)
  nrow_data      <- nrow(data)
  is_numeric     <- vapply(data[var_names], is.numeric, logical(1))
  is_date        <- vapply(data[var_names],
                           function(x) inherits(x, "Date") || inherits(x, "POSIXt"),
                           logical(1))
  is_categorical <- vapply(data[var_names],
                           function(x) is.factor(x) || is.character(x),
                           logical(1))
  ncol_data <- length(var_names)
  n_numeric <- sum(is_numeric)
  n_date    <- sum(is_date)
  n_cat     <- sum(is_categorical)

  order_df <- data.frame(
    var  = var_names,
    type = factor(
      ifelse(is_numeric, "numeric",
             ifelse(is_date, "date", "categorical")
      ),
      levels = c("numeric","date","categorical")
    ),
    stringsAsFactors = FALSE
  )
  ordered_vars <- order_df$var[order(order_df$type, order_df$var)]

  # Build the Rmd content up to detailed review
  rmd_lines <- c(
    # YAML header
    "---",
    sprintf("title: \"Exploring variables in %s\"", df_name),
    if (format == "html") "output: html_document" else "output: pdf_document",
    "params:",
    "  data: NULL",
    "---", "",

    # Setup chunk
    "```{r setup, include=FALSE}",
    "library(sumvar); library(dplyr); library(kableExtra)",
    "data <- params$data",
    "```", "",

    # Logo chunk
    "```{r logo, echo=FALSE, results='asis'}",
    "cat(sprintf(",
    "  '<img src=\"%s\" style=\"float:right;width:150px;\"/>',",
    "  system.file('man/figures/logo.png', package = 'sumvar')",
    "))",
    "```", "",

    # High-level summary
    sprintf("There are %d variables and %d rows (observations).", ncol_data, nrow_data),
    sprintf("There are %d numeric, %d date, and %d categorical variables.",
            n_numeric, n_date, n_cat),
    "",

    # Continuous summary
    "### Summary of continuous variables",
    "```{r numeric-summary, echo=FALSE, results='asis', warning=FALSE, message=FALSE, fig.show='hide'}",
    "num_vars <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], id_var)",
    "numeric_summaries <- lapply(num_vars, function(v) {",
    "  df <- do.call(sumvar::dist_sum, list(data, as.name(v)))",
    "  df$variable <- v",
    "  df",
    "}) %>% bind_rows() %>%",
    "  mutate(across(where(is.numeric), ~ round(.x, 2)))",
    "numeric_summaries %>%",
    "  select(variable, everything()) %>%",
    "  knitr::kable() %>%",
    "  kableExtra::kable_styling(full_width = FALSE, position = 'left')",
    "```", "",

    # Date summary
    "### Summary of date variables",
    "```{r date-summary, echo=FALSE, results='asis', warning=FALSE, message=FALSE, fig.show='hide'}",
    "date_vars <- setdiff(names(data)[vapply(data, function(x) inherits(x, 'Date') || inherits(x, 'POSIXt'), logical(1))], id_var)",
    "date_summary <- tibble::tibble(",
    "  variable   = date_vars,",
    "  n          = nrow(data),",
    "  n_missing  = vapply(data[date_vars], function(x) sum(is.na(x)), integer(1)),",
    "  min_value  = vapply(data[date_vars], function(x) as.character(min(x, na.rm = TRUE)), character(1)),",
    "  max_value  = vapply(data[date_vars], function(x) as.character(max(x, na.rm = TRUE)), character(1)),",
    "  n_distinct = vapply(data[date_vars], dplyr::n_distinct, integer(1))",
    ")",
    "date_summary %>%",
    "  knitr::kable() %>%",
    "  kableExtra::kable_styling(full_width = FALSE, position = 'left')",
    "```",
    "",


    # Categorical summary
    "### Summary of categorical variables",
    "```{r cat-summary, echo=FALSE, results='asis', warning=FALSE, message=FALSE}",
    "cat_vars <- setdiff(names(data)[vapply(data, function(x) is.factor(x)||is.character(x), logical(1))], id_var)",
    "cat_summary <- tibble::tibble(",
    "  variable     = cat_vars,",
    "  var_type     = vapply(data[cat_vars], function(x) class(x)[1], character(1)),",
    "  n            = nrow(data),",
    "  n_missing    = vapply(data[cat_vars], function(x) sum(is.na(x)), integer(1)),",
    "  n_distinct   = vapply(data[cat_vars], dplyr::n_distinct, integer(1)),",
    "  n_duplicates = vapply(data[cat_vars], function(x) sum(duplicated(x)), integer(1))",
    ")",
    "cat_summary %>%",
    "  knitr::kable() %>%",
    "  kableExtra::kable_styling(full_width = FALSE, position = 'left')",
    "```", "",

    # Detailed review header
    "## Detailed review of variables", ""
  )

  # Start progress bar if requested
  if (progress) {
    cli::cli_progress_bar("Building sections",
                          total = length(ordered_vars),
                          format = "{cli_bar} {cli_count}/{cli_total} vars")
  }

  # Append each variable's detailed chunk
  for (v in ordered_vars) {
    if (progress) cli::cli_progress_update()
    func <- choose_sumvar_function(data[[v]])
    rmd_lines <- c(
      rmd_lines,
      paste0("### `", v, "`"),
      paste0("```{r ", v,
             ", echo=FALSE, results='asis'",
             ", warning=FALSE, message=FALSE",
             ", fig.width=4, fig.height=3}"),
      paste0("tab <- do.call(sumvar::", func,
             ", list(data, as.name(\"", v, "\")))"),
      "tab <- tab %>% dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))",
      "tab %>%",
      "  knitr::kable() %>%",
      "  kableExtra::kable_styling(",
      "full_width = FALSE, position = 'left')",
      "```", ""
    )
  }

  if (progress) cli::cli_progress_done()

  # Render
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(rmd_lines, tmp)
  out <- rmarkdown::render(tmp,
                           output_file = output_file,
                           params      = list(data = data),
                           envir       = new.env(),
                           quiet       = !progress)
  if (progress) cli::cli_alert_success("Report written to {.file {out}}")
  if (interactive()) {
    utils::browseURL(normalizePath(out))
    invisible(out)
  }
}
# Helper to choose the summary function
choose_sumvar_function <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    "dist_date"
  } else if (is.factor(x) || is.character(x)) {
    "tab1"
  } else if (is.numeric(x)) {
    "dist_sum"
  } else {
    ""
  }
}

