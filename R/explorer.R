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
  # When called via %>%, substitute() returns "."; recover the LHS name instead
  if (df_name == ".") {
    df_name <- tryCatch(
      deparse(sys.call(-1)[[2]]),
      error = function(e) "data"
    )
    if (df_name == ".") df_name <- "data"
  }
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

  # Prompt for likely ID columns not already specified
  if (interactive()) {
    remaining     <- setdiff(names(data), id_var)
    id_candidates <- grep("^(ids?|pids?)$", remaining, ignore.case = TRUE, value = TRUE)
    for (candidate in id_candidates) {
      response <- readline(prompt = paste0(
        "Column '", candidate, "' looks like an identifier. ",
        "Exclude it from summaries? [y/n]: "
      ))
      if (tolower(trimws(response)) %in% c("y", "yes")) {
        id_var <- c(id_var, candidate)
      }
    }
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
  ncol_data <- length(all_vars)
  n_id      <- length(id_var)
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
    paste0("id_var <- c(", paste(shQuote(id_var), collapse = ", "), ")"),
    "```", "",

    # CSS styling (HTML only)
    if (format == "html") c(
      "<style>",
      "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
      "       line-height: 1.7; color: #2c3e50; max-width: 1050px; margin: 0 auto; padding: 10px 40px; }",
      "h1 { margin-bottom: 0.3em; }",
      "h2 { margin-top: 2.5em; margin-bottom: 0.8em;",
      "     border-bottom: 2px solid #e8eaed; padding-bottom: 0.4em; color: #1a252f; }",
      "h3 { margin-top: 2.2em; margin-bottom: 0.6em; color: #2c3e50;",
      "     border-left: 4px solid #5d9cec; padding-left: 10px; font-size: 1.1em; }",
      "p  { margin-bottom: 1em; }",
      "table { margin-top: 0.6em !important; margin-bottom: 0.4em !important; }",
      "table th { background-color: #f4f6f8 !important; }",
      ".table-footnote { margin-top: 0.2em; margin-bottom: 1.5em; }",
      "hr { border: none; border-top: 1px solid #e8eaed; margin: 2em 0; }",
      "</style>", ""
    ) else NULL,

    # Logo chunk
    "```{r logo, echo=FALSE, results='asis'}",
    "cat(sprintf(",
    "  '<img src=\"%s\" style=\"float:right;width:150px;\"/>',",
    "  system.file('man/figures/logo.png', package = 'sumvar')",
    "))",
    "```", "",

    # High-level summary
    sprintf("There are %d variables and %d rows (observations).", ncol_data, nrow_data),
    sprintf("There are %d numeric, %d date, and %d categorical variable%s%s",
            n_numeric, n_date, n_cat,
            if (n_numeric + n_date + n_cat == 1) "" else "s",
            if (n_id > 0)
              sprintf(", and %d identifier variable%s (%s).",
                      n_id,
                      if (n_id == 1) "" else "s",
                      paste(id_var, collapse = ", "))
            else "."),
    "",

    # Continuous summary (only included when numeric variables exist)
    if (n_numeric > 0) c(
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
      if (format == "pdf")
        "  select(variable, everything(), -n_outliers, -shapiro_p, -normal) %>%"
      else
        "  select(variable, everything()) %>%",
      "  knitr::kable() %>%",
      if (format == "html")
        "  kableExtra::kable_styling(full_width = FALSE, position = 'left') %>%"
      else
        "  kableExtra::kable_styling(full_width = FALSE, position = 'left', font_size = 8) %>%",
      "  kableExtra::footnote(general = '95% CIs use the t-distribution when n < 30, and the Z-distribution when n >= 30.',",
      "                       general_title = 'Note: ', footnote_as_chunk = TRUE)",
      "```", ""
    ) else NULL,

    # Correlation heatmap (only included when >= 2 numeric variables)
    if (n_numeric >= 2) c(
      "### Correlation matrix",
      "```{r corr-heatmap, echo=FALSE, results='asis', warning=FALSE, message=FALSE, fig.width=7, fig.height=5}",
      "num_vars_corr <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], id_var)",
      "cor_mat <- cor(data[num_vars_corr], use = 'pairwise.complete.obs')",
      "cor_df  <- as.data.frame(as.table(cor_mat))",
      "names(cor_df) <- c('Var1', 'Var2', 'r')",
      "ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = r)) +",
      "  ggplot2::geom_tile(color = 'white') +",
      "  ggplot2::geom_text(ggplot2::aes(label = round(r, 2)), size = 3) +",
      "  ggplot2::scale_fill_gradient2(low = '#d73027', mid = 'white', high = '#1a9850',",
      "                                midpoint = 0, limits = c(-1, 1), name = 'r') +",
      "  ggplot2::labs(x = NULL, y = NULL) +",
      "  ggplot2::theme_minimal() +",
      "  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))",
      "```",
      ""
    ) else NULL,

    # Date summary (only included when date variables exist)
    if (n_date > 0) c(
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
      ""
    ) else NULL,


    # Categorical summary (only included when categorical variables exist)
    if (n_cat > 0) c(
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
      "```", ""
    ) else NULL,

    # Detailed review header
    "## Detailed review of variables", ""
  )

  # Start progress bar if requested
  if (progress) {
    cli::cli_progress_bar("Building sections",
                          total = length(ordered_vars),
                          format = "{pb_bar} {pb_current}/{pb_total} vars")
  }

  # Append each variable's detailed chunk
  for (v in ordered_vars) {
    if (progress) cli::cli_progress_update()
    func <- choose_sumvar_function(data[[v]])
    if (is.null(func)) next
    rmd_lines <- c(
      rmd_lines,
      if (format == "html") "<hr>" else NULL,
      paste0("### ", v),
      paste0("```{r ", v,
             ", echo=FALSE, results='asis'",
             ", warning=FALSE, message=FALSE",
             ", fig.width=8, fig.height=3.5}"),
      paste0("var_summary <- do.call(sumvar::", func,
             ", list(data, as.name(\"", v, "\")))"),
      "var_summary <- var_summary %>% dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))",
      if (format == "pdf" && func == "dist_sum")
        "var_summary <- var_summary %>% dplyr::select(-dplyr::any_of(c('n_outliers', 'shapiro_p', 'normal')))"
      else
        NULL,
      "var_summary %>%",
      "  knitr::kable() %>%",
      "  kableExtra::kable_styling(",
      if (format == "pdf") "full_width = FALSE, position = 'left', font_size = 8)" else "full_width = FALSE, position = 'left')",
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
    NULL
  }
}

