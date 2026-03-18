# Explore all variables and generate an HTML or PDF summary report

Analyses a data frame or tibble, summarising all continuous, date and
categorical variables, missing data and duplicate values, and produces
an HTML or PDF report.

## Usage

``` r
explorer(
  data,
  output_file = NULL,
  format = c("html", "pdf"),
  progress = TRUE,
  id_var = NULL
)
```

## Arguments

- data:

  A data frame or tibble to explore.

- output_file:

  The name of the output file. Default uses `<df_name>_report.html` or
  `.pdf`

- format:

  Output format, either `"html"` (default) or `"pdf"`.

- progress:

  If `TRUE` (default), show a progress bar while building the report.

- id_var:

  Character vector of column names to treat as IDs (not summarised).

## Value

Outputs an html or PDF summary. Output in PDF typically takes longer.

For PDF output, a LaTeX distribution must be installed. TinyTeX is
recommended. To install, run:

    install.packages("tinytex")
    tinytex::install_tinytex()

## Examples

``` r
if (FALSE) { # \dontrun{
# Build example data from mtcars with some factors and a date column:
  cars_example <- mtcars %>%
    dplyr::mutate(
      across(c(vs, am, gear, carb, cyl), as.factor),
      date_var = as.Date("2025-06-01") +
        sample(-300:300, nrow(mtcars), replace = TRUE),
      id = dplyr::row_number()
    )
# To run explorer:
  explorer(mtcars)                  # with progress bar
  explorer(mtcars, progress = FALSE) # omit progress bar
  explorer(mtcars, format = "pdf")   # PDF output
  explorer(mtcars, format = "pdf", id_var = "id")   # Identify ID variable
} # }
```
