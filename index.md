# sumvar

The sumvar package quickly explores continuous, date and categorical
variables. sumvar brings the ease and simplicity of the `sum` and `tab`
commands from Stata to R.

## Installation

Install from CRAN:

``` r
install.packages("sumvar")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("alstockdale/sumvar")
```

## Functions

- **[`dist_sum()`](https://alstockdale.github.io/sumvar/reference/dist_sum.md)**
  — explore a continuous variable; stratify by a grouping variable with
  `df %>% dist_sum(var, group)`.
- **[`dist_date()`](https://alstockdale.github.io/sumvar/reference/dist_date.md)**
  — summarise date variables; usage is the same as
  [`dist_sum()`](https://alstockdale.github.io/sumvar/reference/dist_sum.md).
- **[`tab1()`](https://alstockdale.github.io/sumvar/reference/tab1.md)**
  — frequency table for a single categorical variable:
  `df %>% tab1(var)`.
- **[`tab()`](https://alstockdale.github.io/sumvar/reference/tab.md)** —
  two-way cross-tabulation: `df %>% tab(var1, var2)`.
- **[`dup()`](https://alstockdale.github.io/sumvar/reference/dup.md)** —
  explore duplicates and missing values across a single variable or an
  entire data frame.
- **[`explorer()`](https://alstockdale.github.io/sumvar/reference/explorer.md)**
  — generate an HTML or PDF exploratory data analysis report for a whole
  data frame.

Both
[`dist_sum()`](https://alstockdale.github.io/sumvar/reference/dist_sum.md)
and [`tab()`](https://alstockdale.github.io/sumvar/reference/tab.md)
include options for frequentist hypothesis tests. See the function help
pages for details.

All functions accept the `%>%` pipe from a tibble or data frame and
return results as a tibble, so outputs can be saved and further
manipulated:

``` r
summary <- df %>% dist_sum(var)
```
