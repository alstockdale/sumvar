# Create a cross-tabulation of two categorical variables

Creates a cross-tabulation of two categorical variables with row or
column percentages, row and column totals, and optional hypothesis
tests. Prints a formatted table to the console (similar to Stata's `tab`
command) with the column variable name displayed as a spanning header
above its levels.

## Usage

``` r
tab(
  data,
  variable1,
  variable2,
  show = c("row", "col", "n"),
  test = c("both", "chi", "exact", "none"),
  totals = TRUE,
  dp = 1
)
```

## Arguments

- data:

  The data frame or tibble.

- variable1:

  The row variable (first categorical variable).

- variable2:

  The column variable (second categorical variable).

- show:

  What to display in each cell: `"row"` (default) shows `n (row%)`,
  `"col"` shows `n (col%)`, `"n"` shows counts only.

- test:

  Hypothesis test(s) to report: `"both"` (default) runs both chi-squared
  and Fisher's exact; `"chi"` for chi-squared only; `"exact"` for
  Fisher's exact only (falls back to simulated p-value for large
  tables); `"none"` to suppress tests.

- totals:

  If `TRUE` (default), includes a Total column (row totals) and a Total
  row (column totals). Set to `FALSE` to suppress both.

- dp:

  Number of decimal places for percentages. Default is `1`.

## Value

A wide-format tibble (invisibly) with:

- First column: levels of `variable1` (plus `"Total"` if
  `totals = TRUE`).

- For each level of `variable2`: `{level}_n` (integer count) and, when
  `show != "n"`, `{level}_pct` (numeric percentage).

- `total_n`: row totals (when `totals = TRUE`).

- `p_chi`, `p_fisher`: p-values on the first row, `NA` elsewhere (when
  `test = "both"`; individual tests add `test`, `statistic`, `p_value`
  instead).

## Examples

``` r
example_data <- dplyr::tibble(
  group1 = sample(c("a", "b", "c"), 100, replace = TRUE),
  group2 = sample(c("male", "female"), 100, replace = TRUE)
)
tab(example_data, group1, group2)
#>         |         group2          |       
#> group1  |   female   |    male    | Total 
#> --------+------------+------------+-------
#> a       | 11 (36.7%) | 19 (63.3%) |  30   
#> b       | 16 (53.3%) | 14 (46.7%) |  30   
#> c       | 18 (45.0%) | 22 (55.0%) |  40   
#> --------+------------+------------+-------
#> Total   | 45 (45.0%) | 55 (55.0%) |  100  
#> 
#> Chi-squared: X²(2) = 1.684, p = 0.431
#> Fisher's Exact: p = 0.449
tab(example_data, group1, group2, show = "col")
#>         |          group2           |       
#> group1  |   female    |    male     | Total 
#> --------+-------------+-------------+-------
#> a       | 11 (24.4%)  | 19 (34.5%)  |  30   
#> b       | 16 (35.6%)  | 14 (25.5%)  |  30   
#> c       | 18 (40.0%)  | 22 (40.0%)  |  40   
#> --------+-------------+-------------+-------
#> Total   | 45 (100.0%) | 55 (100.0%) |  100  
#> 
#> Chi-squared: X²(2) = 1.684, p = 0.431
#> Fisher's Exact: p = 0.449
tab(example_data, group1, group2, test = "none")
#>         |         group2          |       
#> group1  |   female   |    male    | Total 
#> --------+------------+------------+-------
#> a       | 11 (36.7%) | 19 (63.3%) |  30   
#> b       | 16 (53.3%) | 14 (46.7%) |  30   
#> c       | 18 (45.0%) | 22 (55.0%) |  40   
#> --------+------------+------------+-------
#> Total   | 45 (45.0%) | 55 (55.0%) |  100  
result <- tab(example_data, group1, group2)
#>         |         group2          |       
#> group1  |   female   |    male    | Total 
#> --------+------------+------------+-------
#> a       | 11 (36.7%) | 19 (63.3%) |  30   
#> b       | 16 (53.3%) | 14 (46.7%) |  30   
#> c       | 18 (45.0%) | 22 (55.0%) |  40   
#> --------+------------+------------+-------
#> Total   | 45 (45.0%) | 55 (55.0%) |  100  
#> 
#> Chi-squared: X²(2) = 1.684, p = 0.431
#> Fisher's Exact: p = 0.449
```
