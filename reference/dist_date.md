# Summarize and visualize a date variable

Summarises the minimum, maximum, median, and interquartile range of a
date variable, optionally stratified by a grouping variable. Produces a
histogram and (optionally) a density plot.

## Usage

``` r
dist_date(data, var, by = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- var:

  The date variable to summarise.

- by:

  Optional grouping variable.

## Value

A tibble with summary statistics for the date variable.

## See also

[`dist_sum`](https://alstockdale.github.io/sumvar/reference/dist_sum.md)
for continuous variables.

## Examples

``` r
# Example ungrouped
df <- tibble::tibble(
  dt = as.Date("2020-01-01") + sample(0:1000, 100, TRUE)
)
dist_date(df, dt)

#> # A tibble: 1 × 7
#>       n n_miss min        p25        median     p75        max       
#>   <int>  <int> <date>     <date>     <date>     <date>     <date>    
#> 1   100      0 2020-01-12 2020-09-20 2021-07-06 2022-03-06 2022-09-24

# Example grouped
df2 <- tibble::tibble(
  dt = as.Date("2020-01-01") + sample(0:1000, 100, TRUE),
  grp = sample(1:2, 100, TRUE)
)
dist_date(df2, dt, grp)

#> # A tibble: 2 × 8
#>     grp     n n_miss min        p25        median     p75        max       
#>   <int> <int>  <int> <date>     <date>     <date>     <date>     <date>    
#> 1     1    48      0 2020-01-13 2020-09-25 2021-04-29 2022-02-11 2022-09-24
#> 2     2    52      0 2020-01-04 2020-10-21 2021-06-02 2022-01-27 2022-08-12
```
