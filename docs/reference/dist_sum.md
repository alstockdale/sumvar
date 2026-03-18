# Explore a continuous variable

Summarises a continuous variable, returning a tibble of descriptive
statistics and a plot. When a grouping variable is supplied, results are
stratified by group.

For two groups, a t-test and Wilcoxon rank-sum test are reported. For
three or more groups, a one-way ANOVA and Kruskal-Wallis test are
reported.

## Usage

``` r
dist_sum(data, var, by = NULL)
```

## Arguments

- data:

  The data frame or tibble

- var:

  The continuous variable to summarise

- by:

  Optional grouping variable

## Value

A tibble with one row per group (or one row when ungrouped) containing
the following columns:

- `n`:

  Number of non-missing observations.

- `n_miss`:

  Number of missing (NA) values.

- `median`:

  Median value.

- `p25`, `p75`:

  25th and 75th percentiles (interquartile range boundaries).

- `mean`:

  Arithmetic mean.

- `sd`:

  Standard deviation.

- `ci_lower`, `ci_upper`:

  Lower and upper bounds of the 95% confidence interval for the mean.
  Uses the t-distribution when n \< 30, and the Z-distribution when n
  \>= 30.

- `min`, `max`:

  Minimum and maximum observed values.

- `n_outliers`:

  Count of values more than 1.5 x IQR below Q1 or above Q3 (Tukey fence
  method).

- `shapiro_p`:

  P-value from the Shapiro-Wilk test of normality. Returns `NA` when n
  \< 3 or n \> 5000 (outside the valid range of the test).

- `normal`:

  Logical. `TRUE` if `shapiro_p > 0.05`, indicating no significant
  departure from normality at the 5% level.

- `p_ttest`:

  Shown when two groups are compared. P-value from an independent
  samples t-test, testing whether the means of the two groups differ.
  Assumes approximately normal distributions or large samples. All
  p-values are reported on the first row only; remaining rows contain
  `NA`.

- `p_wilcox`:

  Shown when two groups are compared. P-value from the Wilcoxon rank-sum
  test (Mann-Whitney U test), a non-parametric alternative to the
  t-test. Preferred over `p_ttest` when data are skewed, ordinal, or
  contain outliers, as it compares ranks rather than means and makes no
  distributional assumptions.

- `p_anova`:

  Shown when three or more groups are compared. P-value from a one-way
  analysis of variance (ANOVA) F-test, testing whether at least one
  group mean differs from the others. Assumes approximately normal
  distributions and equal variances across groups.

- `p_kruskal`:

  Shown when three or more groups are compared. P-value from the
  Kruskal-Wallis test, a non-parametric alternative to one-way ANOVA.
  Preferred over `p_anova` when data are skewed or the normality
  assumption is not met, as it compares rank distributions and makes no
  distributional assumptions.

## Examples

``` r
example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10),
                              group = sample(c("a", "b", "c", "d"),
                              size = 100, replace = TRUE))
dist_sum(example_data, age, group)

#> # A tibble: 4 × 17
#>   group     n n_miss median   p25   p75  mean    sd   min   max n_outliers
#>   <chr> <int>  <int>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>      <int>
#> 1 a        34      0   35.8  28.2  41.4  33.4 11.6   3.49  54.6          2
#> 2 b        26      0   29.2  24.7  35.6  29.1  8.76  8.56  42.5          0
#> 3 c        18      0   28.6  21.4  34.3  28.9 11.9   7.82  48.6          0
#> 4 d        22      0   25.4  20.2  29.6  25.3 10.6  -7.10  48.9          2
#> # ℹ 6 more variables: shapiro_p <dbl>, ci_lower <dbl>, ci_upper <dbl>,
#> #   normal <lgl>, p_anova <dbl>, p_kruskal <dbl>
example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10),
                             sex = sample(c("male", "female"),
                             size = 100, replace = TRUE))
dist_sum(example_data, age, sex)

#> # A tibble: 2 × 17
#>   sex        n n_miss median   p25   p75  mean    sd   min   max n_outliers
#>   <chr>  <int>  <int>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>      <int>
#> 1 female    56      0   30.7  19.4  35.6  29.0  9.86  7.94  49.4          0
#> 2 male      44      0   28.8  23.1  33.6  28.3  8.07 11.7   44.8          0
#> # ℹ 6 more variables: shapiro_p <dbl>, ci_lower <dbl>, ci_upper <dbl>,
#> #   normal <lgl>, p_ttest <dbl>, p_wilcox <dbl>
summary <- dist_sum(example_data, age, sex) # Save summary statistics as a tibble.
```
