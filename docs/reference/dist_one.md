# Examine the distribution of a single continous variable

Summarises the median, interquartile range, mean, standard deviation,
confidence intervals of the mean and plots the density and a histogram.
Default option for a density plot and histogram can be overriden with
the options "bee" and "box".

The function accepts an input from a dplyr pipe "%\>%" and outputs the
results as a tibble.

## Usage

``` r
dist_one(data, ...)
```

## Arguments

- data:

  The data frame or tibble

- ...:

  The variable you would like to summarise

  - Default, with no additional arguments: density plot and histogram

  - Add the option "box" to show a boxplot and violin plot eg. data %\>%
    dist_one(var, box)

  - Add the option "bee" to show a beeswarm plot eg. data %\>%
    dist_one(var, bee)

## Value

A tibble with a summary of the variable median, interquartile range,
mean, SD, confidence intervals of the mean (using the Z distribution),
and plots.

## Examples

``` r
example_data <- dplyr::tibble(id = 1:100, age = rnorm(100, mean = 30, sd = 10))
dist_one(example_data, age) # Default, with histogram and density plot
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#> # A tibble: 1 × 10
#>       n median   p25   p75  mean    sd ci_lower ci_upper   min   max
#>   <int>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl>
#> 1   100   30.9  26.4  36.2  30.7  10.5     28.6     32.8  3.88  57.6
dist_one(example_data, age, box) # Option to show a boxplot and violin plot

#> # A tibble: 1 × 10
#>       n median   p25   p75  mean    sd ci_lower ci_upper   min   max
#>   <int>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl>
#> 1   100   30.9  26.4  36.2  30.7  10.5     28.6     32.8  3.88  57.6
dist_one(example_data, age, bee) # Option to show a beeswarm plot (using ggbeeswarm)

#> # A tibble: 1 × 10
#>       n median   p25   p75  mean    sd ci_lower ci_upper   min   max
#>   <int>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl>
#> 1   100   30.9  26.4  36.2  30.7  10.5     28.6     32.8  3.88  57.6
summary <- dist_one(example_data, age) # Save summary statistics as a tibble.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

# Note that this function accepts a pipe input
# eg. example_data %>% dist_one(age)
```
