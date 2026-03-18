# Summarise a categorial variable

Summarises frequencies and percentages for a categorical variable.

The function accepts an input from a dplyr pipe "%\>%" and outputs the
results as a tibble. eg. example_data %\>% tab1(variable)

## Usage

``` r
tab1(data, variable, ..., dp = 1)
```

## Arguments

- data:

  The data frame or tibble

- variable:

  The categorical variable you would like to summarise

- ...:

  Not used. Passing additional variables raises an informative error
  suggesting
  [`tab()`](https://alstockdale.github.io/sumvar/reference/tab.md) for
  two-way tables.

- dp:

  The number of decimal places for percentages (default=1)

## Value

A tibble with frequencies and percentages

## Examples

``` r
example_data <- dplyr::tibble(id = 1:100, group = sample(c("a", "b", "c", "d"),
                                                  size = 100, replace = TRUE))
example_data$group[sample(1:100, size = 10)] <- NA  # Replace 10 with missing
tab1(example_data, group)

#> # A tibble: 6 × 3
#>   Category Frequency Percent
#>   <chr>        <int> <chr>  
#> 1 b               26 26.0   
#> 2 c               26 26.0   
#> 3 a               21 21.0   
#> 4 d               17 17.0   
#> 5 NA              10 10.0   
#> 6 Total          100 100.0  
summary <- tab1(example_data, group) # Save summary statistics as a tibble.

#> # A tibble: 6 × 3
#>   Category Frequency Percent
#>   <chr>        <int> <chr>  
#> 1 b               26 26.0   
#> 2 c               26 26.0   
#> 3 a               21 21.0   
#> 4 d               17 17.0   
#> 5 NA              10 10.0   
#> 6 Total          100 100.0  
```
