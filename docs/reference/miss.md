# Count missing values

Provides the frequency and percentage of missing values. Counts both NA,
and "" for character variables (empty character variables) as missing.

The function accepts an input from a dplyr pipe %\>% and returns results
as a tibble. If no variables are specified, returns a list of missing
values for the entire data frame/ tibble.

eg. data %\>% miss(variable1, variable2)

## Usage

``` r
miss(data, ..., dp = 1)
```

## Arguments

- data:

  The data frame or tibble

- ...:

  The variable(s) to assess for missing data

- dp:

  The number of decimal places for percentages

## Value

A tibble with the number of valid values (n_valid), missing values
(n_missing), together with percentages.

## Examples

``` r
example_data <- dplyr::tibble(id = 1:200, age = round(rnorm(200, mean = 30, sd = 5), digits=0),
                                          sex = sample(c("male", "female"),
                                          size = 200, replace = TRUE),)
example_data$age[sample(1:200, size = 15)] <- NA  # Replace 15 values with missing.
example_data$sex[sample(1:200, size = 30)] <- ""  # Replace 30 values with empty character vectors.
miss(example_data, id, age, sex, dp=1)
#> # A tibble: 3 × 4
#>   variable total n_missing pc_missing
#>   <chr>    <int>     <int> <chr>     
#> 1 id         200         0 0.0       
#> 2 age        200        15 7.5       
#> 3 sex        200        30 15.0      
# Note that this function accepts a pipe input
# eg. example_data %>% miss()
```
