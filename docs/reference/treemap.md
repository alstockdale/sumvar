# Summarise a categorial variable

Creates a treemap to view a categorical variable

The function accepts an input from a dplyr pipe "%\>%" and outputs the
results as a tibble. eg. example_data %\>% treemap(variable)

## Usage

``` r
treemap(data, variable)
```

## Arguments

- data:

  The data frame or tibble

- variable:

  The categorical variable you would like to summarise

## Value

A treemap

## Examples

``` r
data <- mtcars
data$gear <- as.factor(data$gear)
treemap(data, gear)

# Note that this function accepts a pipe input
# eg. example_data %>% tab1(group)
```
