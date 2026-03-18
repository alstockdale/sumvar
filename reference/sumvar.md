# sumvar: Summarise Continuous and Categorical Variables in R

The sumvar package explores continuous and categorical variables. sumvar
brings the ease and simplicity of the "sum" and "tab" functions from
Stata to R.

- To explore a continuous variable, use
  [`dist_sum()`](https://alstockdale.github.io/sumvar/reference/dist_sum.md).
  You can stratify by a grouping variable: `df %>% dist_sum(var, group)`

- To explore dates, use
  [`dist_date()`](https://alstockdale.github.io/sumvar/reference/dist_date.md);
  usage is the same as
  [`dist_sum()`](https://alstockdale.github.io/sumvar/reference/dist_sum.md).

- To summarise a single categorical variable use
  [`tab1()`](https://alstockdale.github.io/sumvar/reference/tab1.md),
  e.g. `df %>% tab1(var)`. For a two-way table, use
  [`tab()`](https://alstockdale.github.io/sumvar/reference/tab.md), e.g.
  `df %>% tab(var1, var2)`. Both include options for frequentist
  hypothesis tests.

- Explore duplicates and missing values with with
  [`dup()`](https://alstockdale.github.io/sumvar/reference/dup.md).

All functions are tidyverse/dplyr-friendly and accept the `%>%` pipe,
outputting results as a tibble. You can save outputs for further
manipulation, e.g. `summary <- df %>% dist_sum(var)`.

## See also

Useful links:

- <https://github.com/alstockdale/sumvar>

- <https://alstockdale.github.io/sumvar/>

- Report bugs at <https://github.com/alstockdale/sumvar/issues>

## Author

**Maintainer**: Alexander Stockdale <a.stockdale@liverpool.ac.uk>
\[copyright holder\]
