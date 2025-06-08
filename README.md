<h1> The sumvar package</h1>

The sumvar package quickly explores continuous and categorical variables. <br>
sumvar aims to bring the ease and simplicity of the "sum" and "tab" functions from stata, to R.

  * **dist_sum()** is for exploring a continuous variable. Can stratify by a grouping variable, for example: `df %>% dist_sum (var, group)`.
  * **dist_date()** is for dates, use the same way as **dist_sum()**.
  * **tab1()** is to explore a single categorical variable, for example: `df %>% tab1(var)`
  * **tab()** is for a twoway table from two categorical variables, for example: `df %>% tab(var1, var2)`.
  * **dup()** explores duplicates and missing values, across a single variable, or a whole database

Both **dist_sum()** and **tab()** include options for performing frequentist hypothesis tests, have a look at the help files.<p>

All functions are tidyverse/dplyr-friendly and accept the `%>%` pipe from a tibble or data frame, outputting results as a tibble. You can save and further manipulate outputs, e.g. `summary <- df %>% dist_sum(var)`
