# sumvar 0.2.0

## New features

* Added `explorer()` to generate an HTML or PDF exploratory data analysis report for an entire data frame, including summaries of all continuous, date and categorical variables, missing data, and duplicates.
* Added `dist_date()` for summarising and visualising date variables, optionally stratified by a grouping variable.
* Redesigned `tab()` with a Stata-style formatted console output. Now supports row percentages, column percentages, or counts only (`show` argument), optional row/column totals, configurable decimal places, and chi-squared and Fisher's exact hypothesis tests by default.
* `dist_sum()` now returns `n_outliers` (IQR method), `shapiro_p` (Shapiro-Wilk p-value), and `normal` (logical) for both ungrouped and grouped summaries.
* `dist_sum()` now automatically uses the t-distribution for confidence intervals when n < 30, and the Z-distribution for n >= 30.
* `tab()` now runs both chi-squared and Fisher's exact tests by default (`test = "both"`).
* `tab()` now prints a note when observations are excluded due to missing values.
* `tab()` and `tab1()` now raise informative errors when the wrong number of variables is supplied.
* `explorer()` now prompts interactively to exclude identifier columns (id, ids, pid, pids) from summaries.
* `explorer()` now includes a correlation heatmap section when two or more numeric variables are present.

## Bug fixes

* `dup()`: fixed inverted label positions in the stacked bar chart.
* `dist_sum()`: fixed incorrect test dispatch (ANOVA vs t-test) when the grouping variable contains missing values.
* `explorer()`: fixed `id_var` scope in generated Rmd; fixed CLI progress bar token names.
* `explorer()`: correctly recovers the data frame name when called via the `%>%` pipe.

# sumvar 0.1.0

* Initial release with `dist_sum()`, `tab1()`, and `dup()`.
