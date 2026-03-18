# sumvar 0.2.0 — Resubmission

## Resubmission notes

This is a resubmission. Version 0.1.0 is currently on CRAN.

## Test environments

* Local Windows 11, R 4.5.2
* R-hub (Windows, Linux, macOS)
* win-builder (release and devel)

## R CMD check results

0 errors | 0 warnings | 1 note

> checking for future file timestamps ... NOTE
>   unable to verify current time

This note is caused by a transient network issue when R CMD check attempts
to verify the system clock. It is not reproducible and is unrelated to the
package code.

## Method References

There are no published references describing the methods in this package.
The package provides convenience wrappers around standard summary statistics
(mean, median, IQR, SD, confidence intervals) and standard hypothesis tests
(t-test, Wilcoxon, ANOVA, Kruskal-Wallis, chi-squared, Fisher's exact)
already available in base R and common CRAN packages.

## Notes

* The `explorer()` function generates an HTML or PDF report via `rmarkdown::render()`.
  PDF output requires a LaTeX distribution (e.g. TinyTeX), but this is optional and
  documented in the function's help page. The package functions correctly without LaTeX
  when using the default HTML output.
