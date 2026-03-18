# Design: `tab` redesign

**Date:** 2026-03-18
**Status:** Approved

## Problem

The current `tab` function returns a wide tibble with columns like `N_female`,
`N_male`, `Percent_female`, `Percent_male`. The column variable name is buried
in column headers with no spanning label above them, making it hard to read —
especially compared to Stata's `tab` command which clearly shows both variable
names and a clean formatted table.

## Decisions

### Function signature

```r
tab(data, variable1, variable2,
    show   = c("row", "col", "n"),
    test   = c("none", "chi", "exact"),
    totals = TRUE,
    dp     = 1)
```

- `show`: controls cell content. `"row"` (default) = `n (row%)`, `"col"` = `n (col%)`, `"n"` = counts only.
- `totals`: `TRUE` (default) adds a right-hand Total column and a bottom Total row. `FALSE` suppresses both.
- `dp`: decimal places for percentages, default 1 (consistent with `tab1`).
- `test`: unchanged from current — `"none"`, `"chi"`, `"exact"`.

### Printed output

A Stata-style formatted table printed via `cat()`. The column variable name
spans as a header above all its column levels. `|` separators are used
consistently between columns, with `+` in separator rows aligning exactly with
`|` in data rows. Example:

```
                |          sex           |
 group          | female     | male      | Total
----------------+------------+-----------+-------
 a              | 22 (55.0%) | 18 (45.0%)|    40
 b              | 11 (36.7%) | 19 (63.3%)|    30
 c              |  8 (44.4%) | 10 (55.6%)|    18
----------------+------------+-----------+-------
 Total          | 41 (47.1%) | 47 (52.9%)|    88

Chi-squared: X²(2) = 1.83, p = 0.401
```

- Column widths are computed dynamically from content.
- All formatting uses base R `cat()` and `formatC()` — no new dependencies.
- Test result (if requested) printed below the table.
- With `totals = FALSE`: right Total column and bottom Total row are omitted.
- With `show = "col"`: percentages in cells show column % instead of row %.
- With `show = "n"`: cells show count only, no parenthetical.

### Return value

Returned **invisibly** (like `tab1`). Wide-format tibble mirroring the printed
table:

- First column: `variable1` name, containing row levels plus `"Total"` (if `totals = TRUE`)
- For each level of `variable2`: `{level}_n` (integer) and, when `show != "n"`, `{level}_pct` (numeric, rounded to `dp`)
- `total_n` column (integer row totals), present when `totals = TRUE`
- If a test was run: `test` (character), `statistic` (numeric), `p_value` (numeric) — populated on the first data row, `NA` elsewhere

### What stays the same

- Pipe-friendly (`data %>% tab(var1, var2)`)
- Factor level ordering preserved; character columns sorted alphabetically
- NAs in either variable excluded from counts
- Fisher's exact test falls back to simulated p-value for large tables (already fixed in prior session)
- `test` argument uses `match.arg()` (already fixed)
