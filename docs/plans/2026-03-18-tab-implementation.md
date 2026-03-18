# `tab` Redesign Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Rewrite `tab()` to print a Stata-style formatted cross-tabulation table and return a wide tibble invisibly.

**Architecture:** The function computes counts and percentages into matrices, passes them to a print helper (`print_tab_table()`) that builds the formatted output via `cat()`, then constructs and returns a wide tibble invisibly. No new package dependencies.

**Tech Stack:** Base R string formatting (`formatC`, `strrep`, `cat`), `rlang` for NSE, `dplyr::pull`, `tibble::as_tibble`, `stats::chisq.test` / `fisher.test`.

---

### Task 1: Create test infrastructure

**Files:**
- Create: `tests/testthat/test-tab.R`

This package has `testthat (>= 3.0.0)` in Suggests and `Config/testthat/edition: 3` in DESCRIPTION, but no `tests/` directory yet.

**Step 1: Create the test directory structure**

```bash
mkdir -p "tests/testthat"
```

Then create `tests/testthat/test-tab.R` with this content:

```r
library(sumvar)
library(dplyr)

# Reproducible test data
set.seed(42)
test_df <- tibble::tibble(
  group  = sample(c("a", "b", "c"), 90, replace = TRUE),
  sex    = sample(c("female", "male"), 90, replace = TRUE),
  animal = sample(c("cat", "dog", "fish"), 90, replace = TRUE)
)
# Add some NAs
test_df$group[1:5] <- NA
test_df$sex[6:8]   <- NA
```

**Step 2: Verify testthat runs**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-tab.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]` (no tests yet, no errors).

**Step 3: Commit**

```bash
git add tests/testthat/test-tab.R
git commit -m "Add test infrastructure for tab redesign"
```

---

### Task 2: Implement core data computation and write tests for it

**Files:**
- Modify: `R/tab.R` (full rewrite)
- Modify: `tests/testthat/test-tab.R`

The core of `tab()` computes: count matrix, row-percentage matrix, column-percentage matrix, and runs hypothesis tests. These are testable independently of the print output.

**Step 1: Write failing tests**

Add to `tests/testthat/test-tab.R`:

```r
test_that("tab returns a tibble invisibly", {
  result <- tab(test_df, group, sex)
  expect_s3_class(result, "tbl_df")
})

test_that("tab returns correct column names for show='row' (default)", {
  result <- tab(test_df, group, sex)
  expect_true("female_n"   %in% names(result))
  expect_true("female_pct" %in% names(result))
  expect_true("male_n"     %in% names(result))
  expect_true("male_pct"   %in% names(result))
  expect_true("total_n"    %in% names(result))
})

test_that("tab excludes NAs from both variables", {
  result <- tab(test_df, group, sex)
  # group has 5 NAs, sex has 3 NAs, so grand total < 90
  grand <- result$total_n[nrow(result)]  # last row = Total
  expect_lt(grand, 90)
})

test_that("tab row percentages sum to 100 per row", {
  result <- tab(test_df, group, sex)
  # exclude the Total row (last row)
  data_rows <- result[result[[1]] != "Total", ]
  row_pct_sums <- data_rows$female_pct + data_rows$male_pct
  expect_true(all(abs(row_pct_sums - 100) < 0.01))
})

test_that("tab col percentages sum to 100 per column", {
  result <- tab(test_df, group, sex, show = "col")
  # exclude the Total row
  data_rows <- result[result[[1]] != "Total", ]
  col_pct_sum <- sum(data_rows$female_pct)
  expect_lt(abs(col_pct_sum - 100), 0.01)
})

test_that("tab show='n' has no pct columns", {
  result <- tab(test_df, group, sex, show = "n")
  expect_false(any(grepl("_pct", names(result))))
})

test_that("tab totals=FALSE has no total_n column and no Total row", {
  result <- tab(test_df, group, sex, totals = FALSE)
  expect_false("total_n" %in% names(result))
  expect_false("Total" %in% result[[1]])
})

test_that("tab preserves factor level ordering", {
  df_f <- test_df %>%
    mutate(group = factor(group, levels = c("c", "b", "a")))
  result <- tab(df_f, group, sex)
  # First three rows should be c, b, a (not a, b, c)
  expect_equal(result[[1]][1:3], c("c", "b", "a"))
})

test_that("tab chi-squared test appears in first row only", {
  result <- tab(test_df, group, sex, test = "chi")
  expect_true("test"      %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value"   %in% names(result))
  expect_equal(result$test[1], "Chi-squared")
  expect_true(all(is.na(result$test[-1])))
})

test_that("tab with no test has no test columns", {
  result <- tab(test_df, group, sex)
  expect_false("test" %in% names(result))
})
```

**Step 2: Run tests — all should fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-tab.R')"
```

Expected: multiple FAIL (function exists but wrong interface).

**Step 3: Rewrite `R/tab.R` completely**

Replace the entire file with:

```r
utils::globalVariables("N")

#' Create a cross-tabulation of two categorical variables
#'
#' @description
#' Creates a cross-tabulation of two categorical variables with row or column
#' percentages, row and column totals, and optional hypothesis tests. Prints a
#' formatted table to the console (similar to Stata's \code{tab} command) with
#' the column variable name displayed as a spanning header above its levels.
#'
#' @param data The data frame or tibble.
#' @param variable1 The row variable (first categorical variable).
#' @param variable2 The column variable (second categorical variable).
#' @param show What to display in each cell: \code{"row"} (default) shows
#'   \code{n (row\%)}, \code{"col"} shows \code{n (col\%)}, \code{"n"} shows
#'   counts only.
#' @param test Optional hypothesis test: \code{"chi"} for chi-squared,
#'   \code{"exact"} for Fisher's exact test (falls back to simulated p-value
#'   for large tables).
#' @param totals If \code{TRUE} (default), includes a Total column (row totals)
#'   and a Total row (column totals). Set to \code{FALSE} to suppress both.
#' @param dp Number of decimal places for percentages. Default is \code{1}.
#'
#' @return A wide-format tibble (invisibly) with:
#'   \itemize{
#'     \item First column: levels of \code{variable1} (plus \code{"Total"} if
#'       \code{totals = TRUE}).
#'     \item For each level of \code{variable2}: \code{{level}_n} (integer count)
#'       and, when \code{show != "n"}, \code{{level}_pct} (numeric percentage).
#'     \item \code{total_n}: row totals (when \code{totals = TRUE}).
#'     \item \code{test}, \code{statistic}, \code{p_value}: hypothesis test
#'       result on the first row, \code{NA} elsewhere (when \code{test != "none"}).
#'   }
#'
#' @importFrom rlang enquo as_label
#' @importFrom dplyr pull
#' @importFrom tibble as_tibble
#' @importFrom stats chisq.test fisher.test
#'
#' @examples
#' example_data <- dplyr::tibble(
#'   group1 = sample(c("a", "b", "c"), 100, replace = TRUE),
#'   group2 = sample(c("male", "female"), 100, replace = TRUE)
#' )
#' tab(example_data, group1, group2)
#' tab(example_data, group1, group2, show = "col")
#' tab(example_data, group1, group2, test = "chi")
#' result <- tab(example_data, group1, group2)
#' @export
tab <- function(data, variable1, variable2,
                show   = c("row", "col", "n"),
                test   = c("none", "chi", "exact"),
                totals = TRUE,
                dp     = 1) {
  show <- match.arg(show)
  test <- match.arg(test)

  var1_q     <- rlang::enquo(variable1)
  var2_q     <- rlang::enquo(variable2)
  var1_label <- rlang::as_label(var1_q)
  var2_label <- rlang::as_label(var2_q)

  v1 <- dplyr::pull(data, !!var1_q)
  v2 <- dplyr::pull(data, !!var2_q)

  # Preserve factor level ordering; otherwise sort unique non-NA values
  row_levels <- if (is.factor(v1)) as.character(levels(v1)) else
    sort(as.character(unique(na.omit(v1))))
  col_levels <- if (is.factor(v2)) as.character(levels(v2)) else
    sort(as.character(unique(na.omit(v2))))

  # Exclude NAs in either variable
  mask <- !is.na(v1) & !is.na(v2)
  v1c  <- factor(as.character(v1[mask]), levels = row_levels)
  v2c  <- factor(as.character(v2[mask]), levels = col_levels)

  # Count matrix: rows = row_levels, cols = col_levels
  count_tbl <- table(v1c, v2c)
  count_mat <- matrix(as.integer(count_tbl),
                      nrow = length(row_levels),
                      ncol = length(col_levels))

  row_sums <- rowSums(count_mat)
  col_sums <- colSums(count_mat)
  grand_n  <- sum(count_mat)

  # Row and column percentage matrices
  row_pct_mat <- count_mat / ifelse(row_sums == 0, 1, row_sums) * 100
  col_pct_mat <- sweep(count_mat, 2,
                       ifelse(col_sums == 0, 1, col_sums), "/") * 100

  # Choose which percentage matrix drives cell display
  pct_mat <- switch(show,
    row = row_pct_mat,
    col = col_pct_mat,
    n   = NULL
  )

  # Hypothesis test
  test_result <- NULL
  if (test != "none") {
    cont_tbl <- table(v1c, v2c)
    if (test == "chi") {
      res <- stats::chisq.test(cont_tbl)
      test_result <- list(
        name      = "Chi-squared",
        statistic = unname(res$statistic),
        df        = unname(res$parameter),
        p_value   = res$p.value
      )
    } else {
      simulated <- FALSE
      res <- tryCatch(
        stats::fisher.test(cont_tbl),
        error = function(e) {
          simulated <<- TRUE
          stats::fisher.test(cont_tbl, simulate.p.value = TRUE, B = 1e4)
        }
      )
      test_result <- list(
        name      = if (simulated) "Fisher's Exact (Simulated)" else "Fisher's Exact",
        statistic = if (!is.null(res$statistic)) unname(res$statistic) else NA_real_,
        df        = NA_real_,
        p_value   = res$p.value
      )
    }
  }

  # Print formatted table
  print_tab_table(
    count_mat  = count_mat,
    pct_mat    = pct_mat,
    row_levels = row_levels,
    col_levels = col_levels,
    var1_label = var1_label,
    var2_label = var2_label,
    col_sums   = col_sums,
    grand_n    = grand_n,
    show       = show,
    totals     = totals,
    dp         = dp,
    test_result = test_result
  )

  # Build and return wide tibble
  result <- build_tab_tibble(
    count_mat  = count_mat,
    pct_mat    = pct_mat,
    row_levels = row_levels,
    col_levels = col_levels,
    var1_label = var1_label,
    col_sums   = col_sums,
    grand_n    = grand_n,
    show       = show,
    totals     = totals,
    dp         = dp,
    test_result = test_result
  )

  invisible(result)
}

# Internal: build wide-format return tibble
build_tab_tibble <- function(count_mat, pct_mat, row_levels, col_levels,
                             var1_label, col_sums, grand_n,
                             show, totals, dp, test_result) {
  nr <- length(row_levels)
  nc <- length(col_levels)

  df <- data.frame(row_var = row_levels, stringsAsFactors = FALSE)
  names(df)[1] <- var1_label

  for (j in seq_len(nc)) {
    lev <- col_levels[j]
    df[[paste0(lev, "_n")]] <- count_mat[, j]
    if (show != "n") {
      df[[paste0(lev, "_pct")]] <- round(pct_mat[, j], dp)
    }
  }

  if (totals) {
    df[["total_n"]] <- rowSums(count_mat)

    # Build total row
    tot <- data.frame(row_var = "Total", stringsAsFactors = FALSE)
    names(tot)[1] <- var1_label
    for (j in seq_len(nc)) {
      lev <- col_levels[j]
      tot[[paste0(lev, "_n")]] <- col_sums[j]
      if (show == "row") {
        tot[[paste0(lev, "_pct")]] <- round(col_sums[j] / grand_n * 100, dp)
      } else if (show == "col") {
        tot[[paste0(lev, "_pct")]] <- round(100, dp)
      }
    }
    tot[["total_n"]] <- grand_n
    df <- rbind(df, tot)
  }

  # Hypothesis test columns: first row only
  if (!is.null(test_result)) {
    df$test      <- NA_character_
    df$statistic <- NA_real_
    df$p_value   <- NA_real_
    df$test[1]      <- test_result$name
    df$statistic[1] <- test_result$statistic
    df$p_value[1]   <- test_result$p_value
  }

  tibble::as_tibble(df)
}

# Internal: print the Stata-style formatted table
print_tab_table <- function(count_mat, pct_mat, row_levels, col_levels,
                            var1_label, var2_label, col_sums, grand_n,
                            show, totals, dp, test_result) {
  nr <- length(row_levels)
  nc <- length(col_levels)

  # Format a single cell
  fmt_cell <- function(n, pct) {
    if (show == "n") return(as.character(n))
    paste0(n, " (", formatC(pct, digits = dp, format = "f"), "%)")
  }

  # Interior cell strings [nr x nc]
  cell_str <- matrix("", nrow = nr, ncol = nc)
  for (i in seq_len(nr))
    for (j in seq_len(nc))
      cell_str[i, j] <- fmt_cell(count_mat[i, j], if (!is.null(pct_mat)) pct_mat[i, j] else 0)

  # Total row cell strings
  if (totals) {
    tot_row_str <- vapply(seq_len(nc), function(j) {
      pct <- if (show == "row") col_sums[j] / grand_n * 100
             else if (show == "col") 100
             else 0
      fmt_cell(col_sums[j], pct)
    }, character(1))
    row_tot_str <- as.character(rowSums(count_mat))
    grand_str   <- as.character(grand_n)
  }

  # ---- Column widths (content + 1 space padding each side) ----
  all_col_content <- if (totals) rbind(cell_str, tot_row_str) else cell_str
  col_w <- pmax(
    nchar(col_levels),
    apply(all_col_content, 2, function(x) max(nchar(x)))
  ) + 2

  # Row label width
  all_labels <- c(var1_label, row_levels, if (totals) "Total")
  row_w <- max(nchar(all_labels)) + 2

  # Total column width
  tot_col_w <- if (totals)
    max(nchar("Total"), nchar(c(row_tot_str, grand_str))) + 2
  else 0L

  # ---- Helpers ----
  # Left-align string in field of width w
  lp <- function(s, w) formatC(as.character(s), width = -w, flag = "-")
  # Centre string in field of width w
  cp <- function(s, w) {
    s   <- as.character(s)
    pad <- max(0L, w - nchar(s))
    paste0(strrep(" ", floor(pad / 2)), s, strrep(" ", ceiling(pad / 2)))
  }

  # inner_w: total width of the column section (all col fields joined by "|")
  inner_w <- sum(col_w) + (nc - 1L)

  # Build a full data/header line:
  # lp(label, row_w) + "|" + col1 + "|" + col2 + ... + ["|" + total]
  build_line <- function(label, cells, total_cell = NULL) {
    col_part <- paste(
      vapply(seq_len(nc), function(j) cp(cells[j], col_w[j]), character(1)),
      collapse = "|"
    )
    line <- paste0(lp(label, row_w), "|", col_part)
    if (totals && !is.null(total_cell))
      line <- paste0(line, "|", cp(total_cell, tot_col_w))
    line
  }

  # Separator line: dashes with "+" where "|" appears in data lines
  sep_inner <- paste(vapply(col_w, function(w) strrep("-", w), character(1)),
                     collapse = "+")
  sep <- paste0(strrep("-", row_w), "+", sep_inner)
  if (totals) sep <- paste0(sep, "+", strrep("-", tot_col_w))

  # ---- Header row 1: var2_label spanning all column cells ----
  line1 <- paste0(strrep(" ", row_w), "|", cp(var2_label, inner_w))
  if (totals) line1 <- paste0(line1, "|", strrep(" ", tot_col_w))

  # ---- Header row 2: var1_label | col level names | "Total" ----
  line2 <- build_line(var1_label, col_levels,
                      total_cell = if (totals) "Total" else NULL)

  # ---- Data rows ----
  data_lines <- vapply(seq_len(nr), function(i) {
    build_line(row_levels[i], cell_str[i, ],
               total_cell = if (totals) row_tot_str[i] else NULL)
  }, character(1))

  # ---- Total row ----
  total_line <- if (totals)
    build_line("Total", tot_row_str, total_cell = grand_str)
  else NULL

  # ---- Print ----
  cat(line1, "\n", sep = "")
  cat(line2, "\n", sep = "")
  cat(sep,   "\n", sep = "")
  for (ln in data_lines) cat(ln, "\n", sep = "")
  if (totals) {
    cat(sep,        "\n", sep = "")
    cat(total_line, "\n", sep = "")
  }

  # ---- Test result ----
  if (!is.null(test_result)) {
    t <- test_result
    cat("\n")
    if (!is.na(t$df) && !is.na(t$statistic)) {
      cat(sprintf("%s: X\u00b2(%d) = %.3f, p = %.3f\n",
                  t$name, as.integer(t$df), t$statistic, t$p_value))
    } else if (!is.na(t$statistic)) {
      cat(sprintf("%s: OR = %.3f, p = %.3f\n",
                  t$name, t$statistic, t$p_value))
    } else {
      cat(sprintf("%s: p = %.3f\n", t$name, t$p_value))
    }
  }
}
```

**Step 4: Run tests — all should pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-tab.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ]`

**Step 5: Quick smoke test of printed output**

```r
library(sumvar)
df <- dplyr::tibble(
  group = sample(c("a","b","c"), 60, replace=TRUE),
  sex   = sample(c("female","male"), 60, replace=TRUE)
)
tab(df, group, sex)
tab(df, group, sex, show = "col")
tab(df, group, sex, show = "n")
tab(df, group, sex, totals = FALSE)
tab(df, group, sex, test = "chi")
```

Verify visually:
- `sex` label spans above `female | male` on row 1
- `|` characters in data rows align with `+` in separator rows
- `show = "col"` cell percentages sum to ~100 per column
- `show = "n"` has no parenthetical `(%)`
- `totals = FALSE` removes the right Total column and bottom Total row
- Chi-squared result appears below the table

**Step 6: Commit**

```bash
git add R/tab.R tests/testthat/test-tab.R
git commit -m "Rewrite tab() with Stata-style print and wide tibble return"
```

---

### Task 3: Add print alignment test

**Files:**
- Modify: `tests/testthat/test-tab.R`

The formatter must ensure `|` in data rows aligns with `+` in separator rows.

**Step 1: Write the alignment test**

Add to `tests/testthat/test-tab.R`:

```r
test_that("tab printed output has aligned separators", {
  df <- tibble::tibble(
    group = c("a", "a", "b", "b"),
    sex   = c("female", "male", "female", "male")
  )
  out <- capture.output(tab(df, group, sex))

  # Row 1: spanning header (var2 label)
  # Row 2: column headers
  # Row 3: separator
  # Row 4+: data rows
  sep_line  <- out[3]
  data_line <- out[4]

  # Find positions of "+" in separator and "|" in data line
  sep_plus_pos  <- gregexpr("\\+", sep_line)[[1]]
  data_pipe_pos <- gregexpr("\\|", data_line)[[1]]

  expect_equal(sep_plus_pos, data_pipe_pos)
})
```

**Step 2: Run test**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-tab.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ]`

**Step 3: Commit**

```bash
git add tests/testthat/test-tab.R
git commit -m "Add separator alignment test for tab printed output"
```

---

### Task 4: Update `@importFrom` and remove unused globals

**Files:**
- Modify: `R/tab.R` (top of file only)

The rewrite no longer uses `tidyr::pivot_wider`, `dplyr::count`, `dplyr::rename`, `dplyr::where`, `dplyr::select`, `dplyr::arrange`, `dplyr::mutate`, `dplyr::group_by`, `dplyr::ungroup`, `dplyr::bind_rows`, `dplyr::if_else`, `dplyr::n`, `dplyr::desc`. It also no longer uses the `N` global variable. Remove those.

The correct imports for the new implementation:

```r
#' @importFrom rlang enquo as_label
#' @importFrom dplyr pull
#' @importFrom tibble as_tibble
#' @importFrom stats chisq.test fisher.test
```

Also remove `utils::globalVariables("N")` from the top of the file since `N` is no longer referenced.

**Step 1: Verify no R CMD CHECK notes**

```bash
Rscript -e "rcmdcheck::rcmdcheck(args = '--no-tests')"
```

Expected: no new NOTES about unused imports or undefined globals.

**Step 2: Commit**

```bash
git add R/tab.R
git commit -m "Clean up imports in tab.R after rewrite"
```

---

### Task 5: Update vignette

**Files:**
- Modify: `vignettes/exploring-data-with-sumvar.Rmd`

The vignette currently has no `tab` example. Add one showing the main usage patterns.

**Step 1: Add a Two-way tables section to the vignette**

Find the section after `tab1` (around line 85) and add before `# Check for duplicate`:

```rmd
# Two-way tables

**tab()** creates a cross-tabulation of two categorical variables. By default it
shows counts and row percentages, with row and column totals.

```{r crosstab}
df_tab <- dplyr::tibble(
  treatment = sample(c("control", "treatment"), 100, replace = TRUE),
  outcome   = sample(c("improved", "stable", "worse"), 100, replace = TRUE)
)

df_tab %>% tab(treatment, outcome)
df_tab %>% tab(treatment, outcome, show = "col")  # column percentages
df_tab %>% tab(treatment, outcome, test = "chi")  # with chi-squared test
result <- df_tab %>% tab(treatment, outcome)      # save as tibble
```
```

**Step 2: Commit**

```bash
git add vignettes/exploring-data-with-sumvar.Rmd
git commit -m "Add two-way table examples to vignette"
```
