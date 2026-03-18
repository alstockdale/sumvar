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
#' @param test Hypothesis test(s) to report: \code{"both"} (default) runs both
#'   chi-squared and Fisher's exact; \code{"chi"} for chi-squared only;
#'   \code{"exact"} for Fisher's exact only (falls back to simulated p-value
#'   for large tables); \code{"none"} to suppress tests.
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
#'     \item \code{p_chi}, \code{p_fisher}: p-values on the first row, \code{NA}
#'       elsewhere (when \code{test = "both"}; individual tests add \code{test},
#'       \code{statistic}, \code{p_value} instead).
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
#' tab(example_data, group1, group2, test = "none")
#' result <- tab(example_data, group1, group2)
#' @export
tab <- function(data, variable1, variable2,
                show   = c("row", "col", "n"),
                test   = c("both", "chi", "exact", "none"),
                totals = TRUE,
                dp     = 1) {
  if (missing(variable2)) {
    stop("To use tab, select two variables to cross-tabulate. For a single variable, use the tab1() command instead.",
         call. = FALSE)
  }

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
  mask       <- !is.na(v1) & !is.na(v2)
  n_excluded <- sum(!mask)
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

  # Hypothesis tests
  run_chi <- test %in% c("both", "chi")
  run_fisher <- test %in% c("both", "exact")

  chi_result <- NULL
  if (run_chi) {
    res <- stats::chisq.test(count_tbl)
    chi_result <- list(
      name      = "Chi-squared",
      statistic = unname(res$statistic),
      df        = unname(res$parameter),
      p_value   = res$p.value
    )
  }

  fisher_result <- NULL
  if (run_fisher) {
    simulated <- FALSE
    res <- tryCatch(
      stats::fisher.test(count_tbl),
      error = function(e) {
        simulated <<- TRUE
        stats::fisher.test(count_tbl, simulate.p.value = TRUE, B = 1e4)
      }
    )
    fisher_result <- list(
      name      = if (simulated) "Fisher's Exact (Simulated)" else "Fisher's Exact",
      statistic = if (!is.null(res$statistic)) unname(res$statistic) else NA_real_,
      df        = NA_real_,
      p_value   = res$p.value
    )
  }

  # For single-test calls, set test_result for backward-compatible tibble columns
  test_result <- if (test == "chi") chi_result else if (test == "exact") fisher_result else NULL

  # Print formatted table
  print_tab_table(
    count_mat     = count_mat,
    pct_mat       = pct_mat,
    row_levels    = row_levels,
    col_levels    = col_levels,
    var1_label    = var1_label,
    var2_label    = var2_label,
    col_sums      = col_sums,
    grand_n       = grand_n,
    show          = show,
    totals        = totals,
    dp            = dp,
    chi_result    = chi_result,
    fisher_result = fisher_result,
    test_result   = test_result,
    n_excluded    = n_excluded
  )

  # Build and return wide tibble
  result <- build_tab_tibble(
    count_mat     = count_mat,
    pct_mat       = pct_mat,
    row_levels    = row_levels,
    col_levels    = col_levels,
    var1_label    = var1_label,
    col_sums      = col_sums,
    grand_n       = grand_n,
    show          = show,
    totals        = totals,
    dp            = dp,
    chi_result    = chi_result,
    fisher_result = fisher_result,
    test_result   = test_result
  )

  invisible(result)
}

# Internal: build wide-format return tibble
build_tab_tibble <- function(count_mat, pct_mat, row_levels, col_levels,
                             var1_label, col_sums, grand_n,
                             show, totals, dp,
                             chi_result = NULL, fisher_result = NULL,
                             test_result = NULL) {
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
    df[["total_n"]] <- as.integer(rowSums(count_mat))

    # Build total row
    tot <- data.frame(row_var = "Total", stringsAsFactors = FALSE)
    names(tot)[1] <- var1_label
    for (j in seq_len(nc)) {
      lev <- col_levels[j]
      tot[[paste0(lev, "_n")]] <- as.integer(col_sums[j])
      if (show == "row") {
        tot[[paste0(lev, "_pct")]] <- round(col_sums[j] / grand_n * 100, dp)
      } else if (show == "col") {
        tot[[paste0(lev, "_pct")]] <- round(100, dp)
      }
    }
    tot[["total_n"]] <- as.integer(grand_n)
    df <- rbind(df, tot)
  }

  # Hypothesis test columns: first row only
  if (!is.null(chi_result) && !is.null(fisher_result)) {
    # "both" mode: separate p_chi / p_fisher columns
    df$p_chi    <- NA_real_
    df$p_fisher <- NA_real_
    df$p_chi[1]    <- chi_result$p_value
    df$p_fisher[1] <- fisher_result$p_value
  } else if (!is.null(test_result)) {
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
                            show, totals, dp,
                            chi_result = NULL, fisher_result = NULL,
                            test_result = NULL, n_excluded = 0L) {
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
      cell_str[i, j] <- fmt_cell(count_mat[i, j],
                                 if (!is.null(pct_mat)) pct_mat[i, j] else 0)

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
  lp <- function(s, w) formatC(as.character(s), width = -w, flag = "-")
  cp <- function(s, w) {
    s   <- as.character(s)
    pad <- max(0L, w - nchar(s))
    paste0(strrep(" ", floor(pad / 2)), s, strrep(" ", ceiling(pad / 2)))
  }

  inner_w <- sum(col_w) + (nc - 1L)

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

  sep_inner <- paste(vapply(col_w, function(w) strrep("-", w), character(1)),
                     collapse = "+")
  sep <- paste0(strrep("-", row_w), "+", sep_inner)
  if (totals) sep <- paste0(sep, "+", strrep("-", tot_col_w))

  # Header row 1: var2_label spanning all column cells
  var2_display <- if (nchar(var2_label) > inner_w)
    paste0(substr(var2_label, 1, inner_w - 2), "..")
  else var2_label
  line1 <- paste0(strrep(" ", row_w), "|", cp(var2_display, inner_w))
  if (totals) line1 <- paste0(line1, "|", strrep(" ", tot_col_w))

  # Header row 2: var1_label | col level names | "Total"
  line2 <- build_line(var1_label, col_levels,
                      total_cell = if (totals) "Total" else NULL)

  # Data rows
  data_lines <- vapply(seq_len(nr), function(i) {
    build_line(row_levels[i], cell_str[i, ],
               total_cell = if (totals) row_tot_str[i] else NULL)
  }, character(1))

  # Total row
  total_line <- if (totals)
    build_line("Total", tot_row_str, total_cell = grand_str)
  else NULL

  # Print
  cat(line1, "\n", sep = "")
  cat(line2, "\n", sep = "")
  cat(sep,   "\n", sep = "")
  for (ln in data_lines) cat(ln, "\n", sep = "")
  if (totals) {
    cat(sep,        "\n", sep = "")
    cat(total_line, "\n", sep = "")
  }

  # Missing data note
  if (n_excluded > 0L) {
    cat(sprintf("\nNote: %d observation%s excluded due to missing values.\n",
                n_excluded, if (n_excluded == 1L) "" else "s"))
  }

  # Print test results
  print_one_test <- function(tr) {
    if (!is.na(tr$df) && !is.na(tr$statistic)) {
      cat(sprintf("%s: X\u00b2(%d) = %.3f, p = %.3f\n",
                  tr$name, as.integer(tr$df), tr$statistic, tr$p_value))
    } else if (!is.na(tr$statistic)) {
      cat(sprintf("%s: OR = %.3f, p = %.3f\n",
                  tr$name, tr$statistic, tr$p_value))
    } else {
      cat(sprintf("%s: p = %.3f\n", tr$name, tr$p_value))
    }
  }

  if (!is.null(chi_result) || !is.null(fisher_result) || !is.null(test_result)) {
    cat("\n")
    if (!is.null(chi_result))    print_one_test(chi_result)
    if (!is.null(fisher_result)) print_one_test(fisher_result)
    if (!is.null(test_result) && is.null(chi_result) && is.null(fisher_result))
      print_one_test(test_result)
  }
}
