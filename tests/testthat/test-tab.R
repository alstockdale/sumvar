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
  expect_true(all(abs(row_pct_sums - 100) < 0.15))
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

test_that("tab dp argument controls decimal places in tibble", {
  result_1 <- tab(test_df, group, sex, dp = 1)
  result_2 <- tab(test_df, group, sex, dp = 2)
  # dp=2 should have more decimal precision (smaller rounding step)
  # check that pct values differ between dp=1 and dp=2 for at least one cell
  # (almost certain given random data)
  expect_false(identical(result_1$female_pct, result_2$female_pct))
})

test_that("tab test='exact' returns Fisher test name in tibble", {
  result <- tab(test_df, group, sex, test = "exact")
  expect_true("test" %in% names(result))
  expect_equal(result$test[1], "Fisher's Exact")
})

test_that("tab count columns are integer type", {
  result <- tab(test_df, group, sex)
  expect_type(result$female_n, "integer")
  expect_type(result$male_n,   "integer")
  expect_type(result$total_n,  "integer")
})

test_that("tab printed output has aligned separators", {
  df <- tibble::tibble(
    group = c("a", "a", "b", "b"),
    sex   = c("female", "male", "female", "male")
  )
  out <- capture.output(tab(df, group, sex))

  # Output structure:
  #   out[1]: spanning header (var2 label)
  #   out[2]: column headers row
  #   out[3]: separator row
  #   out[4]: first data row
  #   out[5]: second data row
  #   out[6]: separator row
  #   out[7]: total row
  sep_line  <- out[3]
  data_line <- out[4]

  sep_plus_pos  <- gregexpr("\\+", sep_line)[[1]]
  data_pipe_pos <- gregexpr("\\|", data_line)[[1]]

  expect_equal(as.integer(sep_plus_pos), as.integer(data_pipe_pos))
})
