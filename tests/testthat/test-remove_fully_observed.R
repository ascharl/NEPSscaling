context("remove_fully_observed")

test_that("var == NULL", {
  dat <- data.frame(
    variable = LETTERS,
    missing = rep(c("imputed", "observed"), each = 13)
  )
  result <- data.frame(
    variable = LETTERS[1:13],
    missing = "imputed"
  )
  expect_equal(remove_fully_observed(dat, var = NULL), result)
})

test_that("var != NULL", {
  dat <- data.frame(
    variable = LETTERS,
    missing = rep(c("imputed", "observed"), each = 13)
  )
  result <- data.frame(
    variable = LETTERS[1:14],
    missing = c(rep("imputed", 13), "observed")
  )
  expect_equal(remove_fully_observed(dat, var = "N"), result)
})
