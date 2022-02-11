context("reshape_data_pvs")

test_that("imputation = 'all'", {
  pv <- data.frame(
    ID_t = 1:13,
    matrix(1, nrow = 13, ncol = 13)
  )
  names(pv) <- c("ID_t", LETTERS[1:13])
  pv_obj <- list(
    pv = replicate(5, pv, simplify = FALSE)
  )
  result <- data.frame(
    ID_t = rep(rep(1:13, each = 13), 5),
    variable = rep(rep(LETTERS[1:13], 13), 5),
    value = 1,
    imputation = as.factor(rep(1:5, each = 13*13))
  )
  expect_equivalent(reshape_data_pvs(pv_obj, imputation = "all"),
                    result)
})

test_that("imputation = 1:3", {
  pv <- data.frame(
    ID_t = 1:13,
    matrix(1, nrow = 13, ncol = 13)
  )
  names(pv) <- c("ID_t", LETTERS[1:13])
  pv_obj <- list(
    pv = replicate(5, pv, simplify = FALSE)
  )
  result <- data.frame(
    ID_t = rep(rep(1:13, each = 13), 3),
    variable = rep(rep(LETTERS[1:13], 13), 3),
    value = 1,
    imputation = as.factor(rep(1:3, each = 13*13))
  )
  expect_equivalent(reshape_data_pvs(pv_obj, imputation = 1:3),
                    result)
})
