context("reshape_data")

test_that("imputation == 'all'", {
  indmis <- data.frame(
    ID_t = 1:13,
    matrix(rep(0:1, 169), nrow = 13, ncol = 13)
  )
  names(indmis) <- c("ID_t", LETTERS[1:13])
  pv <- data.frame(
    ID_t = 1:13,
    matrix(1, nrow = 13, ncol = 13)
  )
  names(pv) <- c("ID_t", LETTERS[1:13])
  pv_obj <- list(
    indmis = indmis,
    pv = replicate(5, pv, simplify = FALSE)
  )
  result <- data.frame(
    ID_t = rep(1:13, each = 5*13),
    variable = rep(rep(LETTERS[1:13], each = 5), 13),
    missing = "observed",
    value = 1,
    imputation = as.factor(rep(1:5, 13*13))
  )
  sel <- 6:10
  for (i in 1:83) {
    sel <- c(sel, sel[(length(sel) - 4):length(sel)] + 10)
  }
  result$missing[sel] <- "imputed"
  result$missing <- as.factor(result$missing)
  expect_equivalent(reshape_data(pv_obj, imputation = "all"),
                    result)
})

test_that("imputation == 1:3", {
  indmis <- data.frame(
    ID_t = 1:13,
    matrix(rep(0:1, 169), nrow = 13, ncol = 13)
  )
  names(indmis) <- c("ID_t", LETTERS[1:13])
  pv <- data.frame(
    ID_t = 1:13,
    matrix(1, nrow = 13, ncol = 13)
  )
  names(pv) <- c("ID_t", LETTERS[1:13])
  pv_obj <- list(
    indmis = indmis,
    pv = replicate(5, pv, simplify = FALSE)
  )
  result <- data.frame(
    ID_t = rep(1:13, each = 3*13),
    variable = rep(rep(LETTERS[1:13], each = 3), 13),
    missing = "observed",
    value = 1,
    imputation = as.factor(rep(1:3, 13*13))
  )
  sel <- 4:6
  for (i in 1:83) {
    sel <- c(sel, sel[(length(sel) - 2):length(sel)] + 6)
  }
  result$missing[sel] <- "imputed"
  result$missing <- as.factor(result$missing)
  expect_equivalent(reshape_data(pv_obj, imputation = 1:3),
                    result)
})
