context("calculate_standardized_regr_coeff")

test_that("calculate_standardized_regr_coeff: longitudinal", {
  regr.coeff <- list(imp1 = data.frame(Variable = c("Intercept", "var1", "var2"),
                                       coeff_w1 = c(1, 2, 2),
                                       se_w1 = c(1, 1, 1),
                                       coeff_w2 = c(1, 2, 2),
                                       se_w2 = c(1, 1, 1)))
  rownames(regr.coeff[[1]]) <- c("Intercept", "var1", "var2")
  datalist <- list(imp1 = data.frame(ID_t = 1:10, var1 = 1:10, var2 = 1:10,
                                     PV = 1))
  longitudinal <- TRUE
  waves <- c("_w1", "_w2")
  variance <- list(c(4, 4))

  result <- list(
    imp1 = data.frame(Variable = c("Intercept", "var1", "var2"),
                      coeff_w1 = c(1, 2, 2),
                      coeff_std_w1 = c(NA, 2, 2) * sd(1:10) / 2,
                      se_w1 = c(1, 1, 1),
                      coeff_w2 = c(1, 2, 2),
                      coeff_std_w2 = c(NA, 2, 2) * sd(1:10) / 2,
                      se_w2 = c(1, 1, 1))
  )

  expect_equivalent(calculate_standardized_regr_coeff(regr.coeff,
                                                      datalist,
                                                      longitudinal,
                                                      waves, variance),
                    result)
})

test_that("calculate_standardized_regr_coeff: cross-sectional", {
  regr.coeff <- data.frame(Variable = c("Intercept", "var1", "var2"),
                           imp1_coeff = c(1, 2, 2),
                           imp1_se = c(1, 1, 1))
  rownames(regr.coeff) <- c("Intercept", "var1", "var2")
  datalist <- list(imp1 = data.frame(ID_t = 1:10, var1 = 1:10, var2 = 1:10,
                                     PV = 1))
  longitudinal <- FALSE
  waves <- NULL
  variance <- 4

  result <- data.frame(Variable = c("Intercept", "var1", "var2"),
                       imp1_coeff = c(1, 2, 2),
                       imp1_coeff_std = c(NA, 2, 2) * sd(1:10) / 2,
                       imp1_se = c(1, 1, 1))

  expect_equivalent(calculate_standardized_regr_coeff(regr.coeff,
                                                      datalist,
                                                      longitudinal,
                                                      waves, variance),
                    result)
})
