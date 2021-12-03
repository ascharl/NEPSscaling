context("extract_correct_number_of_pvs")

test_that("test correct if bgdata = NULL", {

  bgdata <- NULL
  nmi <- 2
  npv <- 2
  pvs <- list( # list of imputations containing data.frames for each estimated pv
    replicate(5, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
    replicate(5, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
  )

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs)
  expect_equal(length(test), npv)
  expect_equal(test[[npv]][["PV"]], 21:30)
})

test_that("test correct if bgdata != NULL", {

  bgdata <- data.frame(ID_t = 1:10, covariate = c(rnorm(9), NA))
  nmi <- 2
  npv <- 2
  pvs <- list( # list of imputations containing data.frames for each estimated pv
    replicate(5, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
    replicate(5, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
  )

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs)
  expect_equal(length(test), npv)
})
