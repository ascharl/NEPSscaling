context("extract_correct_number_of_pvs")

test_that("test correct if bgdata = NULL", {

  bgdata <- NULL
  nmi <- 2
  npv <- 2
  pvs <- list( # list of imputations containing data.frames for each estimated pv
    replicate(npv, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
    replicate(npv, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
  )

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll = FALSE)
  expect_equal(length(test$datalist), npv)
  expect_equal(test$datalist[[npv]][["PV"]], 21:30)
  expect_equal(test$npv, npv)
})

test_that("test correct if bgdata != NULL", {

  bgdata <- data.frame(ID_t = 1:10, covariate = c(rnorm(9), NA))
  nmi <- 2
  npv <- 2
  pvs <- list( # list of imputations containing data.frames for each estimated pv
    replicate(npv, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
    replicate(npv, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
  )

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll = FALSE)
  expect_equal(length(test$datalist), npv)
  expect_equal(test$npv, npv)
})

test_that("test returnAll", {

  bgdata <- data.frame(ID_t = 1:10, covariate = c(rnorm(9), NA))
  nmi <- 2
  npv <- 2
  pvs <- list( # list of imputations containing data.frames for each estimated pv
    replicate(npv, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
    replicate(npv, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
  )

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll = FALSE)
  expect_equal(length(test), npv)

  test <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll = TRUE)
  expect_equal(length(test$datalist), npv*nmi)
  expect_equal(test$npv, npv * nmi)
})
