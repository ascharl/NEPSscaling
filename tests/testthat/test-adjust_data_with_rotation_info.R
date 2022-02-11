context("adjust_data_with_rotation_info")

test_that("adjust: bgdata == NULL / single df", {
  data <- data.frame(ID_t = 1:10, var = 1:10)
  resp <- data.frame(ID_t = 1:10, item = 1:2)
  ID_t <- data.frame(ID_t = 1:10)
  bgdata <- data.frame(ID_t = 1:10, var2 = 1:10)
  position <- data.frame(ID_t = 1:10, position = 1:2)

  expect_equivalent(
    adjust_data_with_rotation_info(data, resp, ID_t, bgdata, position),
    list(data = data, resp = resp, ID_t = ID_t, bgdata = bgdata,
         position = position[, -1, drop = FALSE])
  )

  expect_equivalent(
    adjust_data_with_rotation_info(data, resp, ID_t, bgdata = NULL, position),
    list(data = data, resp = resp, ID_t = ID_t, bgdata = NULL,
         position = position[, -1, drop = FALSE])
  )

  position <- data.frame(ID_t = 1:6, position = 1:2)
  expect_equivalent(
    adjust_data_with_rotation_info(data, resp, ID_t, bgdata, position),
    list(data = data[1:6, ], resp = resp[1:6, ], ID_t = ID_t[1:6, , drop = FALSE],
         bgdata = bgdata[1:6, ], position = position[, -1, drop = FALSE])
  )
})

test_that("adjust: bgdata is imputed list", {
  data <- data.frame(ID_t = 1:10, var = 1:10)
  resp <- data.frame(ID_t = 1:10, item = 1:2)
  ID_t <- data.frame(ID_t = 1:10)
  bgdata <- list(data.frame(ID_t = 1:10, var2 = 1:10),
                 data.frame(ID_t = 1:10, var2 = 1:10))
  position <- data.frame(ID_t = 1:10, position = 1:2)

  expect_equivalent(
    adjust_data_with_rotation_info(data, resp, ID_t, bgdata, position),
    list(data = data, resp = resp, ID_t = ID_t, bgdata = bgdata,
         position = position[, -1, drop = FALSE])
  )

  position <- data.frame(ID_t = 1:6, position = 1:2)
  expect_equivalent(
    adjust_data_with_rotation_info(data, resp, ID_t, bgdata, position),
    list(data = data[1:6, ], resp = resp[1:6, ], ID_t = ID_t[1:6, , drop = FALSE],
         bgdata = list(bgdata[[1]][1:6, ], bgdata[[2]][1:6, ]),
         position = position[, -1, drop = FALSE])
  )
})
