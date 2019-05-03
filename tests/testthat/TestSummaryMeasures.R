context("summary measures")


test_that("aux mean returns a positive number", {

  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_gt(aux_mean(5, 0.5), 0)
  expect_type(aux_mean(5, 0.5), 'double')

})

test_that("variance_mean returns a positive number", {

  expect_equal(aux_variance(5, 0.5), 1.25)
  expect_gt(aux_variance(5, 0.5), 0)
  expect_lt(aux_variance(5, 0.3), 5)

})

test_that("aux_mode returns appropriate values", {

  expect_equal(aux_mode(6, 0.7), 4)
  expect_gt(aux_mode(6, 1), 0)
  expect_lt(aux_mode(3, 0.2), 1)

})

test_that("aux_skewness returns appropriate values", {

  expect_equal(aux_skewness(4, 0.5), 0)
  expect_lt(aux_skewness(4, 0.8), 0)
  expect_gt(aux_skewness(4, 0.1), 1)

})

test_that("aux_kurtosis returns appropriate values", {

  expect_equal(aux_kurtosis(5, 0.2), 0.05)
  expect_lt(aux_kurtosis(5, 0.5), 0)
  expect_gt(aux_kurtosis(5, 0.9), 1)

})
