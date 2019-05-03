context("binomial")

test_that("bin_choose behaves appropriately", {

  expect_error(bin_choose(3, 5), "k cannot be greater than n")
  expect_equal(bin_choose(5, 2), 10)
  expect_gt(bin_choose(5, 5), 0)

})

test_that("bin_probability behaves appropriately", {

  expect_error(bin_probability(4, 2, 0.2), "success cannot be greater than trials")
  expect_lt(bin_probability(2, 8, 0.7), 1)
  expect_length(bin_probability(success = 0:2, trials = 5, prob = 0.5), 3)

})

test_that("bin_distribution returns appropriate outputs", {

  expect_s3_class(bin_distribution(2, 1), "bindis")
  expect_error(bin_distribution(2, -1), "p has to be a number between 0 and 1")
  expect_length(bin_distribution(5, 0.5), 2)

})

test_that("bin_cumulative behaves appropriately", {

  expect_error(bin_cumulative(5, 2), "p has to be a number between 0 and 1")
  expect_length(bin_cumulative(5, 0.2), 3)
  expect_s3_class(bin_cumulative(5, 0.5), "bincum")

})


