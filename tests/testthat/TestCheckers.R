context("Checkers")


test_that("check_prob is a number between 0 and 1", {

  x <- 0.5;
  y <- -1;
  z <- 2;
  l <- 'a';

  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  expect_error(check_prob(y), "p has to be a number between 0 and 1")
  expect_error(check_prob(l), "invalid prob value")

})

test_that("check_trials only allows positive integers", {

  expect_equal(check_trials(1), TRUE)
  expect_error(check_trials('a'), "invalid trials value")
  expect_error(check_trials('-4'), "invalid trials value")

})

test_that("check_success returns appropiate values", {

  expect_equal(check_success(2, 5), TRUE)
  expect_error(check_success(4, 2), "success cannot be greater than trials")
  expect_error(check_success('a', 3), "invalid success value")

})
