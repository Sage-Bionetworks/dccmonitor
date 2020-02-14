context("validation-results.R")

data <- list(
  check1 = dccvalidator::check_pass("msg", "behavior"),
  check2 = dccvalidator::check_pass("msg", "behavior"),
  check3 = dccvalidator::check_warn("msg", "behavior"),
  check4 = dccvalidator::check_fail("msg", "behavior"),
  check5 = NULL
)

test_that("percent_pass_validation returns correct value", {
  res1 <- percent_pass_validation(data)
  res2 <- percent_pass_validation(data[1])
  res3 <- percent_pass_validation(data[3])
  res4 <- percent_pass_validation(data[4])
  res5 <- percent_pass_validation(data[c(1, 3)])
  res6 <- percent_pass_validation(data[c(1, 4)])
  res7 <- percent_pass_validation(data[c(3, 4)])
  res8 <- percent_pass_validation(data[5])
  expect_equal(res1, 50)
  expect_equal(res2, 100)
  expect_equal(res3, 0)
  expect_equal(res4, 0)
  expect_equal(res5, 50)
  expect_equal(res6, 50)
  expect_equal(res7, 0)
  expect_equal(res8, 0)
})

test_that("percent_pass_validation returns 0 if passed NULL", {
  res <- percent_pass_validation(NULL)
  expect_equal(res, 0)
})

test_that("percent_pass_validation returns 0 if all checks are NULL", {
  data <- list(
    check1 = NULL,
    check2 = NULL
  )
  res <- percent_pass_validation(NULL)
  expect_equal(res, 0)
})
