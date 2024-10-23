library(testthat)

test_that("OR_95CI computes correctly", {
  coef <- c(0.5, -0.2)
  se <- c(0.1, 0.05)
  result <- OR_95CI(coef, se, 0.05, 2)
  expect_true(grepl("\\d+\\.\\d+ \\(\\d+\\.\\d+, \\d+\\.\\d+\\)", result))
})

devtools::test()

devtools::document()

