source('Project/reader.r')
library(testthat)

library(readr)

auth_df = read_auth()

test_that('Should have 203years + 1 column', {
  expect_equal(length(auth_df), 204)
})

test_that('Should have 30288ids', {
  expect_equal(length(auth_df[, 1]), 30288)
})

test_that('Should throw a warning', {
  expect_warning(function() {
    get_auth_by_year(auth_df, 1799)
  })
  expect_warning(function() {
    get_auth_by_year(auth_df, 2003)
  })
})