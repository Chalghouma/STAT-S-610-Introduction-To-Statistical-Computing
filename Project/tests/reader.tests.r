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
  expect_warning({
    get_auth_by_year(auth_df, 1799)
  })
  expect_warning({
    get_auth_by_year(auth_df, 2003)
  })
})


test_that('Should match authorities for year 2002', {
  auth_value_case_id_5 = 2.913e-05
  auth_value_case_id_30288 = 0
  authority_vector = get_auth_by_year(auth_df, 2002)
  expect_equal(auth_value_case_id_5, authority_vector[5])
  expect_equal(auth_value_case_id_30288, authority_vector[30288])
})

test_that('Should match authorities for year 2001', {
  auth_value_case_id_30288 = NA
  authority_vector = get_auth_by_year(auth_df, 2001)
  expect_equal(auth_value_case_id_30288, authority_vector[30288])
})