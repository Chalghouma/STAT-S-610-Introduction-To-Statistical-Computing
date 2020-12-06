source('Project/selector.r')
library(testthat)

library(readr)
library(tidyverse)


cases = read_csv('Project/data/judicial.csv')
my_data <- as_tibble(cases)

test_that('', {
  cu = cases_until(my_data, 1764)[[1]]
  expect_equal(length(cu), 9)
})

test_that('Given ids should have the correct years', {
  expect_equal(case_data_by_id(my_data, 1)$year, 1754)
  expect_equal(case_data_by_id(my_data, 16125)$year, 1923) 
  expect_equal(case_data_by_id(my_data, 26226)$year, 1976) 
})
