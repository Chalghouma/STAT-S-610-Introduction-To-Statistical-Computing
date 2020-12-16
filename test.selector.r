source('Project/source_paths.r')
source(get_selector_file_path())
source(get_reader_file_path())
library(testthat)

library(readr)
library(tidyverse)


judicial_data <- read_judicial_data()

test_that('We should have 9 cases up until 1764', {
  cases <- get_cases_until(judicial_data, 1764)[[1]]
  expect_equal(length(cases), 9)
})

test_that('Given ids should have the correct years', {
  expect_equal(get_case_data_by_id(1,judicial_data)$year, 1754)
  expect_equal(get_case_data_by_id(16125,judicial_data)$year, 1923)
  expect_equal(get_case_data_by_id(26226,judicial_data)$year, 1976)
})
