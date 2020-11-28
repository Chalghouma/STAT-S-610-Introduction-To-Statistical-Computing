source('Project/selector.r')
library(testthat)

library(readr)
library(tidyverse)


test_that('', {
  ids = c(1, 2, 3, 4)
  io = calculate_inward_outward(ids, './Project/tests/small_sample_1/allcities.txt')
  inward = io$inward_vector
  outward = io$outward_vector

  expect_equal(inward, c(0, 1, 1, 1))
  expect_equal(outward, c(3, 0, 0, 0))
})



test_that('a', {
  cases = read_csv('Project/data/judicial.csv')
  my_data <- as_tibble(cases)
  projected = my_data %>% select(1, 9, 10)
  ids = projected[,1]$caseid

  ino = calculate_inward_outward(ids)
  inward =  ino$inward_vector
  outward = ino$outward_vector


  indeg = projected[, 2]$indeg
  outdeg = projected[, 3]$outdeg
  expect_equal(inward, indeg)
  expect_equal(outward, outdeg)
})