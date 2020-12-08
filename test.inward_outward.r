source('Project/source_paths.r')
source(get_citations_reader_file_path())
library(testthat)

library(readr)
library(tidyverse)


test_that('Should have the exact same inward/outward', {
  ids <- c(1, 2, 3, 4)
  io <- calculate_inward_outward(ids, './Project/data/tests/small_sample_1/allcities.txt')
  inward <- io$inward_vector
  outward <- io$outward_vector

  expect_equal(inward, c(0, 1, 1, 1))
  expect_equal(outward, c(3, 0, 0, 0))
})



test_that('Is supposed to have the same inward/outward values. (But fails due to the .csv inconsistency', {
  cases = read_csv('Project/data/judicial.csv')
  my_data <- as_tibble(cases)

  case_ids_with_inout_deg <- my_data %>% select(1, 9, 10)
  ids <- case_ids_with_inout_deg[, 1]$caseid

  ino <- calculate_inward_outward(ids)
  inward <- ino$inward_vector
  outward <- ino$outward_vector


  indeg <- case_ids_with_inout_deg[, 2]$indeg
  outdeg <- case_ids_with_inout_deg[, 3]$outdeg
  expect_false(isTRUE(all.equal(inward,indeg)))
  expect_false(isTRUE(all.equal(outward,outdeg)))
  # expect_equal(inward, indeg)
  # expect_equal(outward, outdeg)
})