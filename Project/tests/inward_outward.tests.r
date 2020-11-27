source('Project/selector.r')
library(testthat)

ids = c(1, 2, 3, 4)

test_that('', {
  io = calculate_inward_outward(ids, 'Project/tests/small_sample_1/allcities.txt')
  inward = io$inward_vector
  outward = io$outward_vector

  expect_equal(inward, c(0, 1, 1, 1))
  expect_equal(outward, c(3, 0, 0, 0))
})