source('Project/selector.r')
library(testthat)

ids = c(1, 2, 3, 4)

test_that('', {
  inward = calculate_inward_outward(ids, 'Project/tests/small_sample_1/allcities.txt')
  expect_equal(inward,c(3,0,0,0))
})