source('Project/io/hubs_authorities.r')
library(testthat)


test_that("Hardcoded authorities' written vector should be the same", {
  authorities = c(0.25, 0.1, 0.3, 0.07)
  year = 2000
  write_authorities(authorities, year)

  written_data = read_authorities(year)
  expect_equal(written_data, authorities)
})

test_that("Hardcoded hubs' written vector should be the same", {
  hubs = c(0.25, 0.1, 0.3, 0.07)
  year = 2000
  write_hubs(hubs, year)

  written_data = read_hubs(year)
  expect_equal(written_data, hubs)
})



