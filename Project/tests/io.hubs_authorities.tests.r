source('Project/source_paths.r')
source(get_hubs_authorities_io_file_path())
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

test_that('',{
  years= c(1850,2000)
  df = read_authorities_df_from_year_interval(years)
  expect_equal(get_authority_from_generated_authorities_df(2000,1,df ) , 0.25)
  expect_equal(get_authority_from_generated_authorities_df(2000,2,df ) , 0.1)
  expect_equal(get_authority_from_generated_authorities_df(2000,3,df ) , 0.3)
  expect_equal(get_authority_from_generated_authorities_df(2000,4,df ) , 0.07)
})


