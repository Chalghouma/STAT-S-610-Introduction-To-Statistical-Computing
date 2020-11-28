source('Project/reader.r')
library(testthat)

library(readr)

auth_df = read_auth()

test_that('Should have 203years + 1 column', {
    expect_that(length(auth_df),204)
})

test_that('Should have 30288ids',{
    expect_that(length(auth_df[,1]),30288)
})