# STAT-S 610
# LAB 3
# 2020-10-02
# https://jfukuyama.github.io/teaching/stat610/assignments/lab3.pdf
# TO RUN: testthat::test_dir('.')

# --- setup --- #

# load package functions and functions we want to test
library(testthat)
import::here(compute_f_hat, llr, make_predictor_matrix, make_weight_matrix, W,
             .from = 'llr_functions.R')

context("Check local linear regression function")

# generate data for a very simple regression model
n <- 15
x <- rnorm(n)
y <- rnorm(x + rnorm(n))
z <- seq(-1, 1, length.out = 100)
omega <- 1

# --- tests --- #

test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})

test_that("make_weight_matrix works on simple cases", {
  # make an example weight matrix
  Wz <- make_weight_matrix(z[1], x, omega)
  
  # check that the output is a diagonal matrix, 
  expect_true(all(c(Wz[upper.tri(Wz)], Wz[lower.tri(Wz)]) == 0))
  
  # that all the elements are nonnegative, 
  expect_true(all(Wz >= 0))
  
  # that the weights are correct in simple cases 
  # where you know what the output should be
  r <- abs(x - z[1]) / omega
  expect_equal(diag(Wz), sapply(r, W))
})

test_that("make_predictor_matrix works on simple cases", {
  # make an example predictor matrix
  X <- make_predictor_matrix(x)
  
  # write tests to check that the dimensions are correct, 
  expect_equal(dim(X), c(n, 2))
  
  # the first column is all 1's, 
  expect_equal(X[, 1], rep(1, n))
  
  # second column is x
  expect_equal(X[, 2], x)
})
