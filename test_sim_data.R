library(testthat)

source("sim_data.R")

test_that("simulated data",{
  
  dat <- sim_data(2200, 41, .17, .8)

  expect_length(dat[,1], 41)
  
})
