# Write a function that takes a positive integer n, sums all the cubed values 
# from 1 to n, and returns that sum.
# 
# Assume that the input n will always be a positive integer.
# 
# Examples: 
# sumCubes(2) // 9
# // sum of the cubes of 1 and 2 is 1 + 8


# Solution: ----

sum_cubes <- function(n){
  
  sum(do.call(function(x)x^3, list(seq_len(n))))
  
}


# Sample Tests: ----
library(testthat)

test_that("Sample Tests", {
  expect_equal(sum_cubes(1), 1)
  expect_equal(sum_cubes(2), 9)
  expect_equal(sum_cubes(3), 36)
  expect_equal(sum_cubes(4), 100)
  expect_equal(sum_cubes(10), 3025)
  expect_equal(sum_cubes(123), 58155876)
})
