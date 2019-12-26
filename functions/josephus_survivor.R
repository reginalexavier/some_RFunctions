# Original Problem: ----
# 
# 
# In this kata you have to correctly return who is the "survivor", ie: the last
# element of a Josephus permutation.
# 
# Basically you have to assume that n people are put into a circle and that they
# are eliminated in steps of k elements, like this:
#
# josephus_survivor(7,3) => means 7 people in a circle;
# one every 3 is eliminated until one remains
# [1,2,3,4,5,6,7] - initial sequence
# [1,2,4,5,6,7] => 3 is counted out
# [1,2,4,5,7] => 6 is counted out
# [1,4,5,7] => 2 is counted out
# [1,4,5] => 7 is counted out
# [1,4] => 5 is counted out
# [4] => 1 counted out, 4 is the last element - the survivor!
#
# Note: you may assume that both n and k will always be >=1.


# Solution: ----

josephus_survivor <- function(N, k) {
  f_n <- 1
  for (i in seq_len(N)) {
    f_n <- ((k - 1 + f_n) %% i) + 1
  }
  f_n
}



# Sample Tests: ----
library(testthat)

test_that("basic tests", {
  expect_equal(josephus_survivor(7, 3), 4)
  expect_equal(josephus_survivor(11, 19), 10)
  expect_equal(josephus_survivor(40, 3), 28)
  expect_equal(josephus_survivor(14, 2), 13)
  expect_equal(josephus_survivor(100, 1), 100)
  expect_equal(josephus_survivor(1, 300), 1)
  expect_equal(josephus_survivor(2, 300), 1)
  expect_equal(josephus_survivor(5, 300), 1)
  expect_equal(josephus_survivor(7, 300), 7)
  expect_equal(josephus_survivor(300, 300), 265)
})

