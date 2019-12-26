# Given a string of words, you need to find the highest scoring word.
# 
# Each letter of a word scores points according to its position in the 
# alphabet: a = 1, b = 2, c = 3 etc.
# 
# You need to return the highest scoring word as a string.
# 
# If two words score the same, return the word that appears earliest in the 
# original string.
# 
# All letters will be lowercase and all inputs will be valid.




# Solution: ----

high <- function(x) {
  lookup_num <- c(factor(letters))
  names(lookup_num) <- c(letters)
  score <- sapply(unlist(strsplit(x, " ")), function(x) sum(lookup_num[unlist(strsplit(x, ""))]))
  rk <- rank(score, ties.method = "last")
  names(rk[match(max(rk), rk)])
}



# Sample Tests: ----
library(testthat)

test_that("Sample Tests", {
  expect_equal(high('man i need a taxi up to ubud'), 'taxi')
  expect_equal(high('what time are we climbing up the volcano'), 'volcano')
  expect_equal(high('take me to semynak'), 'semynak')
})