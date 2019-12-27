
# Implement a function that receives two IPv4 addresses, and returns the number 
# of addresses between them (including the first one, excluding the last one).
# 
# All inputs will be valid IPv4 addresses in the form of strings. The last 
# address will always be greater than the first one.
# 
# Examples:
#    ips_between("10.0.0.0", "10.0.0.50")  ==   50
#    ips_between("10.0.0.0", "10.0.1.0")   ==  256 
#    ips_between("20.0.0.10", "20.0.1.0")  ==  246




# Solution: ----

ips_between <- function(a, b) {
  tbl <- data.frame(
    a = rep(0, 4),
    b = rep(0, 4),
    c = rep(0, 4),
    d = rep(0, 4),
    e = rep(0, 4)
  )
  
  tbl$a <- as.numeric(unlist(strsplit(a, ".", fixed = TRUE)))
  tbl$b <- as.numeric(unlist(strsplit(b, ".", fixed = TRUE)))
  
  tbl$c <- c(tbl$a[1] * 2 ^ 24, tbl$a[2] * 2 ^ 16, tbl$a[3] * 2 ^ 8, tbl$a[4])
  tbl$d <- c(tbl$b[1] * 2 ^ 24, tbl$b[2] * 2 ^ 16, tbl$b[3] * 2 ^ 8, tbl$b[4])
  
  tbl$e <- tbl$d - tbl$c
  
  sum(tbl$e)
  
}


# Sample Tests: ----
library(testthat)

test_that("Sample tests", {
  expect_equal(ips_between("10.0.0.0", "10.0.0.50"), 50)
  expect_equal(ips_between("20.0.0.10", "20.0.1.0"), 246)
})

