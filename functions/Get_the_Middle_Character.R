# You are going to be given a word. Your job is to return the middle character 
# of the word. If the word's length is odd, return the middle character. If the 
# word's length is even, return the middle 2 characters.
# 
# Examples:
#    getMiddle("test") should return "es"
# 
#    getMiddle("testing") should return "t"
# 
#    getMiddle("middle") should return "dd"
# 
#    getMiddle("A") should return "A"


# Solution: ----

# Sol 01
get_middle <- function(s){  
  n <- c((nchar(s) + 1) %/% 2, nchar(s) %/% 2 + 1)
  substr(s, n[1], n[2])
}


# Sol 02
get_middle <- function(s){
  if (nchar(s) == 1) {
    return(s)
  } else {
    cmpl <- nchar(s)
    if (cmpl %% 2 == 0) {
      n <- c(cmpl / 2, cmpl / 2 + 1)
      
    } else {
      n <- c(cmpl / 2+1, cmpl / 2+1)
    }
    
    saida <- substr(s, n[1], n[2])
    
    return(saida)
    
  }
}




# Sample Tests: ----
library(testthat)

test_that("Sample Tests", {
  expect_equal(get_middle("test"),"es")
  expect_equal(get_middle("testing"),"t")
  expect_equal(get_middle("middle"), "dd")
  expect_equal(get_middle("A"), "A")
  expect_equal(get_middle("of"), "of")
})
