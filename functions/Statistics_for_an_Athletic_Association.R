# You are the "computer expert" of a local Athletic Association (C.A.A.). Many 
# teams of runners come to compete. Each time you get a string of all race 
# results of every team who has run. For example here is a string showing the 
# individual results of a team of 5 runners:
#   
#   "01|15|59, 1|47|6, 01|17|20, 1|32|34, 2|3|17"
# 
# Each part of the string is of the form: h|m|s where h, m, s (h for hour, m for
# minutes, s for seconds) are positive or null integer (represented as strings) 
# with one or two digits. There are no traps in this format.
# 
# To compare the results of the teams you are asked for giving three statistics;
# range, average and median.
# 
# Range : difference between the lowest and highest values. In {4, 6, 9, 3, 7} 
# the lowest value is 3, and the highest is 9, so the range is 9 − 3 = 6.
# 
# Mean or Average : To calculate mean, add together all of the numbers in a set 
# and then divide the sum by the total count of numbers.
# 
# Median : In statistics, the median is the number separating the higher half of
# a data sample from the lower half. The median of a finite list of numbers can 
# be found by arranging all the observations from lowest value to highest value 
# and picking the middle one (e.g., the median of {3, 3, 5, 9, 11} is 5) when 
# there is an odd number of observations. If there is an even number of 
# observations, then there is no single middle value; the median is then defined 
# to be the mean of the two middle values (the median of {3, 5, 6, 9} is (5 + 6) / 2 = 5.5).
#  
# Your task is to return a string giving these 3 values. For the example given 
# above, the string result will be
# 
# "Range: 00|47|18 Average: 01|35|15 Median: 01|32|34"
# 
# of the form:
#   
#   "Range: hh|mm|ss Average: hh|mm|ss Median: hh|mm|ss"
# 
# where hh, mm, ss are integers (represented by strings) with each 2 digits.
# 
# Remarks:
# 
# 1: if a result in seconds is ab.xy... it will be given truncated as ab.
# 
# 2: if the given string is "" you will return ""



# Solution: ----

library(lubridate)

stat <- function(strg) {
  if (strg == "") { return("")} else { #test for empty input
    team <- seconds(hms(unlist(strsplit(strg, ",")))) #transforming the data
    # compute the range the average and the median
    team_range <- seconds_to_period(range(as.numeric(team))[2] - range(as.numeric(team))[1])
    team_mean <- seconds_to_period(mean(team))
    team_meadian <- seconds_to_period(median(team))
    # the return
    paste(
      sprintf("Range: %02d|%02d|%02d", as.numeric(hour(team_range)),
              as.numeric(minute(team_range)),
              trunc(as.numeric(second(team_range))), 2),
      sprintf("Average: %02d|%02d|%02d", as.numeric(hour(team_mean)),
              as.numeric(minute(team_mean)),
              trunc(as.numeric(second(team_mean))), 2),
      sprintf("Median: %02d|%02d|%02d", as.numeric(hour(team_meadian)),
              as.numeric(minute(team_meadian)),
              trunc(as.numeric(second(team_meadian))), 2))
  }
}





# Sample Tests: ----


testing <- function(s, expected) {
  actual <- stat(s)
  expect_equal(actual, expected)
}

test_that("tests stat", {
  testing("01|15|59, 1|47|16, 01|17|20, 1|32|34, 2|17|17", "Range: 01|01|18 Average: 01|38|05 Median: 01|32|34")
  testing("02|15|59, 2|47|16, 02|17|20, 2|32|34, 2|17|17, 2|22|00, 2|31|41", "Range: 00|31|17 Average: 02|26|18 Median: 02|22|00")
})



