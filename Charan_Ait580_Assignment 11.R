library(tidyverse)
d <- read.csv("~/Desktop/EmployeeAttrition(1).csv")
hist(d$Age)
a <- d$Age
b <- d$MonthlyIncome
plot(a, b, main = "Age Vs Monthly Income",
     xlab = "Age", ylab = "Monthly Income",pch = 19)
