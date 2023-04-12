library(tibble)
library(dplyr)
data("women")
women2 <- women
women2$height[4] <- 155
women2$weight[4] <- 56
women2
