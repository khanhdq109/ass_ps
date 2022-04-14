library("tidyverse")
library("plyr")
library("dplyr")
library("moments")
library("readr")
library("rlist")

anova <- function(fact, value) {
  fact <- as.factor(fact)
  analysis <- aov(value ~ fact)
  summary(analysis)
}

fact <- c("A", "A", "A", "A", "A", 
          "B", "B", "B", "B", "B",
          "C", "C", "C", "C", "C")
value <- c(0.56, 1.12, 0.90, 1.07, 0.94,
          0.72, 0.69, 0.87, 0.78, 0.91,
          0.62, 1.08, 1.07, 0.99, 0.93)
anova(fact, value)

giaovien <- c("A", "A", "A", "A", "A", "A",
              "B", "B", "B", "B", "B", "B",
              "C", "C", "C", "C", "C", "C")
diem <- c(82, 86, 79, 83, 85, 84,
          74, 82, 78, 75, 76, 77,
          79, 79, 77, 78, 82, 79)
anova(giaovien, diem)
