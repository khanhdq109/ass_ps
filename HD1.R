library("tidyverse")
library("plyr")
library("dplyr")
library("moments")
library("readr")
library("rlist")


# Mot so dinh nghia ham:
vis <- function(dep_delay, carrier) {
  lengthc <- tapply(dep_delay, carrier, length)
  cat("Kich thuoc mau o cac hang hang khong:\n")
  print(lengthc, quote = TRUE, row.names = FALSE)
  
  meanc <- tapply(dep_delay, carrier, mean)
  cat("Trung binh mau o cac hang hang khong:\n")
  print(meanc, quote = TRUE, row.names = FALSE)
  
  sdc <- tapply(dep_delay, carrier, sd)
  vari <- sdc * sdc
  cat("Phuong sai mau o cac hang hang khong:\n")
  print(vari, quote = TRUE, row.names = FALSE)
  
  varc <- vari * lengthc / (lengthc - 1)
  cat("Phuong sai mau hieu chinh o cac hang hang khong:\n")
  print(varc, quote = TRUE, row.names = FALSE)
  
  minc <- tapply(dep_delay, carrier, min)
  cat("Gia tri nho nhat o cac hang hang khong:\n")
  print(minc, quote = TRUE, row.names = FALSE)
  
  maxc <- tapply(dep_delay, carrier, max)
  cat("Gia tri lon nhat o cac hang hang khong:\n")
  print(maxc, quote = TRUE, row.names = FALSE)
  
  Q1 <- tapply(dep_delay, carrier, quantile, probs = 0.25)
  cat("Phan vi 1 o cac hang hang khong:\n")
  print(Q1, quote = TRUE, row.names = FALSE)
  
  Q2 <- tapply(dep_delay, carrier, quantile, probs = 0.5)
  cat("Phan vi 2 o cac hang hang khong:\n")
  print(Q2, quote = TRUE, row.names = FALSE)
  
  Q3 <- tapply(dep_delay, carrier, quantile, probs = 0.75)
  cat("Phan vi 3 o cac hang hang khong:\n")
  print(Q3, quote = TRUE, row.names = FALSE)
  
  boxplot(dep_delay ~ carrier,
          xlab = "Carrier",
          ylab = "Dep_delay",
          main = "Plot")
}

anova <- function(fact, value) {
  fact <- as.factor(fact)
  analysis <- aov(value ~ fact)
  summary(analysis)
}

# 1/ Doc du lieu:
setwd("D:/Khanh/BK/HK212/XSTK/Code")
load(file = "flights.rda")
view(flights)


# 2/ Lam sach du lieu:
cat("Do dai du lieu truoc khi lam sach: ", nrow(flights), '\n')
flights<-na.omit(flights)
cat("Do dai du lieu sau khi lam sach: ", nrow(flights), '\n')


# 3/ Lam ro du lieu:
# Chuyen doi bien:
year<-flights$year
month<-flights$month
day<-flights$month
carrier<-flights$carrier
origin<-flights$origin
dest<-flights$dest
dep_time<-flights$dep_time
arr_time<-flights$arr_time
dep_delay<-flights$dep_delay
arr_delay<-flights$arr_delay
distance<-flights$distance

# Thong ke mo ta:
vis(dep_delay, carrier)

# Loai bo cac outlier:
meanc <- tapply(dep_delay, carrier, mean)
temp <- carrier[!duplicated(carrier)]
for (i in c(1:length(temp))) {
  idx <- which(carrier == temp[i])
  Q <- quantile(dep_delay[idx], probs = c(.25, .75), na.rm = FALSE)
  iqr <- IQR(dep_delay[idx], na.rm = FALSE)
  upper <- Q[[2]] + 1.5 * iqr
  lower <- Q[[1]] - 1.5 * iqr
  for (j in c(1:length(idx))) {
    if (flights$dep_delay[idx[j]] < lower || flights$dep_delay[idx[j]] > upper) {
      flights$dep_delay[idx[j]] <- meanc[[temp[i]]]
    }
  }
}

# Cap nhat bien:
dep_delay<-flights$dep_delay

# Thong ke mo ta lai du lieu:
vis(dep_delay, carrier)


# 4/ ANOVA mot nhan to:
anova(carrier, dep_delay)


# 5/ Mo hinh hoi quy tuyen tinh:

