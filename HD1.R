library("tidyverse")
library("plyr")
library("dplyr")
library("moments")
library("readr")
library("rlist")

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
length <- tapply(dep_delay, carrier, length)
cat("Kich thuoc mau o cac hang hang khong:\n")
length

mean <- tapply(dep_delay, carrier, mean)
cat("Trung binh mau o cac hang hang khong:\n")
mean

sd <- tapply(dep_delay, carrier, sd)
vari <- sd * sd
cat("Phuong sai mau o cac hang hang khong:\n")
vari

varc <- vari * length / (length - 1)
cat("Phuong sai mau hieu chinh o cac hang hang khong:\n")
varc

minc <- tapply(dep_delay, carrier, min)
cat("Gia tri nho nhat o cac hang hang khong:\n")
minc

maxc <- tapply(dep_delay, carrier, max)
cat("Gia tri lon nhat o cac hang hang khong:\n")
maxc

Q1 <- tapply(dep_delay, carrier, quantile, probs = 0.25)
cat("Phan vi 1 o cac hang hang khong:\n")
Q1

Q2 <- tapply(dep_delay, carrier, quantile, probs = 0.5)
cat("Phan vi 2 o cac hang hang khong:\n")
Q2

Q3 <- tapply(dep_delay, carrier, quantile, probs = 0.75)
cat("Phan vi 3 o cac hang hang khong:\n")
Q3

boxplot(flights$dep_delay ~ flights$carrier,
        xlab = "Carrier",
        ylab = "Dep_delay",
        main = "Plot")


# 4/ ANOVA mot nhan to:


# 5/ Mo hinh hoi quy tuyen tinh:

