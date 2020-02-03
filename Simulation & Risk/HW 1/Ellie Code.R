###############################
#                             #
#     Simulation and Risk:    #
#          Homework 1         #
#                             #
#        Ellie Caldwell       #
#                             #
###############################

########### LIBRARIES ###########

#Load in needed libraries
library(readxl)
library(metRology)
library(graphics)
library(ks)
library(dplyr)
library(car)
library(stats)



########### DATA ###########

#Load in data
data = read_excel('C:/Users/Ellie Caldwell/Documents/Simulation and Risk/Homework1_SR/Analysis_Data.xlsx', sheet="Drilling Cost")
#Remove source row
data = data[-1,]
#Set first row to be headers
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
data = header.true(data)
#Remove Date column that is not a year
data = data[,-2]
#Calculate average cost
data$avg_cost = (as.numeric(data$`U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`) + as.numeric(data$`U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)`) + as.numeric(data$`U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`))/3
#Calculate percent change
data <- mutate(data) %>% 
        mutate(percent_cost_change = ((avg_cost)-lag(avg_cost))/lag(avg_cost))

#Create new data set with for only 1991-2006
new_data = subset(data, select=c('Date','Arithmetic Return - Crude Oil', 'Arithmetic Return - Natural Gas', 'Arithmetic Return - Dry Well'))
new_data = new_data[32:47, 2:4]
new_data = as.matrix(new_data)
final_48 = as.vector(new_data)
final_48 = as.data.frame(final_48)
final_48$final_48 = as.numeric(as.character(final_48$final_48))



########### DATA EXPLORATION ###########

#Pull average cost from 2006
avg_cost_2006 = as.numeric(data[47,8])

#Calculate mean and sd of percent changes
mean_data = mean(final_48$final_48)
sd_data = sd(final_48$final_48)

#Look at distribution of data
hist(final_48$final_48, breaks=15)

#Check normality of data with qqplot
qqnorm(final_48$final_48, pch=1)
qqline(final_48$final_48, col = "steelblue", lwd = 2)
#Looks normal

#Check normality with Sharipo Wilk test
shapiro.test(final_48$final_48)
#Fail to reject null and conclude that data is from normal 

#Run kernel density for data
Density.Pt <- density(final_48$final_48, bw="SJ-ste")
Density.Pt

Est.Pt <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1000)
hist(Est.Pt, breaks=50, main='Estimated 1991-2006 Distribution', xlab='Final Value')

########### Normal 2006-2012 ###########

#Run simulation for 2006-2012 using Normal Distribution
set.seed(12345)
P_2006_2012_n <- rep(0,10000)
for(i in 1:10000){
  P0 <- avg_cost_2006
  r <- rnorm(n=1, mean=mean_data, sd=sd_data)
  Pt_n <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rnorm(n=1, mean=mean_data, sd=sd_data)
    Pt_n <- Pt_n*(1+r)
  }
  P_2006_2012_n[i] <- Pt_n
}

mean(P_2006_2012_n)
sd(P_2006_2012_n)

hist(P_2006_2012_n, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Run loop for 2012-2015 using Triangular Distribution 
set.seed(12345)
P_2012_2015_n <- rep(0,10000)
for(i in 1:10000){
  P0 <- P_2006_2012_n[i]
  r <- rtri(n=1, min=-0.22, max=-0.07, mode=-0.0917)
  Pt_n <- P0*(1 + r)
  
  for(j in 1:3){
    r <- rtri(n=1, min=-0.22, max=-0.07, mode=-0.0917)
    Pt_n <- Pt_n*(1+r)
  }
  P_2012_2015_n[i] <- Pt_n
}

mean(P_2012_2015_n)
sd(P_2012_2015_n)

hist(P_2012_2015_n, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Run loop for 2015-2019 using Triangular Distribution
set.seed(12345)
P_2015_2019_n <- rep(0,10000)
for(i in 1:10000){
  P0 <- P_2012_2015_n[i]
  r <- rtri(n=1, min=0.02, max=0.06, mode=0.05)
  Pt_n <- P0*(1 + r)
  
  for(j in 1:4){
    r <- rtri(n=1, min=0.02, max=0.06, mode=0.05)
    Pt_n <- Pt_n*(1+r)
  }
  P_2015_2019_n[i] <- Pt_n
}

mean(P_2015_2019_n)
sd(P_2015_2019_n)

hist(P_2015_2019_n, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Predict for 2020
set.seed(12345)
P_2020_n <- rep(0,10000)
for(i in 1:10000) {
  P0 <- P_2015_2019_n[i]
  r <- rtri(n=1, min=0.02, max=0.06, mode=0.05)
  Pt_n <- P0*(1+r)
  P_2020_n[i] <- Pt_n
}
min(P_2020_n)
max(P_2020_n)
hist(P_2020_n, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

########### Kernel Density 2006-2012 ###########

#Run simulation for 20016-2012 using kernel density
set.seed(12345)
P_2006_2012_kd <- rep(0,10000)
for(i in 1:10000){
  P0 <- avg_cost_2006
  r <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1)
  Pt_kd <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1)
    Pt_kd <- Pt_kd*(1+r)
  }
  P_2006_2012_kd[i] <- Pt_kd
}

mean(P_2006_2012_kd)
sd(P_2006_2012_kd)

hist(P_2006_2012_kd, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Run loop for 2012-2015 using Triangular Distribution 
set.seed(12345)
P_2012_2015_kd <- rep(0,10000)
for(i in 1:10000){
  P0 <- P_2006_2012_kd[i]
  r <- rtri(n=1, min=-0.22, max=-0.07, mode=-0.0917)
  Pt_kd <- P0*(1 + r)
  
  for(j in 1:3){
    r <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1)
    Pt_kd <- Pt_kd*(1+r)
  }
  P_2012_2015_kd[i] <- Pt_kd
}

mean(P_2012_2015_kd)
sd(P_2012_2015_kd)

hist(P_2012_2015_kd, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Run loop for 2015-2019 using Triangular Distribution
set.seed(12345)
P_2015_2019_kd <- rep(0,10000)
for(i in 1:10000){
  P0 <- P_2012_2015_kd[i]
  r <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1)
  Pt_kd <- P0*(1 + r)
  
  for(j in 1:4){
    r <- rtri(n=1, min=0.02, max=0.06, mode=0.05)
    Pt_kd <- Pt_kd*(1+r)
  }
  P_2015_2019_kd[i] <- Pt_kd
}

mean(P_2015_2019_kd)
sd(P_2015_2019_kd)

hist(P_2015_2019_kd, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")

#Predict for 2020
set.seed(12345)
P_2020_kd <- rep(0,10000)
for(i in 1:10000) {
  P0 <- P_2015_2019_kd[i]
  r <- rkde(fhat=kde(final_48$final_48, h=0.07935), n=1)
  Pt_kd <- P0*(1+r)
  P_2020_kd[i] <- Pt_kd
}
min(P_2020_kd)
max(P_2020_kd)
hist(P_2020_kd, breaks=50)
abline(v = avg_cost_2006, col="red", lwd=2)
mtext("2006 Avg Cost", at=avg_cost_2006, col="red")
