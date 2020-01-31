###############################
#                             #
#     Simulation and Risk:    #
#         Homework 1          #
#                             #
#                             #
#        Kyle Clapper         #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("graphics")
install.packages("ks")

library(graphics)
library(ks)
library(EnvStats)
library(dplyr)
library(nortest)

# Importing Data
setwd("C:/Users/k_cla/OneDrive/Desktop/MSA Program/spring1-orange5/Simulation & Risk/Data/")

data = read.csv(file = 'Kyle 48 Obs.csv')
data = rename(data, 'changes' = 'ï..changes')


############################################ First Simulation Code Under Normal Assumption ##################################################

mean = mean(data$changes)
stdev = sd(data$changes)

PN <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.8
  r <- rnorm(n=1, mean=mean, sd=stdev)
  
  Pt <- P0*(1 + r)
  
  for(j in 2:14){ # j should start at 2 because we do the first year before entering the loop
    if (j <= 6) {
    r <- rnorm(n=1, mean=mean, sd=stdev)
    Pt <- Pt*(1+r) }
    else if (j <= 9) {
    r <- rtri(n=1, min=.07, max=.22, mode=.0917) * -1
    Pt <- Pt*(1+r) }
    else if (j<=14) {
    r <- rtri(n=1, min=.02, max=.06, mode=.05)
    Pt <- Pt*(1+r) }
  }
  PN[i] <- Pt
}

mean(PN)
sd(PN)

hist(PN, breaks=50, main='2020 Simulated Cost Distribution  - Normal', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Cost", at=1000, col="red")

########################################### Second Simulation Code with Density Estimation ###################################################

Density.PD <- density(data$changes, bw="SJ-ste")
Density.PD # this code give us the bandwidth

PD <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.8
  r <- rkde(fhat=kde(data$changes), n=1)
  
  Pt <- P0*(1 + r)
  
  for(j in 2:14){ # j starts at 2 because we ran the first iteration outside of the loop
    if (j <= 6) {
      r <- rkde(fhat=kde(data$changes, h=.07935), n=1) # using the default for bandwidth
      Pt <- Pt*(1+r) }
    else if (j <= 9) {
      r <- rtri(n=1, min=.07, max=.22, mode=.0917) * -1
      Pt <- Pt*(1+r) }
    else if (j<=14) {
      r <- rtri(n=1, min=.02, max=.06, mode=.05)
      Pt <- Pt*(1+r) }
  }
  PD[i] <- Pt
}

mean(PD)
sd(PD)

hist(PD, breaks=50, main='2020 Simulated Cost Distribution - Kernel', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Cost", at=1000, col="red")


################################################## Checking Normality of the changes ###########################################################

# QQ Plot
qqnorm( data$changes, main='changes')
qqline( data$changes )

# Formal test
ad.test(data$changes) # Anderson Darling test indicates the data is not significantly different from a normal distribution


# We should move forward with the first simulation that assumes normality. The sample size is realively small for the kernel density estimator to provide
# a reliable result
