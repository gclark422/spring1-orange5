###############################
#                             #
#     Simulation and Risk:    #
#           Homework 2        #
#                             #
#        Ellie Caldwell       #
#                             #
###############################

#Objective: Simulate 1) cost of a single dry well 2)NPV of single wet well

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

#Load in 100,000 2020 predictions from last phase
drilling_costs <- read.csv('C:/Users/Ellie Caldwell/Documents/Simulation and Risk/2020 Predictions.csv')
drilling_costs <- drilling_costs[,2]
drilling_costs <- as.vector(drilling_costs)
drilling_costs <- 1000*drilling_costs

#Load in Price Projection data
proj <- read_excel("C:/Users/Ellie Caldwell/Documents/Simulation and Risk/Homework2_SR/Analysis_Data.xlsx", sheet = "Price Projections")

#Function to set first row to be headers
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

proj <- proj[-1,]
proj <- header.true(proj)
proj_2020_2035 <- proj[1:16,]
proj_2020_2035 <- as.data.frame(proj_2020_2035)
proj_2020_2035 <- as.numeric(proj_2020_2035)

########### DRY WELL ###########

#Drilling costs + Lease costs + Seismic Costs + Professional Overhead

#Lease costs
set.seed(12345)
r <- rnorm(n=100000, mean=600, sd=50)
Pt_lease <- 960*r

#Seismic Costs
set.seed(12345)
r <- rnorm(n=100000, mean=3, sd=0.35)
Pt_seis <- 43000*r

#Professional Overhead
set.seed(12345)
r <- rtri(n=100000, min=172000, max=279500, mode=215000)
Pt_prof <- r

dry_well = drilling_costs + Pt_lease + Pt_seis + Pt_prof

mean(dry_well)
sd(dry_well)
min(dry_well)
max(dry_well)
median(dry_well)
quantile(dry_well, c(0.05, 0.25, 0.75, 0.95))

hist(dry_well, breaks=50)


########### WET WELL ###########

#Drilling costs

#Lease costs
set.seed(123456)
r <- rnorm(n=100000, mean=600, sd=50)
Pt_lease <- 960*r

#Seismic Costs
set.seed(123456)
r <- rnorm(n=100000, mean=3, sd=0.35)
Pt_seis <- 43000*r


#Professional Overhead
set.seed(123456)
r <- rtri(n=100000, min=172000, max=279500, mode=215000)
Pt_prof <- r


#Completetion Costs
set.seed(123456)
r <- rnorm(n=100000, mean=390000, sd=50000)
Pt_comp <- r

#IP
set.seed(12345)
r <- rnorm(n=100000, mean=6, sd=0.28)
Pt_IP <- exp(r)

#Decline rate
set.seed(12345)
r <- runif(n=100000, min=0.15, max=0.32)
Pt_decline <- r

#Need Cholesky Decomposition
R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

Both.r <- cbind(standardize(Pt_decline), standardize(Pt_IP))
dip.r <- U %*% t(Both.r)
dip.r <- t(dip.r)

final.dip.r <- cbind(destandardize(dip.r[,1], Pt_decline), destandardize(dip.r[,2], Pt_IP))

nri <- rep(0, 100000)

#Calculate revenue
initial_costs <- drilling_costs + Pt_lease + Pt_seis + Pt_comp + Pt_prof

proj_2020_2035[,2] <- as.numeric(proj_2020_2035[,2])
proj_2020_2035[,3] <- as.numeric(proj_2020_2035[,3])
proj_2020_2035[,4] <- as.numeric(proj_2020_2035[,4])

npv <- rep(0,100000)

set.seed(12345)

for(j in 1:100000) {
  r_ov <- rep(0,15)
  r_ye1 <- rep(0,15)
  r_ye <- rep(0,15)
  rev <- rep(0,15)
  op_cos <- rep(0,15)
  nri <- rep(0,15)
  royal_pay <- rep(0,15)
  sev_tax <- rep(0,15)
  prof_over <- rep(0,15)
  net_sales <- rep(0,100000)
  
  r_ye[1] <- final.dip.r[j,2]
  r_ye1[1] = final.dip.r[j,2]
  r_ye[1] = (1- final.dip.r[j,1])*r_ye[1]
  r_ov[1] = 365 * (0.5 * (r_ye1[1] + r_ye[1]))
  
  price <- rtri(n=1, min=proj_2020_2035[1, 3], max=proj_2020_2035[1, 2], mode = proj_2020_2035[1, 4])
  
  rev[1] <- r_ov[1] * price
  
  op_cos[1] <- (rnorm(n=1, mean=2.25, sd=0.3)) * r_ov[1]
  
  nri[1] <- rnorm(n=1, mean=0.75, sd=0.02)
  
  prof_over[1] <- rtri(n=1, min=172000, max=279500, mode=215000)
  
  royal_pay[1] <- rev[1] * nri[1]
  
  sev_tax[1] <- royal_pay[1] * 0.046
  
  net_sales[1] <- ((rev[1] - prof_over[1] - op_cos[1] - sev_tax[1])/((1+0.1)^1))
  
  for(i in 2:15){
    r_ye[i] <- final.dip.r[j,2]
    r_ye1[i] = r_ye[i-1]
    r_ye[i] = (1- final.dip.r[j,1])*r_ye[i-1]
    r_ov[i] = 365 * (0.5 * (r_ye1[i] + r_ye[i]))
    
    price <- rtri(n=1, min=proj_2020_2035[i, 3], max=proj_2020_2035[i, 2], mode = proj_2020_2035[i, 4])
    
    rev[i] = r_ov[i] * price
    
    op_cos[i] <- (rnorm(n=1, mean=2.25, sd=0.3)) * r_ov[i]
    
    nri[i] <- nri[1]
    
    prof_over[i] <- prof_over[1]
    
    royal_pay[i] <- rev[i] * nri[i]
    
    sev_tax[i] <- royal_pay[i] * 0.046
    
    net_sales[i] <- ((rev[i] - prof_over[i] - op_cos[i] - sev_tax[i])/((1+0.1)^i))
  }
  
  npv[j] = (-1*initial_costs) + sum(net_sales)
  
}

mean(npv)
sd(npv)
median(npv)
quantile(npv, c(0, 0.05, 0.25, 0.75, 0.95, 1))

hist(npv, breaks=50, xlim=c(1000000, 105000000))
