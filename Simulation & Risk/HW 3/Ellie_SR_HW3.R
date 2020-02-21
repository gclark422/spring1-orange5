###############################
#                             #
#     Simulation and Risk:    #
#         Homework 3          #
#                             #
#        Ellie Caldwell       #
#                             #
###############################

#Goals: 1) Simulate NPV for all wells
#       2) Calculate expected return from scenario and measures of risk
#       3) Recommendation on whether company should invest in scenario

########### LIBRARIES ###########
library(readxl)
library(metRology)
library(graphics)
library(ks)
library(dplyr)
library(car)
library(stats)
library(truncnorm)

########### SEED ###########
set.seed(12345)

########### DATA ###########

#Load in 100,000 2020 predictions from last phase
drilling_costs <- read.csv('C:/Users/Ellie Caldwell/Documents/spring1-orange5/Simulation & Risk/HW 2/Keyur - Code & Outputs/Drilling Cost Simulations.csv')
drilling_costs <- drilling_costs[,1]
#drilling_costs <- sample(drilling_costs, size=100, replace=FALSE)
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


########### Simulate NPV for all wells ###########
num_wells <- sample(x=10:30, size=100000, replace=TRUE)

#Wet well probability
hydro <- rtruncnorm(n=100000, a=0, b=1, mean=0.99, sd=0.05)
reserv <- rtruncnorm(n=100000, a=0, b=1, mean=0.80, sd=0.10)
wet_prob <- hydro * reserv

#See histograms of hydrocarbons, reservoir, and wet well probability
hist(hydro, breaks=50)
hist(reserv, breaks=50)
hist(wet_prob, breaks=50)

#Calculate how many wet and dry wells we have
wet_wells <- rep(0, 100000)
dry_wells <- rep(0, 100000)

#Find out how many wet and dry wells we have using above probabilities
for(i in 1:length(num_wells)) {
  wet_wells[i] <- rbinom(n=1, size=num_wells[i], prob = wet_prob[i])
  dry_wells[i] <- num_wells[i] - wet_wells[i]
}

#Total wet wells
sum(wet_wells)

#Total dry wells
sum(dry_wells)

#Create vector to store overall npv and cost for wet and dry wells, respectively
overall_npv_wet <- rep(0,100000)
overall_cost_dry <- rep(0,100000)

#Lease costs
Pt_lease <- 960 * (rnorm(n=100000, mean=600, sd=50))

#Seismic costs
Pt_seis <- 43000 * (rnorm(n=100000, mean=3, sd=0.35))


#Professional overhead
Pt_prof <- rtri(n=100000, min=172000, max=279500, mode=215000)

#Completetion Costs
Pt_comp <- rnorm(n=100000, mean=390000, sd=50000)

#IP
r <- rnorm(n=30*100000, mean=6, sd=0.28)
Pt_IP <- exp(r)

#Decline rate
Pt_decline <- runif(n=30*100000, min=0.15, max=0.32)

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

#Simulate NPV for wet wells
for(k in 1:100000) {
  
  npv <- rep(0, wet_wells[k])
  
  for(j in 1:wet_wells[k]) {
    
    #Calculate revenue
    initial_costs <- drilling_costs + Pt_lease + Pt_seis + Pt_comp + Pt_prof
    
    proj_2020_2035[,2] <- as.numeric(proj_2020_2035[,2])
    proj_2020_2035[,3] <- as.numeric(proj_2020_2035[,3])
    proj_2020_2035[,4] <- as.numeric(proj_2020_2035[,4])
    
    r_ov <- rep(0,15)
    r_ye1 <- rep(0,15)
    r_ye <- rep(0,15)
    rev <- rep(0,15)
    op_cos <- rep(0,15)
    nri <- rep(0,15)
    royal_pay <- rep(0,15)
    sev_tax <- rep(0,15)
    prof_over <- rep(0,15)
    net_sales <- rep(0,15)
    
    r_ye[1] <- final.dip.r[k,2]
    r_ye1[1] = final.dip.r[k,2]
    r_ye[1] = (1- final.dip.r[k,1])*r_ye[1]
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
      
      r_ye[i] <- final.dip.r[k,2]
      r_ye1[i] = r_ye[i-1]
      r_ye[i] = (1- final.dip.r[k,1])*r_ye[i-1]
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
    
    npv[j] = (-1*initial_costs[k]) + sum(net_sales)
    
  }
  
  overall_npv_wet[k] = sum(npv)

}

#Simulate dry well costs
for (k in 1:100000) {
  
  dry_well_cost <- rep(0, dry_wells[k])

  for(j in 1:dry_wells[k]) {
    
    #Lease cost
    Pt_lease <- 960 * (rnorm(n=1, mean=600, sd=50))
    
    #Seismic costs
    Pt_seis <- 43000 * (rnorm(n=1, mean=3, sd=0.35))
    
    #Professional overhead
    Pt_prof <- rtri(n=1, min=172000, max=279500, mode=215000)
    
    dry_well_cost[j] = drilling_costs + Pt_lease + Pt_seis + Pt_prof
    
  }

  overall_cost_dry[k] = sum(dry_well_cost)

}

#Look at histograms for wet NPV and dry costs
hist(overall_npv_wet, breaks=50)
hist(overall_cost_dry, breaks=50)

#Calculate overall NPV by subtracting dry well costs from wet well NPV
overall_npv = overall_npv_wet - overall_cost_dry

#Look at histogram ofo overall NPV
hist(overall_npv, breaks=50)
summary(overall_npv)

#VaR 1%
VaR = quantile(overall_npv, 0.01)
#$62,353,216

#ES 1%
ES <- mean(overall_npv[overall_npv < VaR], na.rm=TRUE)

#VaR Confidence Interval
n.bootstraps <- 1000
sample.size <- 1000

VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(overall_npv, size=sample.size)
  VaR.boot[i] <- quantile(bootstrap.sample, 0.01, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)
VaR.boot.L
VaR
VaR.boot.U

#Lower: $50,573,262
#Median: $62,353,216
#Upper: $77,035,417

ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
ES.boot.L
ES
ES.boot.U

#Lower: $32,296,466
#Median: $44,812,325
#Upper: $60,372,041

#Year 0 cost to overall_NPV
year_0_to_end <- (median(wet_wells) * 15050000)/(median(dry_wells) * 4610000)
year_0_to_end
