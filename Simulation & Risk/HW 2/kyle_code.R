###############################
#                             #
#     Simulation and Risk:    #
#         Homework 2          #
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
library(stats)
library(readxl)

options(scipen = 999)

setwd("C:/Users/k_cla/OneDrive/Desktop/MSA Program/spring1-orange5/Simulation & Risk/Data/")

# Setting a seed
set.seed(12345)

####################################### Cost of Single a Dry Well ########################################
dry_well <- rep(0,100000)
for(i in 1:100000){
  cost <- 0 # initializing cost to zero
  
  # simulating number of acres
  a <- rnorm(n=1, mean=600, sd=50)
  cost = cost + a*960
  
  # simulating seismic sections
  s <- rnorm(n=1, mean=3, sd=.35)
  cost = cost + s*43000
  
  # simulating professional overhead
  o <- rtri(n=1, min=172000, max=279500, mode=215000)
  cost = cost + o
  
  dry_well[i] <- cost
}

# Adding in the drilling costs
drilling = read.csv("kyle_drilling_costs.csv")
for(i in 1:100000) {
  dry_well[i] = dry_well[i] + drilling$value[i]
}

##################################### Net Present Value of Wet Well ###########################################
# This code runs a separate simulation for each individual component of the NPV for a well.
# Then the outputs of each simulation are combined in a final NPV calculation to return a
# final distribution


################ Year zero Expenses ##################

zero_cost <- rep(0,100000)
prof_over <- rep(0,100000)

for(i in 1:100000){
  
  cost <- 0 # initializing cost to zero
  
  # simulating number of acres
  a <- rnorm(n=1, mean=600, sd=50)
  cost = cost + a*960
  
  # simulating seismic sections
  s <- rnorm(n=1, mean=3, sd=.35)
  cost = cost + s*43000
  
  # simulating completion costs
  c <- rnorm(n=1, mean=390000, sd=50000)
  cost = cost + c
  
  # simulating professional overhead ************************** This needs to be included in every subsequent year!!!!!!!!!!
  o <- rtri(n=1, min=172000, max=279500, mode=215000)
  cost = cost + o
  prof_over[i] <- o
  
  zero_cost[i] <- cost
}

# Adding in the drilling costs
for(i in 1:100000) {
  zero_cost[i] = zero_cost[i] + drilling$value[i]
}

################ Production Risk ####################
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}
prod <- rep(0,100000)
R <- matrix(data=cbind(1,.64, .64, 1), nrow=2) 
U <- t(chol(R))
  
ip <- rlnorm(100000, meanlog=6, sdlog=.128)
d <- runif(100000,.15,.32)
both <- cbind(standardize(ip), standardize(d))
ipd <- U %*% t(both)
ipd <- t(ipd)

production <- cbind(destandardize(ipd[,1], ip), destandardize(ipd[,2], d))

############### Revenue Risk #########################

# Price Risk
data = read_xlsx("Analysis_Data.xlsx",
                 sheet = "Price Projections",
                 skip = 2,
                 na = ".")

data <- as.data.frame(data)

prices <- rep(0,100000)
for(i in 1:100000){
  year <- list()
  for(j in 1:15){
    year[j] <- rtri(n=1, min=data[j,3], max=data[j,2], mode=data[j,4])
  }
prices[i] <- list(year) 
}

# Net Revenue Interest
nr_risk <- rep(0,100000)
for(i in 1:100000){
  nr_risk[i] <- rnorm(n=1, mean=.75, sd=.02)
}

############### Operating Expenses #########################

# Operating Costs
op_cost <- rep(0,100000)
for(i in 1:100000){
  year <- list()
  for(j in 1:15){
    year[j] <- rnorm(n=1, mean=2.25, sd=.30)
    #year <- list(year)
  }
  op_cost[i] <- list(year) 
}

# Severance Taxes
sev_tax <- .046

####################### NPV Calculation ########################
final_npv <- rep(0,100000)

for(i in 1:100000){
  wacc <- .10
  initial_cost <- zero_cost[i]*-1
  initial_production <- production[i,1]
  annual_decrease <- production[i,2]
  yearly_production <- rep(0,16)
  yearly_production[1] <- initial_production
  oil_volume_per_year <- rep(0,15)
  yearly_price <- rep(0,15)
  yearly_net_revenue <- rep(0,15)
  yearly_profit <- rep(0,15)
  npv_list <- rep(0,15)
  
  for(j in 1:15){
  # calculating oil production for the 15 years  
  yearly_production[j+1] <- (1-annual_decrease)*yearly_production[j]
  oil_volume_per_year[j] <- 365*((yearly_production[j]+yearly_production[j+1])/2)
  
  # calculating the net revenue for the 15 years
  yearly_net_revenue[j] <- (oil_volume_per_year[j] * prices[[i]][[j]]) * nr_risk[i] 
  
  # calculating the yearly profit 
  yearly_profit[j] <- (yearly_net_revenue[j] - (yearly_net_revenue[j]*sev_tax) - (oil_volume_per_year[j]*op_cost[[i]][[j]]) - (prof_over[i])) 
  }
  
  # NPV
  for(k in 1:15){
  npv_list[k] <- (yearly_profit[k]) / ((1+wacc)^k) 
  }
  
  final_npv[i] <- initial_cost + sum(npv_list)
}

# Plotting the histogram of NPV
final_npv <- as.data.frame(final_npv)
final_npv = rename(final_npv, 'value' = 'final_npv')

ggplot2::ggplot(final_npv, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "NPV of a single well", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 15000), breaks = seq(0, 15000, by = 2500)) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
  #ggplot2::geom_vline(linetype = "dashed", data = NULL, mapping = ggplot2::aes(xintercept = mean(final_npv$value), colour = "avg_cost")) +
  #ggplot2::scale_colour_manual(values = c("#FD625E"), name = "", labels = c("Average NPV")) +
  #ggplot2::ggtitle("Normal Distribution Simulation")+
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())

ggplot2::ggsave("npv_simulation_kyle.png", device = "png")
