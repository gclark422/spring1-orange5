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

# Setting a seed
set.seed(12345)

############################################ First Simulation Code Under Normal Assumption ##################################################

mean = mean(data$changes)
stdev = sd(data$changes)

PN <- rep(0,100000)
for(i in 1:100000){
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

# making nicer histograms with ggplot

PN <- as.data.frame(PN)
PN = rename(PN, 'value' = 'PN')

ggplot2::ggplot(PN, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Average Cost in 2020", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 30000), breaks = seq(0, 30000, by = 5000)) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
  ggplot2::geom_vline(linetype = "dashed", data = NULL, mapping = ggplot2::aes(xintercept = 2279.8, colour = "avg_cost")) +
  ggplot2::scale_colour_manual(values = c("#FD625E"), name = "", labels = c("Average Cost in 2006")) +
  #ggplot2::ggtitle("Normal Distribution Simulation")+
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())
ggplot2::ggsave("simulation_normal_kyle.png", device = "png")

# Saving out the final distribution
write.csv(PN,"kyle_drilling_costs.csv", row.names = FALSE)

########################################### Second Simulation Code with Density Estimation ###################################################

Density.PD <- density(data$changes, bw="SJ-ste")
Density.PD # this code give us the bandwidth

PD <- rep(0,100000)
for(i in 1:100000){
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

# Making nicer histograms with ggplot

PD <- as.data.frame(PD)
PD = rename(PD, 'value' = 'PD')

ggplot2::ggplot(PD, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Average Cost in 2020", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 30000), breaks = seq(0, 30000, by = 5000)) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
  ggplot2::geom_vline(linetype = "dashed", data = NULL, mapping = ggplot2::aes(xintercept = 2279.8, colour = "avg_cost")) +
  ggplot2::scale_colour_manual(values = c("#FD625E"), name = "", labels = c("Average Cost in 2006")) +
  ggplot2::ggtitle("Kernel Estimate Simulation")+
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())
ggplot2::ggsave("simulation_kernel_density_kyle.png", device = "png")

################################################## Checking Normality of the changes ###########################################################

# QQ Plot
qqnorm( data$changes, main='changes')
qqline( data$changes )

# Formal test
ad.test(data$changes) # Anderson Darling test indicates the data is not significantly different from a normal distribution


# We should move forward with the first simulation that assumes normality. The sample size is realively small for the kernel density estimator to provide
# a reliable result
