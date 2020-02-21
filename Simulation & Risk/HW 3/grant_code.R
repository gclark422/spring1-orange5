# repeat this process for 10000 times:
# for the number of wells simulated from a uniform distribution:
#   draw from random truncated normal distribution for hydrocarbon and reservior risks
#   calculate probability of being a producing well
#   use bernoulli distribution to decide if it's a dry or producing well

#install.packages("truncnorm")
library(truncnorm)
library(ggplot2)
library(tidyverse)

### ------------------- Simulation --------------------- ###
hydro = vector() # a vector containing all the simulated probabilities of having hydrocarbon in a well 
resv = vector() # a vector containing all the simulated probabilities of having a developed reservior for a well 
prop = vector() # a vector containing all the proportions of producing wells

for (ii in 1:10000){
  # number of wells is simulated from a uniform distribution
  num_wells = floor(runif(1,10,30)) 
  outcome_count = vector()
  for (jj in 1:num_wells){
    # draw from random truncated normal distribution for both hydrocarbon and reservior risks
    hydrocarbon = rtruncnorm(1, a=0, b=1, mean = 0.99, sd = 0.05)
    hydro = c(hydro,hydrocarbon)
    reservior = rtruncnorm(1,a=0, b=1, mean = 0.8, sd = 0.1)
    resv = c(resv,reservior)
    seal = 1
    struc = 1
    # calculate the probability of being a producing well
    prob_PW = hydrocarbon*reservior*seal*struc
    # use bernoulli distribution (size =1 and n=1) to decide if it's a dry or producing well
    outcome = rbinom(1, 1, prob_PW)
    outcome_count = c(outcome_count,outcome)
  }
  prop = c(prop, sum(outcome_count)/num_wells)
}

length(hydro)
length(resv)
# check out the distribution
hist(hydro)
hist(resv)
hist(prop)

# calculate VaR and ES
VaR = quantile(prop,0.05) 
# 0.5882 for 1000 simulations; 0.5833 for 10000 simulations
tail_values = prop[prop <= VaR]
ES = mean(tail_values) 
# 0.52 for 1000 simulations; 0.5312 for 10000 simulations

qqnorm(prop);qqline(prop, col = 2)
# shapiro.test(prop)

### -------------- visualizations ------------------ ###

prop_plot = ggplot(tibble(pred = prop), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue', bins = 60) + 
  theme_bw() + 
  labs(x = "Proportion of Wet Wells", 
       y = "Frequency", 
       title = "Figure 1: Histogram of Wet Well Proportions") +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold')) + #size = 14
  scale_y_continuous(breaks = c(0, 100, 200, 300,400,500)) #

prop_plot +
  geom_vline(xintercept = c(VaR,ES),color="red",linetype='dashed') + 
  geom_text(aes(x=VaR, label="5% VaR: 0.583", y=300), colour="red", angle=0) +
  geom_text(aes(x=ES, label="5% CVaR: 0.531", y=200), colour="red", angle=0) 

avg_hydro = mean(hydro)
sd(hydro)
sd(hydro)

hydro_plot = ggplot(tibble(pred = hydro), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue', bins = 60) + 
  ggplot2::annotate("text", x = avg_hydro-0.03, 
                    y = 9000, label = "Mean = 0.956", color = 'red', fontface = 2) + 
  ggplot2::annotate("segment", x = avg_hydro, xend = avg_hydro, y = 0, yend = 8750, colour = "red", size = 1) +
  theme_bw() + 
  labs(x = "Probability of Hydrocarbons", 
       y = "Frequency", 
       title = "Figure 2: Histogram of Hydrocarbon Probabilities") +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))  


avg_resv = mean(resv)
sd(resv)
resv_plot = ggplot(tibble(pred = resv), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue', bins = 60) + 
  ggplot2::annotate("text", x = avg_resv, 
                    y = 9000, label = "Mean = 0.795", color = 'red', fontface = 2) + 
  ggplot2::annotate("segment", x = avg_resv, xend = avg_resv, y = 0, yend = 8500, colour = "red", size = 1) +
  theme_bw() + 
  labs(x = "Probability of Reserviors", 
       y = "Frequency", 
       title = "Histogram of Reservior Probabilities") +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold')) #, size = 12


# --------------- some exploration...-----------------
# truncated normal shape
hydrocarbon = rtruncnorm(100000, a=0, b=1, mean = 0.99, sd = 0.05)
hist(hydrocarbon)

# bernoulli distribution
hist(rbinom(10000, 1, 0.78))

hist(runif(10000,10,30))
