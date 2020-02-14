library(readxl)
library(dplyr)
library(car)
library(triangle)
library(ks)
library(lubridate)
library(ggplot2)
library(magrittr)

## turn off exponential notation
options(scipen = 999)

## Miscellaneous Functions Used
standardize <- function(x){
  x.std = (x - mean(x))/sd(x) #standardizing data due to correlation matrix being 1 on the diagonal
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x) #destandardizing data to be non-standardized and put results in real terms
  return(x.old)
}

## Reading and Cleaning Data
path = "C:\\Users\\grant\\Desktop\\IAA\\Spring 1\\spring1-orange5\\Simulation & Risk\\Data\\Analysis_Data.xlsx"
#path = "C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Simulation/Data/Analysis_Data.xlsx"
excel_sheets(path)
drill = read_excel(path,sheet = "Drilling Cost",skip = 2)
colnames(drill) = c("Year","Cost_per_Crude_Oil","Cost_per_Natural_Gas","Cost_per_Dry_Well"
                    ,"Oil_Return","Gas_Return","Well_Return")
drill = drill %>% mutate(Year= year(Year))

price_proj = read_excel(path,sheet = "Price Projections",skip = 2)
colnames(price_proj) = c("Year","High_Price","Low_Price","Reference_Price")

# From HW1 ####

cost <- read_excel("C:\\Users\\grant\\Desktop\\IAA\\Spring 1\\spring1-orange5\\Simulation & Risk\\Data\\Analysis_Data.xlsx",
                   #cost <- read_excel("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Simulation/Data/Analysis_Data.xlsx",
                   sheet = "Drilling Cost",
                   skip = 2
)
cost %<>% select(
  date = Date,
  crud_oil_cost = `U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`,
  nat_gas_cost = `U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)`,
  dry_cost = `U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`,
  crud_oil_ret = `Arithmetic Return - Crude Oil`,
  nat_gas_ret = `Arithmetic Return - Natural Gas`,
  dry_ret = `Arithmetic Return - Dry Well`
)

cost %<>% mutate(year = lubridate::year(date))

# creating dataframe of 48 x 2 oberstations for 1991-2006 cost and return
sub_dat <- cost %>%
  filter(year >= 1991 & year <= 2006)

combined_cost <- with(sub_dat, c(crud_oil_cost, nat_gas_cost, dry_cost))
combined_ret <- with(sub_dat, c(crud_oil_ret, nat_gas_ret, dry_ret))

combined <- data.frame(year = sub_dat$year, cost = combined_cost, ret = as.numeric(combined_ret))

avg_06 <- dplyr::select(combined, year, cost) %>% filter(year == 2006) %>% pull %>% mean
avg_06 = avg_06*1000
avg <- mean(combined$ret)
stdev <- sd(combined$ret)
P19 <- rep(0, 1e4)

## Dry Well Cost Simulation:
cost_dry = rep(0,10000)
set.seed(303)

for (ii in 1:10000){
  P0 <- avg_06
  r <- rnorm(n = 1, mean = avg, sd = stdev)
  
  Pt <- P0 * (1 + r)
  
  for (j in 1:5) {
    r <- rnorm(n = 1, mean = avg, sd = stdev)
    Pt <- Pt * (1 + r)
  }
  
  # 2013-2015
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.07, b = 0.22, c = 0.0917)
    Pt <- Pt * (1 - r)
  }
  
  # 2016-2019
  for (j in 1:4) {
    r <- rtriangle(1, a = 0.02, b = 0.06, c = 0.05)
    Pt <- Pt * (1 + r)
  }
  
  
  acre_cost = 960*rnorm(n=1, mean=600, sd=50)
  seismic_cost = 43000*rnorm(n=1, mean=3, sd=0.35)
  staff_cost = rtriangle(n=1, a = 172000, b = 279500, c = 215000)
  total_cost = acre_cost+seismic_cost+staff_cost
  
  cost_dry[ii] = total_cost + Pt
}

## Print Histogram of Wet Well NPV
avg_dry <- mean(cost_dry)
ggplot(tibble(pred = cost_dry), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue', bins = 60) + 
  theme_bw() +
  annotate("text", x = avg_dry, 
           y = 800, label = "Mean Cost = $4.7 million", color = 'red', fontface = 2) + 
  annotate("segment", x = avg_dry, xend = avg_dry, y = 0, yend = 775, colour = "red", size = 1.5) + 
  labs(x = "Cost(in millions of dollars)", 
       y = "Frequency", 
       title = "Histogram of Dry Well Cost Possibilities") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
  scale_x_continuous(limits = c(0, 15e6), breaks = c(0, 4e6, 8e6, 12e6), labels = c(0, 4, 8, 12))

list_hist = hist(cost_dry, breaks=100)
hist(cost_dry, 
     breaks = 100,
     main="Histogram of Dry Well Cost Possibilities",
     xlab="Cost",
     border="blue",
     col="grey",
     las=1)
abline(v=mean(cost_dry), col="green")
text(1.05*mean(cost_dry), 0.95*max(list_hist$count), paste("Mean = ", round(mean(cost_dry),2), sep=""), col = "green", adj =c(0,0))

# 95% CI
sem <- sd(cost_dry) / sqrt(length(cost_dry)) # (4687988, 4748311)
# 95% confidence intervals of the mean
c(mean(cost_dry) - 1.96 * sem, mean(cost_dry) + 1.96 * sem)

#cost at 99% quantile
quantile(cost_dry, .99) # 9501966


## Wet Well Cost Simulation:

#setup components of simulation
num=100000 ## number of simulations
cost_wet = rep(0,num)
rev_wet = rep(0,num)
NPV_sim = rep(0,num)
set.seed(404)
R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R))
high_vec = price_proj$High_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]
low_vec = price_proj$Low_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]
mid_vec = price_proj$Reference_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]


#random generate simulation num of correlated initial and decline rates
#initial rates
init_rate = rlnorm(n=num, meanlog=6, sdlog=0.28)
decline_rate = runif(n=num, min=0.15, max=0.32)
#correlate initial rates
Both.r <- cbind(standardize(init_rate), standardize(decline_rate)) # standardizing random draws
Corr.r <- U %*% t(Both.r) # multiplying U by the matrix with both standardized random draws
Corr.r <- t(Corr.r) # transpose matrix
final.Corr.r <<- cbind(destandardize(Corr.r[,1], init_rate), destandardize(Corr.r[,2], decline_rate)) # de-standardize
#set initial rates for 2019
init_rate_all = final.Corr.r[,1] # first column is initial rates
decline_rate_all = final.Corr.r[,2] # second column is decline rates (CORRELATED!!)


#calculate costs and revenues for each year (15 year forecast)
for (ii in 1:num){
  
  P0 <- avg_06
  r <- rnorm(n = 1, mean = avg, sd = stdev)
  
  Pt <- P0 * (1 + r)
  
  for (j in 1:5) {
    r <- rnorm(n = 1, mean = avg, sd = stdev)
    Pt <- Pt * (1 + r)
  }
  
  # 2013-2015
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.07, b = 0.22, c = 0.0917)
    Pt <- Pt * (1 - r)
  }
  
  # 2016-2019
  for (j in 1:4) {
    r <- rtriangle(1, a = 0.02, b = 0.06, c = 0.05)
    Pt <- Pt * (1 + r)
  }
  
  ## Year 0 Costs:
  acre_cost = 960*rnorm(n=1, mean=600, sd=50)
  seismic_cost = 43000*rnorm(n=1, mean=3, sd=0.35)
  staff_cost = rtriangle(n=1, a = 172000, b = 279500, c = 215000)
  setup_cost = rnorm(n=1, mean=390000, sd=50000)
  init_cost = acre_cost+seismic_cost+staff_cost+setup_cost+Pt
  
  ## Year 1 & Beyond
  #set initial rates
  royalty_rate = rnorm(n=1, mean=0.25, sd=0.02) #stays the same each year
  init_rate = init_rate_all[ii] #updates every year based on end_rate equation
  decline_rate = decline_rate_all[ii] #stays the same each year
  
  #build revenue & cost compounding for each year
  total_cost = init_cost
  total_rev =0
  FNR = rep(0,15)
  
  #for loop for all 15 years
  for (jj in 1:15){
    #find end rate and oil volume for this year
    end_rate = (1-decline_rate)*init_rate
    oil_vol= 365*(init_rate+end_rate)/2
    
    #obtain price from sheet (high, low, mean could be moved outside for loop for time saving)
    oil_price = rtriangle(n=1, a=low_vec[jj], b=high_vec[jj], c=mid_vec[jj]) 
    
    #calculate revenue
    revenue= oil_vol*oil_price
    
    #calculate royalties on revenue
    NRI = revenue*royalty_rate
    new_rev = revenue-NRI
    
    #calculate operating costs
    op_cost_per_barrel = rnorm(n=1, mean=2.25, sd=0.3)
    op_cost=oil_vol*op_cost_per_barrel
    
    #calculate tax
    tax = new_rev*0.046
    
    #final cost & revenue for year
    cost=tax+NRI+op_cost+staff_cost
    total_cost=total_cost+cost
    final_rev = revenue-cost
    total_rev=total_rev+final_rev
    
    #reset initial
    init_rate = end_rate
    
    #FNR Storage
    FNR[jj] = final_rev
  }
  
  cost_wet[ii]=total_cost
  rev_wet[ii]=total_rev-total_cost
  
  #NPV Calculation
  NPV = -init_cost
  for (i in 1:15){
    NPV = NPV+(FNR[i]/((1+0.1)^i))
  }
  NPV_sim[ii]=NPV
}

## Print Histogram of Wet Well NPV
avg_npv <- mean(NPV_sim)
ggplot(tibble(pred = NPV_sim), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue', bins = 60) + 
  theme_bw() +
  annotate("text", x = avg_npv, 
           y = 6900, label = "Mean NPV = $12.5 million", color = 'red', fontface = 2) + 
  annotate("segment", x = avg_npv, xend = avg_npv, y = 0, yend = 6800, colour = "red", size = 1.5) + 
  labs(x = "Revenue(in millions of dollars)", 
       y = "Frequency", 
       title = "Histogram of Wet Well Revenue Possibilities") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
  scale_x_continuous(limits = c(-1e7, 4e7), breaks = c(-1e7, 0, 1e7, 2e7, 3e7), labels = c(-10, 0, 10, 20, 30))

list_hist = hist(NPV_sim, breaks=100)
hist(NPV_sim, 
     breaks=100,
     main="Histogram of Wet Well Revenue Possibilities",
     xlab="Revenue",
     border="blue",
     col="grey",
     las=1
)
abline(v=mean(NPV_sim), col="green")
abline(v=0, col="red")
text(mean(NPV_sim)+5000000, 0.95*max(list_hist$count), paste("Mean = ", round(mean(NPV_sim),2), sep=""), col = "green", adj =c(0,0))
text(5000000, 0.95*max(list_hist$count), "0", col = "red", adj = c(-0.1, -0.1))

es_values <- NPV_sim[NPV_sim <= quantile(NPV_sim, .01)]
View(sort(es_values))
mean(es_values)
## Key Stats
round(mean(NPV_sim),2) ## mean of wet well NPV
length(NPV_sim[which(NPV_sim<0)]) ## simulations where NPV below 0
(length(NPV_sim[which(NPV_sim<0)])/num)*100 ## percent of simulations below 0

