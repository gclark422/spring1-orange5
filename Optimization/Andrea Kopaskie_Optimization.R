# Loading libraries
library(gurobi)
library(tidyverse)
library(tidyquant)
library(dplyr)

# Setting options
options(tibble.width = Inf)

# Downloading data for the mentioned Stocks and writing it as a csv
tidyquant::tq_get(c("STX", "FTV", "NOW", "IT", "AMD"),
                  from = "2019-01-01",
                  to = "2020-01-31") %>%
  dplyr::select(symbol, date, close) %>%
  readr::write_csv("C:\\Users\\andre\\Desktop\\NCSU\\AA 503\\Optimization\\Project 1\\Stock Price Data.csv")

# Read csv
df <- read.csv("C:\\Users\\andre\\Desktop\\NCSU\\AA 503\\Optimization\\Project 1\\Stock Price Data.csv")

# Format dataframe
df2 <- as.data.frame(group_split(df, symbol))

df2 <- select(df2, c(close, close.1, close.2, close.3, close.4))

colnames(df2) <- c("AMD", "FTV", "IT", "NOW", "STX")

# Calculate returns of stocks
df3 <- data.frame(matrix(ncol = 1, nrow = 272))

df3$AMD_return <- (df2$AMD - lag(df2$AMD)) / lag(df2$AMD)
df3$FTV_return <- (df2$FTV - lag(df2$FTV)) / lag(df2$FTV)
df3$IT_return <- (df2$IT - lag(df2$IT)) / lag(df2$IT)
df3$NOW_return <- (df2$NOW - lag(df2$NOW)) / lag(df2$NOW)
df3$STX_return <- (df2$STX - lag(df2$STX)) / lag(df2$STX)

df3 <- df3[,-1] 
df3 <- df3[-1,] 

# Optimization of stocks
# Minimize risk / return of >= 0.05%

# Calculate the mean vector
mean.vec = apply(df3, 2, mean)

# Calculate covariance vector
cov.vec=cov(df3)

# Create list
model <- list()

# Constraint matrix
model$A <- matrix(c(1,1,1,1,1, mean.vec), nrow=2, byrow=T)

# Objective function
model$Q <- cov.vec
model$obj <- c(0,0,0,0,0)

model$rhs <- c(1, 0.0005)
model$sense <- c('=', '>=')

# Run optimization
result <- gurobi(model, list())
result.names = c("AMD", "FTV", "IT", "NOW", "STX")
names(result$x) = result.names

result$objval
result$x


# Efficient Frontier
A.1 <- matrix(c(rep(1,length(mean.vec)),mean.vec),nrow=2,byrow=T)
A.2 <- diag(length(mean.vec))

model$A <- rbind(A.1,A.2)
model$Q <- cov.vec
model$obj <- rep(0,length(mean.vec))

model$sense <- c('=', rep('>=',length(mean.vec) + 1 ))

param = seq(0.00001, 0.002, by = 0.00001)

eff.front.weight=matrix(nrow=length(param),ncol=length(mean.vec))
eff.front.return=vector(length=length(param))
eff.front.risk=param

for (i in 1:length(param))
  {
    model$rhs <- c(1,param[i],rep(0,length(mean.vec)))
    result <- gurobi(model,list())
    eff.front.return[i]=sum(result$x*mean.vec)
    eff.front.risk[i]=sqrt(result$objval)
    eff.front.weight[i,]=result$x
}

# Prepare data to plot
frontier <- as.data.frame(eff.front.return)
temp <- as.data.frame(eff.front.risk)

frontier <- cbind(frontier, temp)

frontier$return <- frontier$eff.front.return * 100
frontier$risk <- frontier$eff.front.risk * 100

# Plot data
ggplot(frontier, aes(x=frontier$risk, y=frontier$return)) +
  geom_line() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Risk (%)") + ylab("Expected Return (%)") + ggtitle("Efficient Frontier Plot")