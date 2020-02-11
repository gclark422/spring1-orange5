library(gurobi)
library(tidyverse)
library(tidyquant)
library(dplyr)

# Loading in the Data
setwd('C:/Users/k_cla/OneDrive/Desktop/MSA Program/spring1-orange5/Optimization/')
data = read.csv(file = 'Data/Stock Price Data.csv')

# Manipulating the Data
df <- as.data.frame(group_split(data, symbol))

df <- select(df, c(close, close.1, close.2, close.3, close.4))

colnames(df) <- c("AMD", "FTV", "IT", "NOW", "STX")

# Calculate returns of stocks
df2 <- data.frame(matrix(ncol = 1, nrow = 272))

df2$AMD_return <- (df$AMD - lag(df$AMD)) / lag(df$AMD)
df2$FTV_return <- (df$FTV - lag(df$FTV)) / lag(df$FTV)
df2$IT_return <-  (df$IT -  lag(df$IT)) /  lag(df$IT)
df2$NOW_return <- (df$NOW - lag(df$NOW)) / lag(df$NOW)
df2$STX_return <- (df$STX - lag(df$STX)) / lag(df$STX)

df2 <- df2[,-1] 
df2 <- df2[-1,] 

# Mean and Covariance vectors
mean.vec = apply(df2,2,mean)
cov.vec = cov(df2)

# Optimization Model
model <- list()
model$A <- matrix(c(1,1,1,1,1, mean.vec), nrow=2, byrow=T)
model$Q <- cov.vec

model$obj   <- c(0,0,0,0,0)


model$rhs   <- c(1,.0005)
model$sense <- c('=','>=')


result <- gurobi(model,list())

result.names=c('AMD','FTV','IT','NOW','STX')
names(result$x)=result.names
result$objval
result$x

