###############################
#                             #
#       Optimization:         #
#       Final Project         #
#                             #
#        Ellie Caldwell       #
#                             #
###############################

#Load in libraries
library(gurobi)
library(prioritizr)
library(dplyr)
library(tidyverse)

#Load in data
stocks=read.table('C:\\Users\\Ellie Caldwell\\Documents\\spring1-orange5\\Optimization\\Data\\Stock Price Data.csv',sep=',',header=T)

#Flip groups to columns
stocks_group <- stocks %>% group_by(symbol)
stocks_split <- group_split(stocks_group)
stocks_final <- as.data.frame(stocks_split)
stocks_final <- stocks_final %>% select(c(close, close.1, close.2, close.3, close.4)) #AMD, FTV, IT, NOW, STX
colnames(stocks_final) <- c("AMD", "FTV", "IT", "NOW", "STX") 

#Calculate returns
returns <- stocks_final %>% mutate(AMD_return = (AMD - lag(AMD)) / lag(AMD),
                                   FTV_return = (FTV - lag(FTV)) / lag(FTV),
                                   IT_return = (IT - lag(IT)) / lag(IT),
                                   NOW_return = (NOW - lag(NOW)) / lag(NOW),
                                   STX_return = (STX - lag(STX)) / lag(STX))
returns_final <- returns %>% select(c(AMD_return,
                                FTV_return,
                                IT_return,
                                NOW_return,
                                STX_return))
#Remove NA row
returns_final <- returns_final[-1,]
#Calculate mean
mean.vec=apply(returns_final, 2, mean)
#Calculate covariance
returns_final <- as.matrix(returns_final)
cov.vec=cov(returns_final)

#Optimization
model <- list()

model$A     <- matrix(c(1,1,1,1,1,mean.vec),nrow=2,byrow=T)
model$Q     <- cov.vec
model$obj   <- c(0,0,0,0,0)
model$rhs   <- c(1,0.0005)
model$sense <- c('=', '>=')
result <- gurobi(model,list())
result.names=c("AMD", "FTV", "IT", "NOW", "STX")
names(result$x)=result.names
result$objval
result$x

################################ Efficient Frontier
##############################################
A.1     <- matrix(c(rep(1,length(mean.vec)),mean.vec),nrow=2,byrow=T)
A.2     <-diag(length(mean.vec))

model$A <- rbind(A.1,A.2)
model$Q     <- cov.vec
model$obj   <- rep(0,length(mean.vec))
model$sense <- c('=','>=', rep('>=',length(mean.vec)))

param=seq(0.00001, 0.002, by = 0.00001)
eff.front.weight=matrix(nrow=length(param),ncol=length(mean.vec))
eff.front.return=vector(length=length(param))
eff.front.risk=param
for (i in 1:length(param))
{
  model$rhs <- c(1,param[i],rep(0,length(mean.vec)))
  result <-gurobi(model,list())
  eff.front.return[i]=sum(result$x*mean.vec)
  eff.front.risk[i]=sqrt(result$objval)
  eff.front.weight[i,]=result$x
}


plot(eff.front.risk,eff.front.return,type='l')
