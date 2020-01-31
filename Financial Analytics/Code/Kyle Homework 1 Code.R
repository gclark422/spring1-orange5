# Financial Analytics Homework 1 Code - Kyle Clapper

# Libraries for Analysis #
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(haven)
library(rockchalk)
library(forcats)
library(stringr)
library(rpart)
library(caret)

##################################################### Data Preparation ###########################################################

# Loading Data

setwd("C:\\Users\\k_cla\\OneDrive\\Desktop\\MSA Program\\spring1-orange5\\Financial Analytics\\")

accepts <- read_sas("accepted_customers.sas7bdat", NULL)

# Renaming the frequency column
names(accepts)[names(accepts) == "_freq_"] <- "weight"

# Removing Age and Status
accepts <- subset(accepts, select = -c(AGE,STATUS))

# Combining Levels of Cards variable
accepts$CARDS <- as.factor(accepts$CARDS)
accepts$CARDS <- combineLevels(accepts$CARDS,c('American Express','VISA mybank','VISA Others','Other credit car'), newLabel = "Other Card")


# Removing Commas
accepts$PRODUCT <- gsub(","," ",accepts$PRODUCT)
accepts$PROF <- gsub(","," ",accepts$PROF)


# Understand Target Variable #

accepts$good <- abs(accepts$GB - 1)
table(accepts$good)

# Setting the target variable as numeric
accepts$good <- as.numeric(accepts$good)

# Setting Categorical Variables as Factors
accepts$TITLE <- as.factor(accepts$TITLE)
accepts$STATUS <- as.factor(accepts$STATUS)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$RESID <- as.factor(accepts$RESID)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$CAR <- as.factor(accepts$CAR)
accepts$CARDS <- as.factor(accepts$CARDS)

# Variables I am choosing to code as Factors -- anything numeric with less than 5 levels
accepts$TEL <- as.factor(accepts$TEL)
accepts$NMBLOAN <- as.factor(accepts$NMBLOAN)
accepts$FINLOAN <- as.factor(accepts$FINLOAN)
accepts$EC_CARD <- as.factor(accepts$EC_CARD)
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$LOCATION <- as.factor(accepts$LOCATION)
accepts$DIV <- as.factor(accepts$DIV)

# Create Training and Validation #
set.seed(12345)

train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$good)
table(test$good)

##################################################### Information Value ###########################################################

# Need to make sure train is a dataframe
train <- as.data.frame(train)

# Gathering the names of numeric variables in data #
num_names <- names(train)[sapply(train, is.numeric)] 

# Creating empty list to store all results #
result_num <- list() 

# Looping through the numeric variables
for(i in 1:length(num_names)){
  result_num[[num_names[i]]] <- smbinning(df = train, y = "good", x = num_names[i])
}

# Binning of Factor Variables #

# Gathering the names of factor variables in data #
fac_names <- names(train)[sapply(train, is.factor)] 

# Creating empty list to store all results #
result_fac <- list() 

# Looping through the factor variables
for(i in 1:length(fac_names)){
  result_fac[[fac_names[i]]] <- smbinning.factor(df = train, y = "good", x = fac_names[i])
}

# Combining the Lists
result_all <- c(result_num,result_fac)


# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary

##################################################### Variable Binning ###########################################################

# Taking only the significant variables and re-splitting training/test
accepts <- subset(accepts, select = c(TMJOB1,INCOME,PERS_H,CARDS,EC_CARD,good, weight))

# Create Training and Validation #
set.seed(12345)

train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

train <- as.data.frame(train)
train <- as.data.frame(test)

table(train$good)
table(test$good)


# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig_num <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_num[[num_names[i]]] <- check_res
  }
}

# Binning of Factor Variables - IV >= 0.1 #
fac_names <- names(train)[sapply(train, is.factor)] # Gathering the names of numeric variables in data #

result_all_sig_fac <- list() # Creating empty list to store all results #

for(i in 1:length(fac_names)){
  check_res <- smbinning.factor(df = train, y = "good", x = fac_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_fac[[num_names[i]]] <- check_res
  }
}


# Generating Variables of Bins and WOE Values for Continuous Variables#
for(i in 1:length(result_all_sig_num)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig_num[[i]], chrname = paste(result_all_sig_num[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_num)) {
  for (i in 1:nrow(train)) {
    bin_name_num <- paste(result_all_sig_num[[j]]$x, "_bin", sep = "")
    bin_num <- substr(train[[bin_name_num]][i], 2, 2)
    
    woe_name_num <- paste(result_all_sig_num[[j]]$x, "_WOE", sep = "")
    
    if(bin_num == 0) {
      bin_num <- dim(result_all_sig_num[[j]]$ivtable)[1] - 1
      train[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    } else {
      train[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    }
  }
}

# Generating Variables of Bins and WOE Values for Factor Variables#
for(i in 1:length(result_all_sig_fac)) {
  train <- smbinning.factor.gen(df = train, ivout = result_all_sig_fac[[i]], chrname = paste(result_all_sig_fac[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_fac)) {
  for (i in 1:nrow(train)) {
    bin_name_fac <- paste(result_all_sig_fac[[j]]$x, "_bin", sep = "")
    bin_fac <- substr(train[[bin_name_fac]][i], 2, 2)
    
    woe_name_fac <- paste(result_all_sig_fac[[j]]$x, "_WOE", sep = "")
    
    if(bin_fac == 0) {
      bin_fac <- dim(result_all_sig_fac[[j]]$ivtable)[1] - 1
      train[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    } else {
      train[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    }
  }
}
##################################################### Initial Modeling ###########################################################

# Build Initial Logistic Regression #
initial_score <- glm(data = train, good ~
                       TMJOB1_WOE + 
                       INCOME_WOE + 
                       PERS_H_WOE +
                       CARDS_WOE +
                       EC_CARD_WOE, 
                     weights = train$weight, family = "binomial")

summary(initial_score)


# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", plot = "auc")

# Evaluate the Initial Model - Testing Data #

for(i in 1:length(result_all_sig_num)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig_num[[i]], chrname = paste(result_all_sig_num[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_num)) {
  for (i in 1:nrow(test)) {
    bin_name_num <- paste(result_all_sig_num[[j]]$x, "_bin", sep = "")
    bin_num <- substr(test[[bin_name_num]][i], 2, 2)
    
    woe_name_num <- paste(result_all_sig_num[[j]]$x, "_WOE", sep = "")
    
    if(bin_num == 0) {
      bin_num <- dim(result_all_sig_num[[j]]$ivtable)[1] - 1
      test[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    } else {
      test[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    }
  }
}

for(i in 1:length(result_all_sig_fac)) {
  test <- smbinning.factor.gen(df = test, ivout = result_all_sig_fac[[i]], chrname = paste(result_all_sig_fac[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_fac)) {
  for (i in 1:nrow(test)) {
    bin_name_fac <- paste(result_all_sig_fac[[j]]$x, "_bin", sep = "")
    bin_fac <- substr(test[[bin_name_fac]][i], 2, 2)
    
    woe_name_fac <- paste(result_all_sig_fac[[j]]$x, "_WOE", sep = "")
    
    if(bin_fac == 0) {
      bin_fac <- dim(result_all_sig_fac[[j]]$ivtable)[1] - 1
      test[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    } else {
      test[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", plot = "auc")

# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(0,725), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(0,725), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(0,725), main = "Distribution of Scores", xlab = "Score")


##################################################### Reject Analysis ###########################################################

####### Code to Load data and clean #########

rejects <- read_sas("rejected_customers.sas7bdat", NULL)

# Renaming the frequency column
names(rejects)[names(rejects) == "_freq_"] <- "weight"

# Removing Age and Status
rejects <- subset(rejects, select = -c(AGE,STATUS,INC, INC1))

# Combining Levels of Cards variable
rejects$CARDS <- as.factor(rejects$CARDS)
rejects$CARDS <- combineLevels(rejects$CARDS,c('VISA Citibank','VISA Others','Other credit car'), newLabel = "Other Card")


# Removing Commas
rejects$PRODUCT <- gsub(","," ",rejects$PRODUCT)
rejects$PROF <- gsub(","," ",rejects$PROF)

# Setting Categorical Variables as Factors
rejects$TITLE <- as.factor(rejects$TITLE)
rejects$PRODUCT <- as.factor(rejects$PRODUCT)
rejects$RESID <- as.factor(rejects$RESID)
rejects$NAT <- as.factor(rejects$NAT)
rejects$PROF <- as.factor(rejects$PROF)
rejects$CAR <- as.factor(rejects$CAR)
rejects$CARDS <- as.factor(rejects$CARDS)

# Variables I am choosing to code as Factors -- anything numeric with less than 5 levels
rejects$TEL <- as.factor(rejects$TEL)
rejects$NMBLOAN <- as.factor(rejects$NMBLOAN)
rejects$FINLOAN <- as.factor(rejects$FINLOAN)
rejects$EC_CARD <- as.factor(rejects$EC_CARD)
rejects$BUREAU <- as.factor(rejects$BUREAU)
rejects$LOCATION <- as.factor(rejects$LOCATION)
rejects$DIV <- as.factor(rejects$DIV)

rejects <- as.data.frame(rejects)

######### Replacing original variables with WOE values ##########

# Generating Variables of Bins and WOE Values for Continuous Variables#
rejects_scored <- rejects

for(i in 1:length(result_all_sig_num)) {
  rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig_num[[i]], chrname = paste(result_all_sig_num[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_num)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name_num <- paste(result_all_sig_num[[j]]$x, "_bin", sep = "")
    bin_num <- substr(rejects_scored[[bin_name_num]][i], 2, 2)
    
    woe_name_num <- paste(result_all_sig_num[[j]]$x, "_WOE", sep = "")
    
    if(bin_num == 0) {
      bin_num <- dim(result_all_sig_num[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    } else {
      rejects_scored[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    }
  }
}

# Generating Variables of Bins and WOE Values for Factor Variables#
for(i in 1:length(result_all_sig_fac)) {
  rejects_scored <- smbinning.factor.gen(df = rejects_scored, ivout = result_all_sig_fac[[i]], chrname = paste(result_all_sig_fac[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_fac)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name_fac <- paste(result_all_sig_fac[[j]]$x, "_bin", sep = "")
    bin_fac <- substr(rejects_scored[[bin_name_fac]][i], 2, 2)
    
    woe_name_fac <- paste(result_all_sig_fac[[j]]$x, "_WOE", sep = "")
    
    if(bin_fac == 0) {
      bin_fac <- dim(result_all_sig_fac[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    } else {
      rejects_scored[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    }
  }
}

pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

# Reject Inference - Hard Cut-off #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects$good <- as.numeric(rejects_scored$pred < 0.9789)
rejects$good <- as.factor(rejects$good)

# Downsampling Rejects so we have the same ratio of good/bad as accepts ************************************************

rejects$weight <- ifelse(rejects$good == 1, 20, .67)

# New Combined Data Set #
comb_hard <- rbind(accepts, rejects)

##################################################### Final Model Building ###########################################################

comb <- comb_hard

set.seed(12345)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.75*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

############ Information Value Summary ###########

# Need to make sure train is a dataframe
train_comb <- as.data.frame(train_comb)
train_comb$good <- as.numeric(train_comb$good)

# Gathering the names of numeric variables in data #
num_names <- names(train_comb)[sapply(train_comb, is.numeric)] 

###################### Unsure about this Code####################
# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig_num <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train_comb, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_num[[num_names[i]]] <- check_res
  }
}

# Binning of Factor Variables - IV >= 0.1 #
fac_names <- names(train_comb)[sapply(train_comb, is.factor)] # Gathering the names of numeric variables in data #

result_all_sig_fac <- list() # Creating empty list to store all results #

for(i in 1:length(fac_names)){
  check_res <- smbinning.factor(df = train_comb, y = "good", x = fac_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_fac[[num_names[i]]] <- check_res
  }
}
##########################################################################################################



# Creating empty list to store all results #
result_num <- list() 

# Looping through the numeric variables
for(i in 1:length(num_names)){
  result_num[[num_names[i]]] <- smbinning(df = train_comb, y = "good", x = num_names[i])
}

# Binning of Factor Variables #

# Gathering the names of factor variables in data #
fac_names <- names(train_comb)[sapply(train_comb, is.factor)] 

# Creating empty list to store all results #
result_fac <- list() 

# Looping through the factor variables
for(i in 1:length(fac_names)){
  result_fac[[fac_names[i]]] <- smbinning.factor(df = train_comb, y = "good", x = fac_names[i])
}

# Combining the Lists
result_all <- c(result_num,result_fac)


# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train_comb, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary

################### Variable Binning #####################

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig_num <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train_comb, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_num[[num_names[i]]] <- check_res
  }
}

# Binning of Factor Variables - IV >= 0.1 #
fac_names <- names(train_comb)[sapply(train_comb, is.factor)] # Gathering the names of numeric variables in data #

result_all_sig_fac <- list() # Creating empty list to store all results #

for(i in 1:length(fac_names)){
  check_res <- smbinning.factor(df = train_comb, y = "good", x = fac_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig_fac[[num_names[i]]] <- check_res
  }
}


# Generating Variables of Bins and WOE Values for Continuous Variables#
for(i in 1:length(result_all_sig_num)) {
  train_comb <- smbinning.gen(df = train_comb, ivout = result_all_sig_num[[i]], chrname = paste(result_all_sig_num[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_num)) {
  for (i in 1:nrow(train_comb)) {
    bin_name_num <- paste(result_all_sig_num[[j]]$x, "_bin", sep = "")
    bin_num <- substr(train_comb[[bin_name_num]][i], 2, 2)
    
    woe_name_num <- paste(result_all_sig_num[[j]]$x, "_WOE", sep = "")
    
    if(bin_num == 0) {
      bin_num <- dim(result_all_sig_num[[j]]$ivtable)[1] - 1
      train_comb[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    } else {
      train_comb[[woe_name_num]][i] <- result_all_sig_num[[j]]$ivtable[bin_num, "WoE"]
    }
  }
}

# Generating Variables of Bins and WOE Values for Factor Variables#
for(i in 1:length(result_all_sig_fac)) {
  train_comb <- smbinning.factor.gen(df = train_comb, ivout = result_all_sig_fac[[i]], chrname = paste(result_all_sig_fac[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig_fac)) {
  for (i in 1:nrow(train_comb)) {
    bin_name_fac <- paste(result_all_sig_fac[[j]]$x, "_bin", sep = "")
    bin_fac <- substr(train_comb[[bin_name_fac]][i], 2, 2)
    
    woe_name_fac <- paste(result_all_sig_fac[[j]]$x, "_WOE", sep = "")
    
    if(bin_fac == 0) {
      bin_fac <- dim(result_all_sig_fac[[j]]$ivtable)[1] - 1
      train_comb[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    } else {
      train_comb[[woe_name_fac]][i] <- result_all_sig_fac[[j]]$ivtable[bin_fac, "WoE"]
    }
  }
}
