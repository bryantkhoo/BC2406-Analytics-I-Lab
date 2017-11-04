# ========================================================================================================
# Purpose:      Rscript codes for lab quiz
# Author:       Bryant Khoo
# DOC:          24-10-2017
#=========================================================================================================

# Custom function to setwd to wherever the R-script is.
#install.packages('rstudioapi')
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
# set_wd to where script is
set_wd()

# Read CSV file
# Ensure that csv's relative path is correct w.r.t wd
dividends.data <- read.csv('dividend.csv')

# How many rows and columns are in the dataset? 124577 rows and 20 columns
dim(dividends.data)

summary(dividends.data)

# Using ggplot 2

library(ggplot2)

# Data exploration 1 : Predictive value?
# Using scatter plot and smoothing
g <- ggplot(dividends.data, aes(x = dividends.data$dvc, y = dividends.data$ib)) 
g + geom_point(shape=1) + geom_smooth(method=lm)+labs(x = "Dividends Common") + labs(y = "Income before Extraordinary Items")
# Findings: Before cleaning there is possibly some relationship between dvc and ib

# Data exploration 2 : Is the data sufficient?
summary(dividends.data)
# There are NA values in dvc and ib ( 26803 and 25399 respectively )
# Check custID for the 56 NAs. Same?
dvc_na <- subset(dividends.data, is.na(dividends.data$dvc))
ib_na <- subset(dividends.data, is.na(dividends.data$ib))
#install.packages('sqldf')
require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM dvc_na EXCEPT SELECT * FROM ib_na')
# Findings: 1671 observations that were NA in dvc were not NA in ib


# Data exploration 3: Data Analomalies?
summary(dividends.data)
sum(dividends.data$dvc < 0, na.rm = TRUE) # Need to set na.rm to True
# Check all rows that are less than 0 and not NA
dividends.data[(dividends.data$dvc < 0 & !is.na(dividends.data$dvc)),]
# Findings: Negative dividends


# Question 2:

# 2a

# Keep records with stock exchange code 11, 12 and 14
dividends.data.2a <- subset(dividends.data, 
                            subset=(dividends.data$exchg == 11 | 
                                      dividends.data$exchg == 12 | 
                                        dividends.data$exchg == 14 ))
nrow(dividends.data.2a) # 70161 rows
# Check again
summary(dividends.data.2a)

# 2b

# Focus on US dominated companies
dividends.data.2b <- subset(dividends.data.2a, 
                            subset=(dividends.data.2a$curcd == 'USD'))
nrow(dividends.data.2b) # 68876 rows
# Check again
summary(dividends.data.2b)

# 2c

# Exclude utilities and financial companies
dividends.data.2c <- subset(dividends.data.2b, 
                            subset=((dividends.data.2b$sic < 6000 & dividends.data.2b$sic > 4999 )|
                                      (dividends.data.2b$sic < 4000) | (dividends.data.2b$sic > 6999)))
nrow(dividends.data.2c) # 35791 rows
# Check again
summary(dividends.data.2c)
g <- ggplot(dividends.data.2c, aes(x = dividends.data.2c$sic, y = dividends.data.2c$ib)) 
g + geom_point(shape=1)+labs(x = "SIC") + labs(y = "Income")

# 2d

# Remove observations that do not report income

dividends.data.2d <- subset(dividends.data.2c, !is.na(dividends.data.2c$ib))
nrow(dividends.data.2d) # 22159 rows left
summary(dividends.data.2d) # no more NA values!

# 2e

# What other data cleaning steps should be executed that are obvious and necessary?
summary(dividends.data.2d)
# There are still na values in dvc, negative dvc
dividends.data.2e <- subset(dividends.data.2d, subset=(!is.na(dividends.data.2d$dvc) & !(dividends.data.2d$dvc < 0)))
dividends.data.2e <- subset(dividends.data.2e, subset=(!is.na(dividends.data.2e$xint)))
summary(dividends.data.2e$ib) # no need for ib
dividends.data.2e <- subset(dividends.data.2e, subset=(!is.na(dividends.data.2e$spi)))
dividends.data.2e <- subset(dividends.data.2e, subset=(!is.na(dividends.data.2e$idit)))
dividends.data.2e <- subset(dividends.data.2e, subset=(!is.na(dividends.data.2e$costat)))
nrow(dividends.data.2e) # 13252
summary(dividends.data.2e) # no more NA values for dvc and no more negative dvc!

# There are 1355 NA values in prstkc
summary(dividends.data.2e$prstkc)
# Lets find out what these NA vaues means.
# prstk is the variable for purchase of common and preferred stock
# Explore how prstkc is scattered if its na vs its not, does it significantly affect div?
g <- ggplot(dividends.data.2e, aes(x = is.na(dividends.data.2e$prstkc), y = dividends.data.2e$dvc)) 
g + geom_point(shape=1)+labs(x = "prstkc") + labs(y = "Dividends") # Not very obvious

# Check if % of those NA with +ve dvc is same as those not NA

# For those that are NA
length(which(is.na(dividends.data.2e$prstkc) & dividends.data.2e$dvc>0)) #203
length(which(is.na(dividends.data.2e$prstkc) & dividends.data.2e$dvc==0)) #1152, roughly 15% dvc>0
length(which((dividends.data.2e$prstkc==0) & dividends.data.2e$dvc>0)) #2378
length(which((dividends.data.2e$prstkc==0) & dividends.data.2e$dvc==0)) #7969, roughly 23% dvc>0
# Percentage quite close, might be off due to difference in size.
# To avoid making wrong assumptions, I will ommit NAs (Last resort)


length(which(dividends.data.2e$prstkc==0)) #10347 are 0
# Perhaps prstkc NA are for missing records.
# min prstkc is -1. How many?
length(which(dividends.data.2e$prstkc==-1)) #1
sum(is.na(dividends.data.2e$prstkc)) #1355 are NA values
dividends.data.2e <- subset(dividends.data.2e, subset=(dividends.data.2e$prstkc >=0)) # -1 and NAs removed


# 3

# Generate summary table
df <- data.frame(FiscalYear=as.character(),
                 NoOfComp=as.character(), 
                 NoOfLoss=as.character(), 
                 NoOfDivPaying=as.character(),
                 NoOfStockRepurc=as.character(),
                 stringsAsFactors=FALSE) 
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

for (year in 2007:2017){
  df<-rbind(df, data.frame(FiscalYear=year,
                           NoOfComp=length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year])), 
                           NoOfLoss=length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$ib < 0])),
                           NoOfDivPaying=length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$dvc > 0])),
                           NoOfStockRepurc=length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$prstkc > 0]))
                           ))
}
# I assumed that companies that did stock repurchases had prstkc value >0


# 4
# Choosing the varibles for the models

library(leaps)
# div as y, find the other Xs
summary(dividends.data.2e)
# consider the following variables for selection:
# fyear, indfmt, tic, at, ib, idit, spi, xint, exchg, costat, sic

# Full model for linear regression 
m.lin.full <- lm(dividends.data.2e$dvc ~ dividends.data.2e$indfmt 
             + dividends.data.2e$at + dividends.data.2e$ib + dividends.data.2e$idit
             + dividends.data.2e$spi + dividends.data.2e$xint+ dividends.data.2e$exchg 
             + dividends.data.2e$costat + dividends.data.2e$sic , data = dividends.data.2e)
m.lin.empty <- lm(dividends.data.2e$dvc ~ 1, data = dividends.data.2e)
summary(m.lin.full)

# Using leaps package to find best subset using BIC, adjr2 and cp
b.subsets.lin <- regsubsets(formula(m.lin.full), nbest = 1, nvmax = 9, data = dividends.data.2e)
summary(b.subsets.lin)
plot(b.subsets.lin)
b.subset.lin.bic <- which.min(summary(b.subsets.lin)$bic)
b.subset.lin.bic
## Best subset with 8 variables has the lowest BIC.
summary(b.subsets.lin)$bic
coef(b.subsets.lin,8)
b.subset.lin.adjr2 <- which.max(summary(b.subsets.lin)$adjr2)
b.subset.lin.adjr2
## Best subset with 9 variables has the highest adjr2.
summary(b.subsets.lin)$adjr2
coef(b.subsets.lin,9)
b.subset.lin.cp <- which.min(summary(b.subsets.lin)$cp)
b.subset.lin.cp
## Best subset with 9 variables has the lowest CP
summary(b.subsets.lin)$cp
coef(b.subsets,9)

# Using stepwise
m.lin.step <- step(m.lin.empty, direction = "forward", scope = formula(m.lin.full), data = dividends.data.2e)
summary(m.lin.step) # all 9 variables


# Is dividend payout affected by losses?
attach(dividends.data.2e)
set.seed(3177)
library(caTools)
# 4 a

# 70% trainset. Stratify on Y = dvc. Caution: Sample size only 32 in this example.
train <- sample.split(Y = dividends.data.2e$dvc, SplitRatio = 0.7)
trainset <- subset(dividends.data.2e, train == T)
testset <- subset(dividends.data.2e, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$dvc)
summary(testset$dvc)

attach(trainset)
# model 1 using linear regression on our selected model
m1 <- lm(dvc ~ idit + ib + xint + at + exchg + spi + indfmt + sic + costat, data = trainset)
summary(m1)

coef(m1) # only the coefficients in the model
formula(m1) # Formula used in the model

dividends.data.2e$dvc # Actual Y
fitted(m1) # Model.1 Predicted Y
residuals(m1) # Errors = Actual - Model Predicted

RMSE.m1.train <- sqrt(mean(residuals(m1)^2))
## 311.546

summary(abs(residuals(m1)))
## Trainset: Min Abs Error = 0.011, Max Abs Error = 7960.104


par(mfrow=c(2,2))  # 4 Charts in a Plot.
plot(m1)
## There are outliers

# Apply model from trainset to predict testset.
predict.m1.test <- predict(m1, newdata = testset)
testset.error <- testset$dvc - predict.m1.test
RMSE.m1.test <- sqrt(mean(testset.error^2))
## 409.3515
summary(abs(testset.error))
# Testset: Min Abs Error = 0.018, Max Abs Error = 12467.401


# First discretise dvc for logistic regression later (categorical Y)
minimum <- min(dividends.data.2e$dvc)
maximum <- max(dividends.data.2e$dvc)
breaks <- c(minimum,0+0.001,maximum)# the +0.0001 is just to make the break levels unique
dividends.data.2e$dvc.bracket <- cut(dividends.data.2e$dvc, breaks=breaks, include.lowest=T)
summary(dividends.data.2e$dvc.bracket)
dividends.data.2e$dvc.bracket <- factor(dividends.data.2e$dvc.bracket, levels = c('[0,0.001]', '(0.001,1.13e+04]'), labels = c(0, 1))

# Full model for logistic regression- categorical Y
m.log.full <- glm(dividends.data.2e$dvc.bracket ~ dividends.data.2e$indfmt 
                  + dividends.data.2e$at + dividends.data.2e$ib + dividends.data.2e$idit
                  + dividends.data.2e$spi + dividends.data.2e$xint+ dividends.data.2e$exchg 
                  + dividends.data.2e$costat + dividends.data.2e$sic , data = dividends.data.2e, family=binomial)
m.log.empty <- glm(dividends.data.2e$dvc.bracket ~ 1, data = dividends.data.2e, family=binomial)
summary(m.log.full)

b.subsets.log <- regsubsets(formula(m.log.full), nbest = 1, nvmax = 9, data = dividends.data.2c)
summary(b.subsets.log)
# Using leaps package to find best subset using BIC, adjr2 and cp
plot(b.subsets.log)
b.subset.log.bic <- which.min(summary(b.subsets.log)$bic)
b.subset.log.bic
## Best subset with 5 variables has the lowest BIC.
summary(b.subsets.log)$bic
coef(b.subsets.log,5)
b.subset.log.adjr2 <- which.max(summary(b.subsets.log)$adjr2)
b.subset.log.adjr2
## Best subset with 8 variables has the highest adjr2.
summary(b.subsets.log)$adjr2
coef(b.subsets.lin,8)
b.subset.log.cp <- which.min(summary(b.subsets.log)$cp)
b.subset.log.cp
## Best subset with 9 variables has the lowest CP
summary(b.subsets.log)$cp
coef(b.subsets.log,8)

# Using stepwise
m.log.step <- step(m.log.empty, direction = "forward", scope = formula(m.log.full), data = dividends.data.2e)
summary(m.log.step) # all 9 variables



# 4 b

# train test set
train <- sample.split(Y = dividends.data.2e$dvc.bracket, SplitRatio = 0.7)
trainset <- subset(dividends.data.2e, train == T)
testset <- subset(dividends.data.2e, train == F)
# model 2 using logistic regression
# now we have discrete for ib, we can finally do logistic regression, do on train set
attach(trainset)
m2 <- glm(dvc.bracket ~ at+ib + idit+ spi+xint, data = trainset, family = binomial)
summary(m2)


# Confusion matrix on Trainset ------------------------------------------

prob.train <- predict(m2, type = 'response')
# factors are [-9.93e+04,0] and (0,2.47e+04]
threshold2 <- sum(trainset$dvc.bracket == "(0.001,1.13e+04]")/length(trainset$dvc.bracket)

predict.train <- ifelse(prob.train > threshold2, "Dividend", "No dividend")

table(trainset$dvc.bracket, predict.train)

# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))
## Caution: Small sample size. Accuracy cannot be trusted.


# Confusion matrix on Testset ------------------------------------------

prob.test <- predict(m2, newdata = testset, type = 'response')


# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, "Dividend", "No Dividend")

table(testset$dvc.bracket, predict.test)

error.test <- round(mean(predict.test != testset$Upgraded), 3)
print(paste('Testset Error Rate =', error.test))

detach(trainset)

# 4 c
# model 3 using decision tree
library(rpart)
library(rpart.plot)			# For Enhanced tree plots via PRP()
options(digits = 5)

attach(dividends.data.2e)
# default cp = 0.01. Set cp = 0 to guarantee no pruning in order to complete phrase 1: Grow tree to max.
# Tree is too big, use minsplit = 100 to reduce tree
# Considering that there are 13252 observations, its not realistic to put in a small min split
m3 <- rpart(dvc.bracket ~ at+ib + idit+ spi+xint, 
            data = dividends.data.2e, method = 'class',control = rpart.control(minsplit = 500, cp = 0))
par(mfrow=c(1,1))
prp(m3)
prp(m3, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5)

# Results of CART as Decision Rules
print(m3)

# Effects of Cost Complexity Pruning at important cp values.
printcp(m3, digits = 3)

# Plot CV error vs cp values
plotcp(m3)
## m1 tree is very small. why?


# 5

# correlation between spi and ib
cor(dividends.data.2e$spi, dividends.data.2e$ib)
cor.test(dividends.data.2e$spi, dividends.data.2e$ib)

# Study relationship between spi and losses
summary(dividends.data.2e)
g <- ggplot(dividends.data.2e, aes(x = dividends.data.2e$spi, y = dividends.data.2e$ib)) 
g + geom_point(shape=1) + geom_smooth(method=lm)

m4 <- lm(dvc ~ at+ib + idit+ spi+xint, data = dividends.data.2e)
library(car)
vif(m4)

