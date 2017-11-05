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
nrow(dividends.data.2d) # 34195 rows left
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
nrow(dividends.data.2e) # 20253
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
nrow(dividends.data.2e)#18893

# convert years to categorical
dividends.data.2e$fyear <- factor(dividends.data.2e$fyear)

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
# Is dividend payout affected by losses?

# Choosing the varibles for the models

# 4b
# Model selection for linear regression
library(leaps)
# div as y, find the other Xs
summary(dividends.data.2e)
# consider the following variables for selection:
# fyear, at, ib, idit, spi, xint, exchg, costat, sic

# Full model for linear regression 
m.lin.full <- lm(dividends.data.2e$dvc ~ dividends.data.2e$fyear
                 + dividends.data.2e$at + dividends.data.2e$ib + dividends.data.2e$idit
                 + dividends.data.2e$spi + dividends.data.2e$xint+ dividends.data.2e$exchg 
                 + dividends.data.2e$costat , data = dividends.data.2e)
m.lin.empty <- lm(dividends.data.2e$dvc ~ 1, data = dividends.data.2e)
summary(m.lin.full) # only costat is not significant

# Using leaps package to find best subset using BIC, adjr2 and cp
b.subsets.lin <- regsubsets(formula(m.lin.full), nbest = 1, nvmax = 9, data = dividends.data.2e)
summary(b.subsets.lin)
plot(b.subsets.lin)

b.subset.lin.bic <- which.min(summary(b.subsets.lin)$bic)
b.subset.lin.bic
## Best subset with 7 variables has the lowest BIC.
summary(b.subsets.lin)$bic
coef(b.subsets.lin,7) # fyear2015, at, ib, idit, spi, xint, exchng

b.subset.lin.adjr2 <- which.max(summary(b.subsets.lin)$adjr2)
b.subset.lin.adjr2
## Best subset with 9 variables has the highest adjr2.
summary(b.subsets.lin)$adjr2
coef(b.subsets.lin,9) #fyear2010,2015,2016, at, ib, idit, spi, xint, exchng

b.subset.lin.cp <- which.min(summary(b.subsets.lin)$cp)
b.subset.lin.cp
## Best subset with 9 variables has the lowest CP
summary(b.subsets.lin)$cp
coef(b.subsets.lin,9) #fyear2010,2015,2016, at, ib, idit, spi, xint, exchng

# All give the same 7


# Using stepwise
m.lin.step <- step(m.lin.empty, direction = "forward", scope = formula(m.lin.full), data = dividends.data.2e)
summary(m.lin.step) 
# at, ib, spi,  fyear2015,  xint, exchng, idit => ***
#fyear2010,fyear2016, fyear2017 => .


attach(dividends.data.2e)
set.seed(2710)
library(caTools)

# 70% trainset. Stratify on Y = dvc. Caution: Sample size only 32 in this example.
train <- sample.split(Y = dividends.data.2e$dvc, SplitRatio = 0.7)
trainset <- subset(dividends.data.2e, train == T)
testset <- subset(dividends.data.2e, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$dvc)
summary(testset$dvc)

attach(trainset)

# model 4a using linear regression on our selected model: 7 variables
m.4b <- lm(dvc ~  at + ib + spi  + xint + fyear + idit + exchg, data = trainset)
summary(m.4b)
coef(m.4b) # only the coefficients in the model
formula(m.4b) # Formula used in the model
dividends.data.2e$dvc # Actual Y
fitted(m.4b) # Model.1 Predicted Y
residuals(m.4b) # Errors = Actual - Model Predicted
RMSE.m4b.train <- sqrt(mean(residuals(m.4b)^2))
## 429.1
summary(abs(residuals(m.4b)))
## Trainset: Min Abs Error = 0.0, Max Abs Error = 18361
par(mfrow=c(2,2))  # 4 Charts in a Plot.
plot(m.4b)
## There are outliers
# Apply model from trainset to predict testset.
predict.m4b.test <- predict(m.4b, newdata = testset)
testset.error <- testset$dvc - predict.m4b.test
RMSE.m4b.test <- sqrt(mean(testset.error^2))
## 402.62
summary(abs(testset.error))
# Testset: Min Abs Error = 0.0, Max Abs Error = 13675


# How about only using the MOST significant variables from m.4b?
# model 4a.2 using MOST significant 5 variables from m.4b
m.4b.2 <- lm(dvc ~  at + ib + spi + fyear  + exchg, data = trainset)
summary(m.4b.2)
coef(m.4b.2) # only the coefficients in the model
formula(m.4b.2) # Formula used in the model
trainset$dvc # Actual Y
fitted(m.4b.2) # Model.1 Predicted Y
residuals(m.4b.2) # Errors = Actual - Model Predicted
RMSE.m4b.2.train <- sqrt(mean(residuals(m.4b.2)^2))
## 429.32
summary(abs(residuals(m.4b.2)))
## Trainset: Min Abs Error = 0.0, Max Abs Error = 18354
par(mfrow=c(2,2))  # 4 Charts in a Plot.
plot(m.4b.2)
## There are outliers
# Apply model from trainset to predict testset.
predict.m4b.2.test <- predict(m.4b.2, newdata = testset)
testset.error <- testset$dvc - predict.m4b.2.test
RMSE.m4b.2.test <- sqrt(mean(testset.error^2))
## 402.98
summary(abs(testset.error))
# Testset: Min Abs Error = 0.0, Max Abs Error = 13632.5
# Model with 7 variables and 5 variables perform roughty the same


#4b
# First discretise dvc for logistic regression later (categorical Y)
minimum <- min(dividends.data.2e$dvc)
maximum <- max(dividends.data.2e$dvc)
breaks <- c(minimum,0+0.001,maximum)# the +0.0001 is just to make the break levels unique
dividends.data.2e$dvc.bracket <- cut(dividends.data.2e$dvc, breaks=breaks, include.lowest=T)
summary(dividends.data.2e$dvc.bracket)
dividends.data.2e$dvc.bracket <- factor(dividends.data.2e$dvc.bracket, levels = c('[0,0.001]', '(0.001,1.9e+04]'), labels = c(0, 1))
summary(dividends.data.2e$dvc.bracket)
# MODEL SELECTION FOR LOGISTIC REGRESSION
# Full model for logistic regression- categorical Y
m.log.full <- glm(dividends.data.2e$dvc.bracket ~ dividends.data.2e$fyear
                  + dividends.data.2e$at + dividends.data.2e$ib + dividends.data.2e$idit
                  + dividends.data.2e$spi + dividends.data.2e$xint+ dividends.data.2e$exchg 
                  + dividends.data.2e$costat , data = dividends.data.2e, family=binomial)
m.log.empty <- glm(dividends.data.2e$dvc.bracket ~ 1, data = dividends.data.2e, family=binomial)
summary(m.log.full)

par(mfrow=c(1,1))
b.subsets.log <- regsubsets(formula(m.log.full), nbest = 1, nvmax = 9, data = dividends.data.2c)
summary(b.subsets.log)
# Using leaps package to find best subset using BIC, adjr2 and cp
plot(b.subsets.log)
b.subset.log.bic <- which.min(summary(b.subsets.log)$bic)
b.subset.log.bic
## Best subset with 7 variables has the lowest BIC.
summary(b.subsets.log)$bic
coef(b.subsets.log,7)
#fyear2009,2010, ib, spi, xint, exchng, costat

b.subset.log.adjr2 <- which.max(summary(b.subsets.log)$adjr2)
b.subset.log.adjr2
## Best subset with 9 variables has the highest adjr2.
summary(b.subsets.log)$adjr2
coef(b.subsets.log,9)
#fyear2009,2010,at,ib,idit,spi,xint,exchng, costat


b.subset.log.cp <- which.min(summary(b.subsets.log)$cp)
b.subset.log.cp
## Best subset with 9 variables has the lowest CP
summary(b.subsets.log)$cp
coef(b.subsets.log,9)
#fyear2009,2010,at,ib,idit,spi,xint,exchng, costat


# Using stepwise
m.log.step <- step(m.log.empty, direction = "forward", scope = formula(m.log.full), data = dividends.data.2e)
summary(m.log.step) 
# ***: exchg, costat, ib, spi, xint
# *: idit, fyear 2010, fyear 2009,
# . : at

# 4 c

# train test set
train <- sample.split(Y = dividends.data.2e$dvc.bracket, SplitRatio = 0.7)
trainset <- subset(dividends.data.2e, train == T)
testset <- subset(dividends.data.2e, train == F)
# model 4b using logistic regression
# now we have discrete for dvc, we can finally do logistic regression, do on train set
attach(trainset)
# Contains all significant var from model selection techniques above
m.4c <- glm(dvc.bracket ~ exchg + costat + ib + spi + xint + idit + fyear + at, data = trainset, family = binomial)
summary(m.4c)


# Confusion matrix on Trainset ------------------------------------------

prob.train <- predict(m.4c, type = 'response')
threshold2 <- sum(trainset$dvc.bracket == 1)/length(trainset$dvc.bracket)
predict.train <- ifelse(prob.train > threshold2, 1, 0)

table(trainset$dvc.bracket, predict.train)

# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))


# Confusion matrix on Testset ------------------------------------------
# remove row with year 2017 from testset
testset <- subset(testset, subset=(testset$fyear!="2017"))
prob.test <- predict(m.4c, newdata = testset, type = 'response')
# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, 1, 0)

table(testset$dvc.bracket, predict.test)

error.test <- round(mean(predict.test != testset$dvc.bracket), 3)
print(paste('Testset Error Rate =', error.test))

detach(trainset)

attach(trainset)
# Contains only MOSTsignificant var from model selection techniques above
m.4c.2 <- glm(dvc.bracket ~ exchg + costat + ib + spi + xint , data = trainset, family = binomial)
summary(m.4c.2)


# Confusion matrix on Trainset ------------------------------------------

prob.train <- predict(m.4c.2, type = 'response')
threshold2 <- sum(trainset$dvc.bracket == 1)/length(trainset$dvc.bracket)
predict.train <- ifelse(prob.train > threshold2, 1, 0)

table(trainset$dvc.bracket, predict.train)

# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))


# Confusion matrix on Testset ------------------------------------------
# remove row with year 2017 from testset
testset <- subset(testset, subset=(testset$fyear!="2017"))
prob.test <- predict(m.4c.2, newdata = testset, type = 'response')
# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, 1, 0)

table(testset$dvc.bracket, predict.test)

error.test <- round(mean(predict.test != testset$dvc.bracket), 3)
print(paste('Testset Error Rate =', error.test))

detach(trainset)

# 4 d
# model 3 using decision tree
library(rpart)
library(rpart.plot)			# For Enhanced tree plots via PRP()
options(digits = 5)

attach(dividends.data.2e)
# default cp = 0.01. Set cp = 0 to guarantee no pruning in order to complete phrase 1: Grow tree to max.
# Tree is too big, use minsplit = 100 to reduce tree
# Considering that there are 18893 observations, its not realistic to put in a small min split
m.4d <- rpart(dvc.bracket ~ fyear + at + ib + idit + spi + xint+ exchg + costat,
            data = dividends.data.2e, method = 'class',control = rpart.control(minsplit = 500, cp = 0))
par(mfrow=c(1,1))
prp(m.4d)
prp(m.4d, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5)

# Results of CART as Decision Rules
print(m.4d)

# Effects of Cost Complexity Pruning at important cp values.
printcp(m.4d, digits = 3)

# Plot CV error vs cp values
plotcp(m.4d)

# optimal cp = cp that result in lowest CV error.
cp.opt <- m.4d$cptable[which.min(m.4d$cptable[,"xerror"]),"CP"]

m.4d.2 <- prune(m.4d, cp = cp.opt)
prp(m.4d.2, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5, main="Pruned Classification Tree")
print(m.4d.2)
printcp(m.4d.2)
## Root node error: 0.34616
## m3 trainset error = 0.636 * 0.34616 = 0.2201 = 22%
## m3 test set error = CV error = 0.643 * 0.3461 = 22 = 22%


predicted <- predict(m.4d.2, newdata = dividends.data.2e, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(dividends.data.2e$dvc.bracket, predicted)


# 4e
timeframe1 <- subset(dividends.data.2e, subset=(dividends.data.2e$fyear=="2007"|dividends.data.2e$fyear=="2008"|dividends.data.2e$fyear=="2009"|dividends.data.2e$fyear=="2010"))
timeframe2 <- subset(dividends.data.2e, subset=(dividends.data.2e$fyear=="2011"|dividends.data.2e$fyear=="2012"|dividends.data.2e$fyear=="2013"))
timeframe3 <- subset(dividends.data.2e, subset=(dividends.data.2e$fyear=="2014"|dividends.data.2e$fyear=="2015"|dividends.data.2e$fyear=="2016"|dividends.data.2e$fyear=="2017"))
# train test set for timeframe1
train1 <- sample.split(Y = timeframe1$dvc.bracket, SplitRatio = 0.7)
trainset1 <- subset(timeframe1, train1 == T)
testset1 <- subset(timeframe1, train1 == F)
# train test set for timeframe2
train2 <- sample.split(Y = timeframe2$dvc.bracket, SplitRatio = 0.7)
trainset2 <- subset(timeframe2, train2 == T)
testset2 <- subset(timeframe2, train2 == F)
# train test set for timeframe3
train3 <- sample.split(Y = timeframe3$dvc.bracket, SplitRatio = 0.7)
trainset3 <- subset(timeframe3, train3 == T)
testset3 <- subset(timeframe3, train3 == F)

# timeframe1 4b
# model 4a.2 using MOST significant 5 variables from m.4b
m.4b.2.1 <- lm(dvc ~  at + ib + spi + fyear  + exchg, data = trainset1)
summary(m.4b.2.1)
coef(m.4b.2.1) # only the coefficients in the model
formula(m.4b.2.1) # Formula used in the model
trainset1$dvc # Actual Y
fitted(m.4b.2.1) # Model.1 Predicted Y
residuals(m.4b.2.1) # Errors = Actual - Model Predicted
RMSE.m4b.2.1.train <- sqrt(mean(residuals(m.4b.2.1)^2))## 273.83
summary(abs(residuals(m.4b.2.1)))
## Trainset: Min Abs Error = 0.0, Max Abs Error = 4749.9
# Apply model from trainset to predict testset.
predict.m4b.2.1.test <- predict(m.4b.2.1, newdata = testset1)
which(is.na(predict.m4b.2.1.test))
length(predict.m4b.2.1.test)
testset.error <- testset1$dvc - predict.m4b.2.1.test
RMSE.m4a.2.1.test <- sqrt(mean(testset.error^2))## 504.65
summary(abs(testset.error))
# Testset: Min Abs Error = 0.0, Max Abs Error = 18719

# timeframe2 4b
# model 4a.2 using MOST significant 5 variables from m.4b
m.4b.2.2 <- lm(dvc ~  at + ib + spi + fyear  + exchg, data = trainset2)
summary(m.4b.2.2)
coef(m.4b.2.2) # only the coefficients in the model
formula(m.4b.2.2) # Formula used in the model
trainset2$dvc # Actual Y
fitted(m.4b.2.2) # Model.1 Predicted Y
residuals(m.4b.2.2) # Errors = Actual - Model Predicted
RMSE.m4b.2.2.train <- sqrt(mean(residuals(m.4b.2.2)^2))## 335.61
summary(abs(residuals(m.4b.2.2)))
## Trainset: Min Abs Error = 0.0, Max Abs Error = 6431.7
# Apply model from trainset to predict testset.
predict.m4b.2.2.test <- predict(m.4b.2.2, newdata = testset2)
testset.error <- testset2$dvc - predict.m4b.2.2.test
RMSE.m4b.2.2.test <- sqrt(mean(testset.error^2))## 366.06
summary(abs(testset.error))
# Testset: Min Abs Error = 0.0, Max Abs Error = 7795.7

# timeframe3 4b
# model 4a.2 using MOST significant 5 variables from m.4b
m.4b.2.3 <- lm(dvc ~  at + ib + spi + fyear  + exchg, data = trainset3)
summary(m.4b.2.3)
coef(m.4b.2.3) # only the coefficients in the model
formula(m.4b.2.3) # Formula used in the model
trainset3$dvc # Actual Y
fitted(m.4b.2.3) # Model.1 Predicted Y
residuals(m.4b.2.3) # Errors = Actual - Model Predicted
RMSE.m4b.2.3.train <- sqrt(mean(residuals(m.4b.2.3)^2))## 505.44
summary(abs(residuals(m.4b.2.3)))
## Trainset: Min Abs Error = 0.0, Max Abs Error = 13353
# Apply model from trainset to predict testset.
# remove row with year 2017 from testset
testset3 <- subset(testset3, subset=(testset3$fyear!="2017"))
predict.m4b.2.3.test <- predict(m.4b.2.3, newdata = testset3)
testset.error <- testset3$dvc - predict.m4b.2.3.test
RMSE.m4b.2.3.test <- sqrt(mean(testset.error^2))## 636
summary(abs(testset.error))
# Testset: Min Abs Error = 0.1, Max Abs Error = 14546.9

# timeframe1 4c
# Contains only MOSTsignificant var from model selection techniques above
m.4c.2.1 <- glm(dvc.bracket ~ exchg + costat + ib + spi + xint , data = trainset1, family = binomial)
summary(m.4c.2.1)
# Confusion matrix on Trainset ------------------------------------------
prob.train <- predict(m.4c.2.1, type = 'response')
threshold2 <- sum(trainset1$dvc.bracket == 1)/length(trainset1$dvc.bracket)
predict.train <- ifelse(prob.train > threshold2, 1, 0)
table(trainset1$dvc.bracket, predict.train)
# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset1$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))
# Confusion matrix on Testset ------------------------------------------
prob.test <- predict(m.4c.2.1, newdata = testset1, type = 'response')
# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, 1, 0)
table(testset1$dvc.bracket, predict.test)
error.test <- round(mean(predict.test != testset1$dvc.bracket), 3)
print(paste('Testset Error Rate =', error.test))

# timeframe2 4
# Contains only MOSTsignificant var from model selection techniques above
m.4c.2.2 <- glm(dvc.bracket ~ exchg + costat + ib + spi + xint , data = trainset2, family = binomial)
summary(m.4c.2.2)
# Confusion matrix on Trainset ------------------------------------------
prob.train <- predict(m.4c.2.2, type = 'response')
threshold2 <- sum(trainset2$dvc.bracket == 1)/length(trainset2$dvc.bracket)
predict.train <- ifelse(prob.train > threshold2, 1, 0)
table(trainset2$dvc.bracket, predict.train)
# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset2$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))
# Confusion matrix on Testset ------------------------------------------
prob.test <- predict(m.4c.2.2, newdata = testset2, type = 'response')
# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, 1, 0)
table(testset2$dvc.bracket, predict.test)
error.test <- round(mean(predict.test != testset2$dvc.bracket), 3)
print(paste('Testset Error Rate =', error.test))

# timeframe3 4c
# Contains only MOSTsignificant var from model selection techniques above
m.4c.2.3 <- glm(dvc.bracket ~ exchg + costat + ib + spi + xint , data = trainset3, family = binomial)
summary(m.4c.2.3)
# Confusion matrix on Trainset ------------------------------------------
prob.train <- predict(m.4c.2.3, type = 'response')
threshold2 <- sum(trainset3$dvc.bracket == 1)/length(trainset3$dvc.bracket)
predict.train <- ifelse(prob.train > threshold2, 1, 0)
table(trainset3$dvc.bracket, predict.train)
# Error Rate, assuming equal weightage to FP & FN.
error.train <- round(mean(predict.train != trainset3$dvc.bracket),3)
print(paste('Trainset Error Rate =', error.train))
# Confusion matrix on Testset ------------------------------------------
# remove row with year 2017 from testset
testset3 <- subset(testset3, subset=(testset3$fyear!="2017"))
prob.test <- predict(m.4c.2.3, newdata = testset3, type = 'response')
# Use same threshold as in trainset
predict.test <- ifelse(prob.test > threshold2, 1, 0)
table(testset3$dvc.bracket, predict.test)
error.test <- round(mean(predict.test != testset3$dvc.bracket), 3)
print(paste('Testset Error Rate =', error.test))

# timeframe1 4d
# default cp = 0.01. Set cp = 0 to guarantee no pruning in order to complete phrase 1: Grow tree to max.
# Tree is too big, use minsplit = 100 to reduce tree
# Considering that there are 18893 observations, its not realistic to put in a small min split
attach(timeframe1)
m.4d.1 <- rpart(dvc.bracket ~ fyear + at + ib + idit + spi + xint+ exchg,
              data = timeframe1, method = 'class',control = rpart.control(minsplit = 50, cp = 0)) #reduced minsplit for bigger tree
par(mfrow=c(1,1))
#prp(m.4d.1)
#prp(m.4d.1, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5)
# Results of CART as Decision Rules
print(m.4d.1)
# Effects of Cost Complexity Pruning at important cp values.
printcp(m.4d.1, digits = 3)
# Plot CV error vs cp values
plotcp(m.4d.1)
# optimal cp = cp that result in lowest CV error.
cp.opt <- m.4d.1$cptable[which.min(m.4d.1$cptable[,"xerror"]),"CP"]
m.4d.1.pruned <- prune(m.4d.1, cp = cp.opt)
#prp(m.4d.2.1, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5, main="Pruned Classification Tree")
print(m.4d.1.pruned)
printcp(m.4d.1.pruned)
## Root node error: 0.34616
## m3 trainset error = 0.636 * 0.34616 = 0.2201 = 22%
## m3 test set error = CV error = 0.643 * 0.3461 = 22 = 22%
predicted <- predict(m.4d.1.pruned, newdata = timeframe1, type='class')
# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(timeframe1$dvc.bracket, predicted)
detach(timeframe1)

# timeframe2 4d
# Considering that there are 18893 observations, its not realistic to put in a small min split
attach(timeframe2)
m.4d.2 <- rpart(dvc.bracket ~ fyear + at + ib + idit + spi + xint+ exchg,
                data = timeframe2, method = 'class',control = rpart.control(minsplit = 50, cp = 0)) #reduced minsplit for bigger tree
par(mfrow=c(1,1))
#prp(m.4d.2)
#prp(m.4d.2 ;/, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5)
# Results of CART as Decision Rules
print(m.4d.2)
# Effects of Cost Complexity Pruning at important cp values.
printcp(m.4d.2, digits = 3)
# Plot CV error vs cp values
plotcp(m.4d.2)
# optimal cp = cp that result in lowest CV error.
cp.opt <- m.4d.2$cptable[which.min(m.4d.2$cptable[,"xerror"]),"CP"]
m.4d.2.pruned <- prune(m.4d.2, cp = cp.opt)
prp(m.4d.2.pruned, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5, main="Pruned Classification Tree")
print(m.4d.2.pruned)
printcp(m.4d.2.pruned)
## Root node error: 0.34616
## m3 trainset error = 0.636 * 0.34616 = 0.2201 = 22%
## m3 test set error = CV error = 0.643 * 0.3461 = 22 = 22%
predicted <- predict(m.4d.2.pruned, newdata = timeframe2, type='class')
# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(timeframe2$dvc.bracket, predicted)
detach(timeframe2)

# timeframe3 4d
# Considering that there are 18893 observations, its not realistic to put in a small min split
attach(timeframe3)
m.4d.3 <- rpart(dvc.bracket ~ fyear + at + ib + idit + spi + xint+ exchg,
                data = timeframe3, method = 'class',control = rpart.control(minsplit = 70, cp = 0)) #reduced minsplit for bigger tree
par(mfrow=c(1,1))
#prp(m.4d.3)
#prp(m.4d.3, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5)
# Results of CART as Decision Rules
print(m.4d.3)
# Effects of Cost Complexity Pruning at important cp values.
printcp(m.4d.3, digits = 3)
# Plot CV error vs cp values
plotcp(m.4d.3)
# optimal cp = cp that result in lowest CV error.
cp.opt <- m.4d.3$cptable[which.min(m.4d.3$cptable[,"xerror"]),"CP"]
m.4d.3.pruned <- prune(m.4d.3, cp = cp.opt)
prp(m.4d.3.pruned, type=2, extra=104, nn=T, nn.box.col = 'light blue',digits=5, main="Pruned Classification Tree")
print(m.4d.3.pruned)
printcp(m.4d.3.pruned)
## Root node error: 0.34616
## m3 trainset error = 0.636 * 0.34616 = 0.2201 = 22%
## m3 test set error = CV error = 0.643 * 0.3461 = 22 = 22%
predicted <- predict(m.4d.3.pruned, newdata = timeframe3, type='class')
# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(timeframe3$dvc.bracket, predicted)
detach(timeframe3)

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

lossesdata <- subset(dividends.data.2e, subset=(dividends.data.2e$ib<0))
g <- ggplot(lossesdata, aes(x = lossesdata$spi, y = lossesdata$ib)) 
g + geom_point(shape=1) + geom_smooth(method=lm)

