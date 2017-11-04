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
g + geom_point(shape=1) + geom_smooth(method=lm) + labs(x = "Dividends Common") + labs(y = "Income before Extraordinary Items")
# Findings: Before cleaning there is possibly some relationship between dvc and ib

# Data exploration 2 : Is the data sufficient?
summary(dividends.data)
# There are NA values in dvc and ib ( 26803 and 25399 respectively )
# Check custID for the 56 NAs. Same?
dvc_na <- subset(dividends.data, is.na(dividends.data$dvc))
ib_na <- subset(dividends.data, is.na(dividends.data$ib))
install.packages('sqldf')
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
                            subset=((dividends.data.2b$sic > 6999 | dividends.data.2b$sic < 6000 )|
                                      (dividends.data.2b$sic > 4999 | dividends.data.2b$sic < 4900) ))
nrow(dividends.data.2c) # 30001 rows
# Check again
summary(dividends.data.2c)
g <- ggplot(dividends.data.2c, aes(x = dividends.data.2c$sic, y = dividends.data.2c$ib)) 
g + geom_point(shape=1)

# 2d

# Remove observations that do not report income

dividends.data.2d <- subset(dividends.data.2c, !is.na(dividends.data.2c$ib))
nrow(dividends.data.2d) # 22159 rows left
summary(dividends.data.2d) # no more NA values!

# 2e

# What other data cleaning steps should be executed that are obvious and necessary?
summary(dividends.data.2d)
# There are still na values in dvc, negative dvc
dividends.data.2e <- subset(dividends.data.2d, !is.na(dividends.data.2d$dvc) & !(dividends.data.2d$dvc < 0))
dividends.data.2e <- subset(dividends.data.2e, !is.na(dividends.data.2e$xint))
summary(dividends.data.2e$ib) # no need for ib
dividends.data.2e <- subset(dividends.data.2e, !is.na(dividends.data.2e$spi))
dividends.data.2e <- subset(dividends.data.2e, !is.na(dividends.data.2e$idit))
dividends.data.2e <- subset(dividends.data.2e, !is.na(dividends.data.2e$costat))
nrow(dividends.data.2e) # 13252
summary(dividends.data.2e) # no more NA values for dvc and no more negative dvc!

# 3
# Generate summary table
df <- data.frame("FiscalYear"=as.character(),
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
  names(df)<-c(year,
               length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year])),
               length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$ib < 0])),
               length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$div > 0])),
               length(unique(dividends.data.2e$gvkey[dividends.data.2e$fyear == year & dividends.data.2e$prskc < 0]))
  )
  df <- rbind(df)
}


# 4
# Choosing the varibles for the models
library(leaps)
# div as y, find the other Xs

m.full <- lm(dividends.data.2e$dvc ~ dividends.data.2e$at + dividends.data.2e$ib + 
               dividends.data.2e$idit + dividends.data.2e$prstkc + dividends.data.2e$spi+
               dividends.data.2e$xint + dividends.data.2e$costat, data = dividends.data.2e)
b.subsets <- regsubsets(formula(m.full), nbest = 1, nvmax = 7, data = dividends.data.2c)
summary(b.subsets)


b.subset.bic <- which.min(summary(b.subsets)$bic)
b.subset.bic
## Best subset with 5 variables has the lowest BIC.
summary(b.subsets)$bic
coef(b.subsets,5)

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
summary(trainset$ib)
summary(testset$ib)

attach(trainset)

# model 1 using linear regression
m1 <- lm(dvc ~ at+ib + idit+ spi+xint, data = trainset)
summary(m1)

coef(m1) # only the coefficients in the model
formula(m1) # Formula used in the model

dividends.data.2e$dvc # Actual Y
fitted(m1) # Model.1 Predicted Y
residuals(m1) # Errors = Actual - Model Predicted

RMSE.m1.train <- sqrt(mean(residuals(m1)^2))
## 314.213

summary(abs(residuals(m1)))
## Trainset: Min Abs Error = 0, Max Abs Error = 101156


par(mfrow=c(2,2))  # 4 Charts in a Plot.
plot(m1)
## There are outliers

# Apply model from trainset to predict testset.
predict.m1.test <- predict(m1, newdata = testset)
testset.error <- testset$dvc - predict.m1.test
RMSE.m1.test <- sqrt(mean(testset.error^2))
## 415.568
summary(abs(testset.error))
# Testset: Min Abs Error = 0.1, Max Abs Error = 12945.9.


# 4 b
# First discretise dvc
minimum <- min(dividends.data.2e$dvc)
maximum <- max(dividends.data.2e$dvc)
breaks <- c(minimum,0+0.001,maximum)# the +0.0001 is just to make the break levels unique
dividends.data.2e$dvc.bracket <- cut(dividends.data.2e$dvc, breaks=breaks, include.lowest=T)
summary(dividends.data.2e$dvc.bracket)
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

