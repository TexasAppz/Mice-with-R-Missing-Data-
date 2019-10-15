# Learning about mice package

# Read in data
polling = read.csv("/Users/jaimegarcia/Documents/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/DealingWithMissingData/PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

# Install and load mice package
#install.packages("mice")
library(mice)

# Multiple imputation
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)

# new code
md.pattern(simple)
simple_aggr = aggr(simple, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(simple), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#This shows the margin plots with the available data in blue and the missing data in red, for variables "Waist" and "Thigh"
# Blue box plots summarize the distribution of observed data given the other variable is observed, 
# Red box plots summarize the distribution of observed data given the other variable is missing.
#For instance from box plots on horizontal axis, we learn "Rasmussen" values tend to be missing for higher "SurveyUSA" value
marginplot(simple[, c("SurveyUSA", "Rasmussen")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

imp = mice(simple, m=10, printFlag=FALSE, maxit = 40, seed=2525)

# Visualizing what values were added to the missing fields in SurveyUSA and Rasmussen
xyplot(imp, SurveyUSA ~ Rasmussen | .imp, pch = 20, cex = 1.4)

imputed = complete(imp)
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Subset data into training set and test set
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Smart Baseline
table(Train$Republican)
sign(20)
sign(-10)
sign(0)

table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen))

# Multicollinearity
cor(Train)
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# Logistic Regression Model
mod0 = glm(Republican~Rasmussen+SurveyUSA+PropR+DiffCount, data = Train, family="binomial")
summary(mod0)
mod0 = glm(Republican~Rasmussen+PropR+DiffCount, data = Train, family="binomial")
summary(mod0)
mod0 = glm(Republican~0+Rasmussen+PropR+DiffCount, data = Train, family="binomial")
summary(mod0)
mod0 = glm(Republican~0+Rasmussen+DiffCount, data = Train, family="binomial")
summary(mod0)

mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)

# Training set predictions
pred0 = predict(mod0, type="response")
table(Train$Republican, pred0 >= 0.5)

pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)

# Two-variable model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5)
summary(mod2)

# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen))

# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)

# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)

