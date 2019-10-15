nhanes = read.csv("/Users/jaimegarcia/Documents/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/DealingWithMissingData/nhanes.csv")
nhanes
str(nhanes)
summary(nhanes)

# Install and load mice package
#install.packages("mice")
library(mice)
#install.packages("VIM")
#answer no to question about installing from source / required compilation
library(VIM)
#install.packages("lattice")
library(lattice)
library(mice)
#To see the pattern of the missing data
md.pattern(nhanes)

#Gender Age Height Weight Leg Waist Thigh   
#92      1   1      1      1   1     1     1  0
##1      1   1      1      0   1     1     1  1
##1      1   1      1      1   1     0     1  1
##1      1   1      1      1   0     1     0  2
##1      1   1      1      1   1     0     0  2
##4      1   1      1      1   0     0     0  3
##       0   0      0      1   5     6     6 18

#This tells us we have all data for 92 observations
# For 1 observation we are missing leg, for 1 thigh, etc
# Last row summarizes number of missing observations, also found in summary(nhanes) by looking at the NAs
summary(nhanes)

#Visualizing missing variables thanks to VIM package
#Graph on left ranks variables in decreasing order of percentage of missingness
#Graph on the right shows missingness patterns. For 92% of observervations we are missing nothing.
#For the next 4% we are missing the first 3 variables
#For the next 1% we are missing only the first 1 etc
nhanes_aggr = aggr(nhanes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#This shows the margin plots with the available data in blue and the missing data in red, for variables "Waist" and "Thigh"
# Blue box plots summarize the distribution of observed data given the other variable is observed, 
# Red box plots summarize the distribution of observed data given the other variable is missing.
#For instance from box plots on horizontal axis, we learn "Thigh" values tend to be missing for lower "Waist" value
marginplot(nhanes[, c("Waist", "Thigh")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#Linear regression where missing observations are deleted
fit.cc = lm(Weight ~ Waist + Thigh + Leg + Gender + Age + Height, data=nhanes)
summary(fit.cc)

fit.cc = lm(Weight ~ Waist + Thigh + Leg + Gender + Height, data=nhanes)
summary(fit.cc)

# Function mice() in mice package is a Markov Chain Monte Carlo (MCMC) method 
# that imputes missing values for each incomplete 
# variable m times by regression of incomplete variables on the other variables iteratively
# using the correlation structure of the data.
imp = mice(nhanes, m=5, printFlag=FALSE, maxit = 40, seed=2525)
# The output imp contains m=5 completed datasets. Each dataset can be analysed
# using function with(), and including an expression for the statistical analysis approach
# we want to apply for each imputed dataset as follows
fit.mi = with(data=imp, exp = lm(Weight ~ Waist + Thigh + Leg + Gender + Height))
summary(fit.mi)
# Next, we combine all the results of the 5 imputed datasets using the pool() function
combFit = pool(fit.mi) 
# Printing out summary coefficients with all numbers having only 4 decimals for greater readability
round(summary(combFit),4)

# increasing the number of data sets we create with imputed data from m=5 to m=20
# to see if it changes the results
imp20 = mice(nhanes, m=20, printFlag=FALSE, maxit = 30, seed=2525)
fit.mi20 = with(data=imp20, exp = lm(Weight ~ Waist + Thigh + Leg + Gender + Height))
combFit20 = pool(fit.mi20)
round(summary(combFit20),4)

# You can also see the pooled adjusted R-squared as
pool.r.squared(fit.mi)
pool.r.squared(fit.mi20)

# We fill our initial data set with the imputed values, for instance with results for m=5
imp_2 = complete(imp, 2)

# We can inspect the distributions of the original and the imputed data:
## scatterplot (of Waist and Thigh) for each imputed dataset
xyplot(imp, Waist ~ Thigh | .imp, pch = 20, cex = 1.4)

# How did mice impute values?
imp$meth
# pmm = predictive mean matching, default method of mice() for imputation of continous incomplete variables
# For each missing value, pmm finds a set of observed values with the closest predicted mean as the missing one 
# and imputes the missing values by a random draw from that set.
