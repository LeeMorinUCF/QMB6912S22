##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specfication
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# March 23, 2021
#
##################################################
#
# Tractor_Model_Spec gives examples of OLS regression models
#   by considering a number of different model specifications.
# In this example, the model specification choices
#   have a parametric form.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)


##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
setwd("C:/Users/le279259/Documents/Teaching/QMB6911_Summer_2021/SoftwareDemo/QMB6911S21/Nonparametrics")

getwd()


##################################################
# Loading the Data
##################################################

tractor_sales <- read.csv('TRACTOR7.csv')

# Inspect the contents.
summary(tractor_sales)
# Make sure there are no problems with the data.


##################################################
# Generating New Variables
##################################################


hist(tractor_sales[, 'saleprice'])
# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Now plot the histogram for log of saleprice:
hist(tractor_sales[, 'log_saleprice'])
# Much better behaved. Looks almost normal.



##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = tractor_sales,
                 formula = saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_1)


##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_2)


summary(tractor_sales[, 'saleprice'])

# See what a difference the John Deere label is worth:
summary(tractor_sales[tractor_sales[, 'johndeere'] == 1, 'saleprice'])
summary(tractor_sales[tractor_sales[, 'johndeere'] == 0, 'saleprice'])


##################################################
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere)

# Output the results to screen.
summary(lm_model_3)


##################################################
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Omit seasonal indicators and transmission type
##################################################

# Estimate a regression model.
lm_model_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
summary(lm_model_4)


##################################################
#
# Exercise:
#
# Consider a polynomial functional form for horsepower.
# Idea: Horsepower improves performance up to a limit,
# then extra power does not add value, only consumes more fuel.
#
# 1. Generate the squared variable.
# 2. Hypothesize the signs.
# 3. Add the squared horsepower term to the regression equation.
# 4. Estimate the revised model.
# 5. Analyze the resulting estimates.
# 6. Make recommendation for the new model.
#
##################################################

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


# Estimate a regression model.
lm_model_5 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
summary(lm_model_5)




##################################################
# Reconsider other variables dropped before
# Using this new functional form for horsepower
##################################################


# Estimate the regression model.
lm_model_6 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_6)


##################################################
# Estimating a Regression Model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_7)

##################################################
#
# Exercise: Test exclusion of seasonal indicators
#   An example of joint hypothesis testing.
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(lm_model_6$residuals^2)
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_7$residuals^2)
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)
num_vars <- 12

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 3

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print(F_stat)

# This value is less than 1, let alone the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.

##################################################
# Estimating a Regression Model
# Model 8: Linear model for log of dollar sale price
# Interact Slope Indicator for Diesel with Engine Hours
##################################################

# Estimate a regression model.
lm_model_8 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours + diesel*enghours + # Note the added term.
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_8)

# Does an additional hour of use affect a diesel-powered tractor
# differently than a gasoline-powered tractor?

# No improvement in R-bar-squared.
# Slope coefficient not significant.
# Diesel and engine coefficients same sign but no longer significant.
# Multicollinearity possibly increases standard errors.

# Conclude that used tractor prices do not change with use
# differently for the type of fuel.


##################################################
# End
##################################################