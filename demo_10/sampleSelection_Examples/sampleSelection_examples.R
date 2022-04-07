##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Sample Selection Models
# with the sampleSelection package.
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# April 7, 2022
#
##################################################
#
# sampleSelection_examples.R gives examples of
#   regression models that adjust for sample selection.
#
# Dependencies:
#   sampleSelection library to estimate models
#     with sample selection.
#   mvtnorm library to generate multivariate
#     normal random variables.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_10/sampleSelection_Examples'
# setwd(wd_path)


##################################################
# Load libraries
##################################################

# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)

# library mvtnorm to generate multivariate normal
# random variables.
library(mvtnorm)


##################################################
# Example 1: Type-2 Tobit Model
##################################################

# First, we estimate a correctly specified
# Tobit-2 model with exclusion restriction.



#--------------------------------------------------
# Generate data
#--------------------------------------------------

# Setting the seed fixes the sequence of random numbers.
set.seed(0)

# Generate matrix of random error terms.
eps <- rmvnorm(500, c(0, 0),
               matrix(c( 1.0, -0.7,
                         -0.7,  1.0), 2, 2))

# Generate selection variable (unobserved).
xs <- runif(500)

# Generate logical selection variable in the selection equation.
ys <- xs + eps[, 1] > 0
# Note that true slope coefficient is one
# and the true intercept coefficient is zero.


# Generate explanatory variable in the outcome equation (observed).
xo <- runif(500)

# Generate dependent variable (partially observed).
yoX <- xo + eps[, 2]
# Note again that true slope coefficient is one
# and the true intercept coefficient is zero.

# Generate dependent variable in the outcome equation (observed).
yo <- yoX * (ys > 0)


# Collect these into a data frame.
toit_2_ex1 <- data.frame(yo = yo,
                         yoX = yoX,
                         xo = xo,
                         ys = ys,
                         xs = xs,
                         eps1 = eps[, 1],
                         eps2 = eps[, 2])

# Note the observations set to zero.
summary(toit_2_ex1)

# Inspect only the observed events.
summary(toit_2_ex1[toit_2_ex1[, 'ys'], ])


summary(yo)
sum(yo == 0)


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
#--------------------------------------------------

tobit_2_sel_1 <- selection(selection = ys ~ xs,
                           outcome = yo ~ xo,
                           data = toit_2_ex1)

summary(tobit_2_sel_1)
# Notice the slope coefficients are close to 1 (the true values).

# Overall, the true values are within the
# 95% confidence intervals of the corresponding estimates.



#--------------------------------------------------
# Compare with Model that Ignores Sample Selection
#--------------------------------------------------


# Estimate the feasible linear model
# with some rows observed to be zero.
tobit_2_lm_1_full <- lm(formula = yo ~ xo,
                   data = toit_2_ex1)

summary(tobit_2_lm_1_full)
# Notice the coefficients differ from the true values.
# The slope is biased toward zero.


# Although it is infeasible, as not all data are observed
# If we had observed the full set of observations,
# the linear model would be correctly specified.

tobit_2_lm_1_inf <- lm(formula = yoX ~ xo,
                       data = toit_2_ex1)

summary(tobit_2_lm_1_inf)

# Although infeasible, the intercept and slope coefficients
# are close to zero and one (the true values).



# We can, however, estimate the model
# with only the variables observed.
# It drops all the information about the observations
# that were not selected.
tobit_2_lm_1_sel <- lm(formula = yoX ~ xo,
                       data = toit_2_ex1[toit_2_ex1[, 'ys'], ])

summary(tobit_2_lm_1_sel)

# In this case, the intercept and slope coefficients
# are close to zero and one (the true values).


# Since this drops observations with partial information,
# however, these estimates are more variable.
