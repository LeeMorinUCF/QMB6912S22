##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specfications
# with the Box-Tidwell Transformation
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# April 7, 2021
#
##################################################
#
# Tractor_Model_Spec gives examples of nonlinear
#   regression models with a number of different
#   model specifications.
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
# Box Tidwell
##################################################

# The Box-Tidwell transformation is in the car package.
library(car)

# Notice that the box.tidwell() function is deprecated.
# The currently available function must be spelled
# boxTidwell() with no dot and a capital T.

#--------------------------------------------------
# First model: transformation of horsepower
#--------------------------------------------------

# Modified from the linear model:
# saleprice ~ horsepower + squared_horsepower +
#   age + enghours +
#   diesel + fwd + manual + johndeere + cab

box_tidwell <- boxTidwell(formula =
                             saleprice ~ horsepower,
                           other.x = ~ age + enghours +
                             diesel + fwd + manual + johndeere + cab,
                           data = tractor_sales)

# The summary method is not available.
# summary(box_tidwell)

print(box_tidwell)
# Note: The "MLE of lambda" is the exponent on horsepower.
# I mistakenly interpreted this as the exponent on saleprice, like Box-Cox,
# but with Box-Tidwell, the exponents are on the explanatory variables
# and are all called lambda.
# The exponent is not significantly different from 1,
# although it is slightly lower.


#--------------------------------------------------
# Second model: transformation of horsepower and age
#--------------------------------------------------


box_tidwell <- boxTidwell(formula =
                            saleprice ~ horsepower + age,
                          other.x = ~ diesel + enghours + fwd + manual + johndeere + cab,
                          data = tractor_sales)

print(box_tidwell)
# The exponent  on horsepower is not significantly different from 1,
# although it is slightly lower.
# The exponent for age is near -1,
# which corresponds to the reciprocal:
# a quick decline in value in the first year of age
# that declines throughout the life of the tractor.


#--------------------------------------------------
# Third model: Repeat with log saleprice
#--------------------------------------------------

# Now try with log saleprice.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])


box_tidwell <- boxTidwell(formula =
                            log_saleprice ~ horsepower + age,
                          other.x = ~ enghours +
                            diesel + fwd + manual + johndeere + cab,
                          data = tractor_sales)

print(box_tidwell)
# With the log saleprice as the dependent variable again,
# we are back to having an exponent on horsepower.
# Age now has a linear effect and no onger needs a transformation.



#--------------------------------------------------
# Fourth model: transformation of engine hours and age
# with horsepower specified with squared horsepower
#--------------------------------------------------

# Now revert back to squared horsepower.

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2

box_tidwell <- boxTidwell(formula =
                            log_saleprice ~ enghours + age,
                          other.x = ~ horsepower + squared_horsepower +
                            diesel + fwd + manual + johndeere + cab,
                          data = tractor_sales)
print(box_tidwell)
# None of the exponents are significant.
# The linear model with squared horsepower was sufficient.




##################################################
# End
##################################################
