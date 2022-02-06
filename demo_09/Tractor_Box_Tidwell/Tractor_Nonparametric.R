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
#   have a nonparametric form.
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


hist(tractor_sales[, 'log_saleprice'], breaks = 50)

hist(tractor_sales[, 'log_saleprice'], breaks = 5)

##################################################
# Univariate kernel estimation
##################################################

# You have used nonparametric methods to plot a density

price_density <- density(tractor_sales[, 'saleprice'])
plot(price_density)

# In the default, the bandwidth is chosen using an algorithm.
# See the help for density.
attributes(price_density)
price_density$bw

# But you can choose it as a tuning parameter.
price_density <- density(tractor_sales[, 'saleprice'],
                         bw = 10000)
plot(price_density)
# A bigger bandwidthe gives you a smooth density,
# but might smooth over the details.


# A smaller bandwidth might make the density too noisy.
price_density <- density(tractor_sales[, 'saleprice'],
                         bw = 1000)
plot(price_density)
# There are many jagged changes that have more to do
# with the particular values observed
# than with the population density.

# We can do something similar to predict one variable
# with the others. First, we will transform the variables.


##################################################
# Demean the data
##################################################

# The first-order term is the mean.
# Take residuals from this benchmark model.
tractor_sales[, 'log_saleprice_dev'] <-
  tractor_sales[, 'log_saleprice'] - mean(tractor_sales[, 'log_saleprice'])
plot(density(tractor_sales[, 'log_saleprice_dev']))


# Now build nonparametric models to predict these deviations.


##################################################
# Noparametric model specifications
# Bivariate kernel estimation
##################################################

#--------------------------------------------------
# Nonparametric model for horsepower
#--------------------------------------------------


# Plot a scattergraph so we can see what we're doing.
plot(tractor_sales[, 'horsepower'],
     tractor_sales[, 'log_saleprice_dev'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Horsepower',
     ylab = 'Deviation for Log Tractor Prices',
     col = 'blue')


# Now calculate a curve for the prediction from tractor prices.
# np_hp_fit_1 <- lowess(tractor_sales[, 'horsepower'],
#                       tractor_sales[, 'log_saleprice_dev'])
# This function allows you to predict (useful in a model later).
np_hp_fit_1 <- loess(log_saleprice_dev ~ horsepower, tractor_sales)

# Add a plot of this curve to the scattergraph.
# lines(np_hp_fit_1, col = 'red', lwd = 3)
tractor_sales[, 'horsepower_np'] <- np_hp_fit_1$fitted
lines(tractor_sales[order(tractor_sales[, 'horsepower']), c('horsepower', 'horsepower_np')],
      col = 'red', lwd = 3)
# More difficult to plot loess than lowess but it will be worth it later.

# As above, you can fine-tune the parameters.
# See the help for lowess.
# The smoother span (bandwidth parameter) is 0.75.
# In lowess, f is "the proportion of points in the plot
# which influence the smooth at each value.
# Larger values give more smoothness."

np_hp_fit_2 <- loess(log_saleprice_dev ~ horsepower, tractor_sales, span = 2.0)

# Add a plot of this curve to the scattergraph.
tractor_sales[, 'horsepower_np'] <- np_hp_fit_2$fitted
lines(tractor_sales[order(tractor_sales[, 'horsepower']), c('horsepower', 'horsepower_np')],
      col = 'green', lwd = 3)
# You can see some flattening with this
# more flexible estimator.


# Try again with less smooting.
np_hp_fit_3 <- loess(log_saleprice_dev ~ horsepower, tractor_sales, span = 0.1)


# Add a plot of this curve to the scattergraph.
tractor_sales[, 'horsepower_np'] <- np_hp_fit_3$fitted
lines(tractor_sales[order(tractor_sales[, 'horsepower']), c('horsepower', 'horsepower_np')],
      col = 'magenta', lwd = 3)
# Much more rough but you capture the decline
# in value for tractors with high horsepower.


# Ultimately, you would choose one that captures what
# is happening and don't need to show all of the curves
# that you fit during your investigation.

# In this case, we will keep the first fit.
tractor_sales[, 'horsepower_np'] <- np_hp_fit_1$fitted


# Try this again on other continuous variables.

#--------------------------------------------------
# Nonparametric model for age
#--------------------------------------------------

plot(tractor_sales[, 'age'],
     tractor_sales[, 'log_saleprice_dev'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Age (years)',
     ylab = 'Deviation for Log Tractor Prices',
     col = 'blue')

# Now calculate a curve for the prediction from tractor prices.
np_age_fit_1 <- loess(log_saleprice_dev ~ age, tractor_sales)

# Add a plot of this curve to the scattergraph.
tractor_sales[, 'age_np'] <- np_age_fit_1$fitted
lines(tractor_sales[order(tractor_sales[, 'age']), c('age', 'age_np')],
      col = 'red', lwd = 3)

# This doesn't look very different from linear.

# Try it with the remaining continuous variable.

#--------------------------------------------------
# Nonparametric model for engine hours
#--------------------------------------------------

plot(tractor_sales[, 'enghours'],
     tractor_sales[, 'log_saleprice_dev'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Engine Hours',
     ylab = 'Deviation for Log Tractor Prices',
     col = 'blue')


# Now calculate a curve for the prediction from tractor prices.
np_hrs_fit_1 <- loess(log_saleprice_dev ~ enghours, tractor_sales)

# Add a plot of this curve to the scattergraph.
tractor_sales[, 'enghours_np'] <- np_hrs_fit_1$fitted
lines(tractor_sales[order(tractor_sales[, 'enghours']), c('enghours', 'enghours_np')],
      col = 'red', lwd = 3)


# Looks as though linear might also be close enough
# (except for the outliers in the tail.)



##################################################
# Revisit our best parametric model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
# Omit seasonal indicators
##################################################

tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


# Estimate a regression model.
lm_model_7 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_7)



##################################################
# Compare this with our semi-parametric model
# with a nonparametric fit on horsepower
# Semiparametric model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_np_model_1 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower_np +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_np_model_1)

# The fit is slightly better but the model is very similar.



##################################################
# Fit categorical variables first
##################################################

# Before, we used only the mean as our benchmark model.
# Now lets build a nonparametric model on the residuals from
# a regression on the other variables.


# Estimate a regression model,
# with everything but the horsepower variable.
lm_model_9 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ # horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_9)

# Now let's store the residuals.
tractor_sales[, 'log_saleprice_resid'] <- lm_model_9$residuals
# Now fit a nonparametric model on this.


# Estimate a regression model,
# with everything but the horsepower variable.
lm_model_10 <- lm(data = tractor_sales,
                 formula = horsepower ~ # horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_10)

# Now let's store the residuals.
tractor_sales[, 'horsepower_resid'] <- lm_model_10$residuals
# Now fit a nonparametric model on this.



# Plot a scattergraph to start.
plot(tractor_sales[, 'horsepower_resid'],
     tractor_sales[, 'log_saleprice_resid'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Horsepower Residuals',
     ylab = 'Log Tractor Price Residuals',
     col = 'blue')


# Now calculate a curve for the prediction from tractor prices.
np_hp_fit_4 <- loess(log_saleprice_resid ~ horsepower_resid, tractor_sales, span = 0.75)


# Add a plot of this curve to the scattergraph.
tractor_sales[, 'horsepower_np_4'] <- np_hp_fit_4$fitted
lines(tractor_sales[order(tractor_sales[, 'horsepower_resid']), c('horsepower_resid', 'horsepower_np_4')],
      col = 'red', lwd = 3)


##################################################
# Fit another semi-parametric model
# with a nonparametric fit on horsepower
# Semiparametric model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_np_model_2 <- lm(data = tractor_sales,
                    formula = log_saleprice ~ horsepower_np_4 +
                      age + enghours +
                      diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_np_model_2)

# This is also a model of comparable quality.

# Now we have a few candidates.
# We can compare them with F-tests.

# Estimate a regression model
# with all of the candidates (the unrestricted model)
lm_np_model_3 <- lm(data = tractor_sales,
                    formula = log_saleprice ~ horsepower_np_4 +
                      horsepower + squared_horsepower +
                      age + enghours +
                      diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_np_model_3)


# F-test of unrestricted model vs. semi-parametric.
# A test of two restrictions (horsepower and horsepower_squared).
num_restr <- 2
RSS_unconstrained <- sum(lm_np_model_3$residuals^2)
RSS_constrained <- sum(lm_np_model_2$residuals^2)
num_obs <- nrow(tractor_sales)
num_vars <- 10 # In unrestricted model (not including constant).

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print(F_stat)
print(pf(F_stat, df1 = num_restr, df2 = num_obs - num_vars - 1,
      lower.tail = FALSE))

# A high F-statistic indicates a significant reduction in quality of fit.
# You reject the null that the restriction is true.

# F-test of unrestricted model vs. parametric.
# A test of one restriction.
num_restr <- 1
RSS_unconstrained <- sum(lm_np_model_3$residuals^2)
RSS_constrained <- sum(lm_model_7$residuals^2)
num_obs <- nrow(tractor_sales)
num_vars <- 10 # In unrestricted model (not including constant).

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print(F_stat)
print(pf(F_stat, df1 = num_restr, df2 = num_obs - num_vars - 1,
         lower.tail = FALSE))

# A high F-statistic indicates a significant reduction in quality of fit.
# You do not reject the null that the restriction is true.


# The full model is still statistically better.
# The submodels are also both acceptable models.
# Of course, the more flexible model does better
# but this, in some sense, uses many more "degrees of freedom"
# so it is not a fair comparison.
# Better to estimate the semiparametric part in the Box-Tidwell
# transformation, which estimates these features jointly.
# We we will do this in a future problem set.

# With these results, I would explore the GAM or the Box-Tidwell
# with the horsepower variable a candidate for the nonparametric term.


##################################################
# End
##################################################
