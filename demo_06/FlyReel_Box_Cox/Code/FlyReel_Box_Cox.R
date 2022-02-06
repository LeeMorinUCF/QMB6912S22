##################################################
#
# QMB 6912 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# February 6, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script analyzes the covariance between variables
# and makes comparisons between subsets of the data.
#
# Dependencies:
#   libraries to be added
#
#
##################################################

##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_05/FlyReel_Data_Vis'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
# tab_dir <- 'Tables' # Last week.


##################################################
# Load libraries
##################################################

# Packages with the Box-Cox Transformation
library(EnvStats)
library(MASS)



##################################################
# Load Data
##################################################

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'FlyReels.csv')
fly_col_names <- c('Name', 'Brand', 'Weight', 'Diameter', 'Width',
                   'Price', 'Sealed', 'Country', 'Machined')

# Load data.
flyreels <- read.csv(file = in_file_name, header = FALSE,
                     col.names = fly_col_names)

# Initial inspection.
print('Summary of FlyReels Dataset:')
print(summary(flyreels))



##################################################
# Generating Variables
##################################################

# Set categorical variables as factors.
cat_var_list <- colnames(flyreels)[lapply(flyreels, class) == "character"]
for (var_name in cat_var_list) {
  flyreels[, var_name] <- as.factor(flyreels[, var_name])
}

# Initial inspection.
print('FlyReels Dataset with Categorical Factors:')
print(summary(flyreels))



# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']


##################################################
# Transforming the Dependent Variable
##################################################

# In Problem Set 4, we investigated the distribution of
# our dependent variable,.
# We analyzed the distribution of the prices in levels
# and by taking logarithms.
# Now we will employ the Box-Cox transformation
# to decide between these specifications.
# First, we can analyze the distributions
# to determine whether they are normally distributed.



##################################################
# Kernel-smoothed pdf of fly reel price.
print('Plotting kernel-smoothed pdf')
print('of fly reel price.')
##################################################

density_price <- density(flyreels[, 'Price'])
fig_file_name <- 'density_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_price,
     main = 'Kernel-smoothed pdf of Fly Reel Prices',
     xlab = 'Price')
dev.off()




##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the natural logarithm of price.')
##################################################

density_log_price <- density(log(flyreels[, 'Price']))
fig_file_name <- 'density_log_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Price')
dev.off()


# To compare these to the normal distribution,
# we can draw a Q-Q plot, plotting the quantiles of
# each on a scatterplot.






##################################################
# Calculating Box-Cox Transformation
# for Univariate Likelihood Function.
print(c('Calculating Box-Cox Transformation',
        'for Univariate Likelihood Function.'))
##################################################





##################################################
# Problem Set 6: Univariate Likelihood Function
##################################################

# Contents of MorinPS6/

#--------------------------------------------------
#
#--------------------------------------------------


# Box-Cox transformation.
Lambda_Price <- function(price, lambda) {

  if (lambda == 0) {
    return(log(price))
  } else {
    return((price^lambda - 1)/lambda)
  }

}

log_like_uni <- function(price, lambda) {

  n <- length(price)
  lambda_price <- Lambda_Price(price, lambda)
  mu_0_lambda <- mean(lambda_price)
  sigma_2_lambda <- sum((lambda_price - mu_0_lambda)^2)/n

  like <- - n/2*log(2*pi*sigma_2_lambda)
  like <- like - 1/2/sigma_2_lambda*sum((lambda_price - mu_0_lambda)^2)
  like <- like + (lambda - 1)*sum(log(price))

  return(like)

}

# Calculate values of the log-likelihood function.
lambda_grid <- seq(0, 2, by = 0.001)
like_grid <- 0*lambda_grid
for (lambda_num in 1:length(lambda_grid)) {
  like_grid[lambda_num] <- log_like_uni(price = flyreels[, 'Price'],
                                    lambda = lambda_grid[lambda_num])
}

# Calculate the log-likelihood function.
plot(x = lambda_grid, y = like_grid,
     type = 'l',
     main = 'Log-likelihood Function',
     xlab = 'Lambda',
     ylab = 'Log-likelihood')

# Calculate restricted likelihood values for mu = 0, 1.
like_mu_0 <- log_like_uni(price = flyreels[, 'Price'], lambda = 0)
like_mu_1 <- log_like_uni(price = flyreels[, 'Price'], lambda = 1)

# Find the MLE, the unrestricted estimate.
lambda_hat <- lambda_grid[which.max(like_grid)]
like_MLE <- max(like_grid)
# Check:
# like_MLE == log_like_uni(price = flyreels[, 'Price'], lambda = lambda_hat)


# Calculate likelihood ratio statistics.
LR_stat_0 <- - 2*(like_mu_0 - like_MLE)
LR_stat_1 <- - 2*(like_mu_1 - like_MLE)

# Compare to quantile of chi-squared distribution with 1 degree of freedom.
LR_cv_5 <- qchisq(p = 0.95, df = 1)

# Calculate p-values for these tests.
p_value_0 <- 1 - pchisq(q = LR_stat_0, df = 1)
p_value_1 <- 1 - pchisq(q = LR_stat_1, df = 1)
# Reject them both. Use the transformation at the MLE.


#--------------------------------------------------
#
#--------------------------------------------------


bc_grid_ES <- EnvStats::boxcox(x = flyreels[, 'Price'],
                               lambda = lambda_grid,
                               optimize = FALSE,
                               objective.name = "Log-Likelihood")
plot(bc_grid_ES$lambda, bc_grid_ES$objective)

# Find optimal value of lambda.
bc_grid_ES_opt <- EnvStats::boxcox(x = flyreels[, 'Price'],
                               lambda = range(lambda_grid),
                               optimize = TRUE,
                               objective.name = "Log-Likelihood")

bc_grid_ES_opt$lambda

summary(bc_grid_ES_opt$lambda)



#--------------------------------------------------
#
#--------------------------------------------------


# Use the function from the MASS package.

# In the MASS package, the notation is the same as for a linear model.
summary(lm(Price ~ 1, data = flyreels))
bc_grid_MASS <- MASS::boxcox(Price ~ 1,
                             data = flyreels,
                             lambda = lambda_grid)
# Plot from the model object.
plot(bc_grid_MASS$x, bc_grid_MASS$y)




# Output to MorinPS6.tex...


##################################################
# Problem Set 8: Running a Regression Model
##################################################

# Contents of MorinPS8/

#--------------------------------------------------
#
#--------------------------------------------------


# Generate new dependent variable with results from Problem Set 6.
flyreels[, 'Trans_Price'] <- Lambda_Price(price = flyreels[, 'Price'],
                                          lambda = lambda_hat)

# Specify first regression model.
colnames(flyreels)
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'Country')
lm_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))

lm_model_1 <- lm(data = flyreels, formula = lm_fmla)
summary(lm_model_1)

#--------------------------------------------------
#
#--------------------------------------------------


# Create indicator for made_in_USA.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')


# Specify second regression model.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA')
lm_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))

lm_model_2 <- lm(data = flyreels, formula = lm_fmla)
summary(lm_model_2)


#--------------------------------------------------
#
#--------------------------------------------------


# Consider interaction terms in third regression model.
table(flyreels[, 'Machined'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')
# All American reels are machined.
table(flyreels[, 'Sealed'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')


var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
lm_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))

lm_model_3 <- lm(data = flyreels, formula = lm_fmla)
summary(lm_model_3)


#--------------------------------------------------
# Box-Cox specification
#--------------------------------------------------


# Start with specification from the second regression model.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA')
bc_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))


bc_grid_ES <- EnvStats::boxcox(x = lm(formula = bc_fmla, data = flyreels),
                               lambda = lambda_grid,
                               optimize = FALSE #,
                               # objective.name = "Log-Likelihood"
                               )
plot(bc_grid_ES$lambda, bc_grid_ES$objective)
summary(bc_grid_ES$lm.obj)
# Looks the same as the model above, with plug-in lambda MLE.


# Find optimal value of lambda.
bc_grid_ES_opt <- EnvStats::boxcox(x = lm(formula = bc_fmla, data = flyreels),
                                   lambda = range(lambda_grid),
                                   optimize = TRUE #,
                                   # objective.name = "Log-Likelihood"
                                   )

summary(bc_grid_ES_opt$lm.obj)

bc_grid_ES_opt$lambda



#--------------------------------------------------
#
#--------------------------------------------------


# Use the function from the MASS package.




# In the MASS package, the notation is the same as for a linear model.
bc_grid_MASS <- MASS::boxcox(bc_fmla,
                             data = flyreels,
                             lambda = lambda_grid)

# Plot from the model object.
plot(bc_grid_MASS$x, bc_grid_MASS$y)


#--------------------------------------------------
# One more method: Code it from scratch to maximize jointly.
# I don't expect a student to do this but it is instructive.
#--------------------------------------------------

# Likelihood function for model with specific names
# of dependent variables.
log_like_multi <- function(fmla, df, lambda) {

  # Perform Box-Cox transformation.
  df[, 'Trans_Price'] <- Lambda_Price(df[, 'Price'], lambda)

  # Estimate model to concentrate out remaining parameters.
  lm_model_1 <- lm(data = df, formula = fmla)
  n <- nrow(df)
  sigma_2_lambda <- sum(lm_model_1$residuals^2)/n

  like <- - n/2*log(2*pi*sigma_2_lambda)
  like <- like - 1/2/sigma_2_lambda*sum(lm_model_1$residuals^2)
  like <- like + (lambda - 1)*sum(log(df[, 'Price']))

  return(like)

}

# Reuse specification from the second regression model.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA')
# Constant only.
# var_list <- c(1)
bc_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))


# Calculate values of the log-likelihood function.
lambda_grid <- seq(0, 2, by = 0.001)
like_grid <- 0*lambda_grid
for (lambda_num in 1:length(lambda_grid)) {
  like_grid[lambda_num] <- log_like_multi(fmla = bc_fmla,
                                          df = flyreels,
                                          lambda = lambda_grid[lambda_num])
}

# Calculate the log-likelihood function.
plot(x = lambda_grid, y = like_grid,
     type = 'l',
     main = 'Log-likelihood Function',
     xlab = 'Lambda',
     ylab = 'Log-likelihood')

# Find the MLE, the unrestricted estimate.
lambda_hat <- lambda_grid[which.max(like_grid)]
like_MLE <- max(like_grid)

# Perform Box-Cox transformation.
flyreels[, 'Trans_Price'] <- Lambda_Price(flyreels[, 'Price'], lambda_hat)

# Estimate model to concentrate out remaining parameters.
lm_model_1 <- lm(data = flyreels, formula = fmla)
summary(lm_model_1)
# Interesting.



#--------------------------------------------------
# Try the optimization again, looping on the model.
#--------------------------------------------------


# Reuse specification from the second regression model.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA')
# Constant only.
# var_list <- c(1)
bc_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))

lambda_hat_opt <- 0.40
# lambda_hat_opt <- lambda_hat
alpha <- 0.01

for (i in 1:100) {

  # Estimate the lm object after transforming with this lambda.
  flyreels[, 'Trans_Price'] <- Lambda_Price(flyreels[, 'Price'], lambda_hat_opt)
  lm_loop <- lm(formula = bc_fmla, data = flyreels)



  # Find optimal value of lambda.
  bc_grid_ES_opt <- EnvStats::boxcox(x = lm_loop,
                                     lambda = range(lambda_grid),
                                     optimize = TRUE #,
                                     # objective.name = "Log-Likelihood"
  )

  # lambda_hat_opt <- bc_grid_ES_opt$lambda
  lambda_hat_opt <- alpha*lambda_hat_opt + (1 - alpha)*bc_grid_ES_opt$lambda
  print(lambda_hat_opt)
}





summary(bc_grid_ES_opt$lm.obj)





##################################################
# End
##################################################
