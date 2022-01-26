##################################################
#
# QMB 6911 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# July 21, 2020
#
##################################################
#
# Sample code for the problem sets in the course QMB 6911,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
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

# Set working directory.
wd_path <- '~/Teaching/QMB6911_Summer_2021/Draft1'
setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

library(xtable)
# library(texreg)


##################################################
# Problem Set 3: Summarize Covariates
##################################################

# Contents of MorinPS3/

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'FlyReels.csv')
fly_col_names <- c('Name', 'Brand', 'Weight', 'Diameter', 'Width',
                   'Price', 'Sealed', 'Country', 'Machined')

# Load data.
flyreels <- read.csv(file = in_file_name, header = FALSE,
                     col.names = fly_col_names)

# Initial inspection.
summary(flyreels)

#--------------------------------------------------
# Summarize numeric variables.
#--------------------------------------------------

# Summarize numeric variables by country of manufacture.
country_sum <- data.frame(Country = unique(flyreels$Country))
for (var_name in colnames(flyreels)[lapply(flyreels, class) == 'numeric']) {

  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  country_sum[, col_names] <- tapply(flyreels$Price, flyreels$Country,
                                     function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])

}

out_tab <- t(country_sum[, 2:ncol(country_sum)])
colnames(out_tab) <- country_sum[, 1]


# Output to TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:summ_by_country',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'Summary by Country of Manufacture')

tab_file_name <- sprintf('summ_by_country.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



# Summarize numeric variables by brand of fly reel.
country_sum <- data.frame(Brand = unique(flyreels$Brand))
for (var_name in colnames(flyreels)[lapply(flyreels, class) == 'numeric']) {

  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  country_sum[, col_names] <- tapply(flyreels$Price, flyreels$Brand,
                                     function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])

}

out_tab <- t(country_sum[, 2:ncol(country_sum)])
colnames(out_tab) <- country_sum[, 1]




#--------------------------------------------------
# Summarize categorical variables.
#--------------------------------------------------

table(flyreels[, 'Brand'], useNA = 'ifany')
table(flyreels[, 'Sealed'], useNA = 'ifany')
table(flyreels[, 'Country'], useNA = 'ifany')
table(flyreels[, 'Machined'], useNA = 'ifany')

# Comparison across brand names.
table(flyreels[, 'Brand'], flyreels[, 'Sealed'], useNA = 'ifany')
table(flyreels[, 'Brand'], flyreels[, 'Country'], useNA = 'ifany')
table(flyreels[, 'Brand'], flyreels[, 'Machined'], useNA = 'ifany')

# Assemble these into a table for output.
out_tab <- cbind(table(flyreels[, 'Brand'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Country'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Sealed'], useNA = 'ifany'),
                 table(flyreels[, 'Brand'], flyreels[, 'Machined'], useNA = 'ifany')
                 )

# Specify column names and add totals.
colnames(out_tab) <- c("Total", "China", "Korea", "USA",
                       "Unsealed", "Sealed", "Cast", "Machined")
out_tab <- rbind(out_tab, colSums(out_tab))
rownames(out_tab)[length(rownames(out_tab))] <- "Totals"

# Output a file with a LaTeX table.



# Output to TeX file.
out_xtable <- xtable(out_tab[, c(2, 3, 4, 1)],
                     digits = 0, label = 'tab:country_by_brand',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'Country of Manufacture by Brand of Fly Reel')

tab_file_name <- sprintf('country_by_brand.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


# Output to TeX file.
out_xtable <- xtable(out_tab[, c(5:8, 1)],
                     digits = 0, label = 'tab:design_by_brand',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'Reel Design by Brand of Fly Reel')

tab_file_name <- sprintf('design_by_brand.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


# Need to fix this one.


##################################################
# Problem Set 4: Analyze Dependent Variable
##################################################

# Plot EDF in base R and output to figure.
ecdf_price <- ecdf(flyreels[, 'Price'])
fig_file_name <- 'ecdf_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of Fly Reel Prices',
     xlab = 'Price',
     ylab = 'Empirical C.D.F.')
dev.off()


# Relative histogram of price.
fig_file_name <- 'hist_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
hist(flyreels[, 'Price'],
     main = 'Relative Histogram of Fly Reel Prices',
     xlab = 'Price',
     probability = TRUE)
dev.off()


# Kernel-smoothed pdf of the natural logarithm of price.
density_log_price <- density(log(flyreels[, 'Price']))
fig_file_name <- 'density_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of the Natural Log. of Fly Reel Prices',
     xlab = 'Price')
dev.off()


##################################################
# Problem Set 5: Generating Variables and Scatterplots
##################################################

# Contents of MorinPS5/

# Create a density variable.
colnames(flyreels)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']


# Relative histogram of density.
hist(flyreels[, 'Density'],
     main = 'Relative Histogram of Flyreel Density',
     xlab = 'Density',
     probability = TRUE)


# Scatterplots.
library(lattice)
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density',
                           'Sealed', 'Machined')
splom(flyreels[, splom_var_list])
# This is a busy figure. Let's simplify it.

# Create new categorical variable combining sealed and machined.
table(flyreels[, c('Sealed', 'Machined')], useNA = 'ifany')
flyreels[, 'Seal_Mach'] <- factor(sprintf('%s_%s',
                                   substr(flyreels[, 'Sealed'], 1, 1),
                                   substr(flyreels[, 'Machined'], 1, 1)))
table(flyreels[, c('Seal_Mach', 'Sealed')], useNA = 'ifany')
table(flyreels[, c('Seal_Mach', 'Machined')], useNA = 'ifany')

# Revise list of variables for 2-dimensional factor.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density',
                    'Seal_Mach')


# Plot in Scatter Plot Matrix.
super.sym <- trellis.par.get("superpose.symbol")
splom(~flyreels[, splom_var_list],
      groups = Country,
      data = flyreels,
      panel = panel.superpose,
      cex = 0.5,
      varname.cex = 0.75,
      axis.text.cex = 0.1,
      axis.text.col = 'white',
      key = list(title = "Three Countries of Origin",
                 columns = 3,
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(levels(flyreels[, 'Country']))))


# Output to MorinPS5.tex...


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
library(MASS)
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




# Output to MorinPS8.tex...


##################################################
# Problem Set 9: Nonlinear Models
##################################################

# Contents of MorinPS9/

#-------------------------------------------------------
# Generalized Additive Model
#-------------------------------------------------------

library(mgcv)


# Model specification.
# var_list <- c('Width', 'Diameter', 'Density',
#               'Sealed', 'Machined', 'made_in_USA')
# var_list <- c('Width', 'Diameter', 'Density', 'Brand',
#               'Sealed', 'Machined', 'made_in_USA')
var_list <- c('Diameter', 'Density', 'Brand',
              'Sealed', 'Machined', 'made_in_USA')
gam_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))

gam_model_1 <- gam(gam_fmla, data = flyreels)
summary(gam_model_1)


# Allow for some nonlinearity.
var_list <- c('s(Diameter)', 's(Density)', 'Brand',
              'Sealed', 'Machined', 'made_in_USA')
gam_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                               paste(var_list, collapse = ' + ')))

gam_model_2 <- gam(gam_fmla, data = flyreels)
summary(gam_model_2)


#-------------------------------------------------------
# Box-Tidwell Transformation
#-------------------------------------------------------

library(car)

# Model specification.
# var_list <- c('Width', 'Diameter', 'Density',
#               'Sealed', 'Machined', 'made_in_USA')
# var_list <- c('Width', 'Diameter', 'Density', 'Brand',
#               'Sealed', 'Machined', 'made_in_USA')
# var_list <- c('Density', 'Diameter')
# var_list <- c('Diameter')
# var_list <- c('Density')
bt_fmla <- as.formula(sprintf('Trans_Price ~ %s',
                              paste(var_list, collapse = ' + ')))
var_list <- c('Brand',
              'Sealed', 'Machined', 'made_in_USA')
bt_other <- as.formula(sprintf(' ~  %s',
                              paste(var_list, collapse = ' + ')))

# box.tidwell() is deprecated, replaced with boxTidwell().
# bt_model_1 <- box.tidwell(formula = bt_fmla, other.x = bt_other, data = flyreels)
bt_model_1 <- boxTidwell(formula = bt_fmla, other.x = bt_other, data = flyreels)
print(bt_model_1)





# Output to MorinPS9.tex...


##################################################
# Problem Set 10: Testing for Country-of-manufacture Effect
##################################################

# Contents of MorinPS10/


summary(gam_model_2)


# Output to MorinPS10.tex...


##################################################
# Problem Set 11: Analysis of Drag-fishing Reels
##################################################

# Contents of MorinPS11/


# Output to MorinPS11.tex...


##################################################
# End
##################################################
