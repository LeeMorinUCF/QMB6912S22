##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specifications
# using Sample Selection Models
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
# FlyReel_SampleSelection gives examples of
#  sample selection models.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   sampleSelection library to estimate models
#     with sample selection.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_10/FlyReel_SampleSelection'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# Libraries to print tables of regression results.
library(xtable)
library(texreg)


# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)

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

# Create logarithm of dependent variable.
flyreels[, 'log_Price'] <- log(flyreels[, 'Price'])

# # Generate new dependent variable with results from Problem Set 6.
# # First define the Box-Cox transformation.
# Lambda_Price <- function(price, lambda) {
#
#   if (lambda == 0) {
#     return(log(price))
#   } else {
#     return((price^lambda - 1)/lambda)
#   }
#
# }
#
# # Recall the optimal exponent from the MLE.
# lambda_hat <- 0.43
# flyreels[, 'Trans_Price'] <- Lambda_Price(price = flyreels[, 'Price'],
#                                           lambda = lambda_hat)


# Replace Country Indicator with made_in_USA Indicator.
table(flyreels[, 'Country'], useNA = 'ifany')
flyreels[, 'made_in_USA'] <- flyreels[, 'Country'] == 'USA'
# Check:
table(flyreels[, 'Country'],
      flyreels[, 'made_in_USA'], useNA = 'ifany')


##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following
# linear regression model.

#--------------------------------------------------
# Interaction with Sealed and made_in_USA
#--------------------------------------------------

# As determined in Problem Sets 6 and 7,
# set target variable as the log transformation
# for remaining analysis.
target_var <- 'log_Price'

# Specify list of variables with interactions.
var_list <- c('Width', 'Diameter', 'Density',
              'Sealed', 'Machined', 'made_in_USA',
              'made_in_USA*Sealed')
# Notice that we keep the made_in_USA indicator
# to maintain different intercept by country.


# Specify the regression formula.
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

# Estimate the model and output to screen.
lm_6 <- lm(data = flyreels, formula = lm_fmla)
print(summary(lm_6))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sealed_USA.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_6),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sealed_USA',
       caption = "Linear Model for Fly Reel Prices")



##################################################
# Sample selection Models
##################################################






##################################################
# End
##################################################
