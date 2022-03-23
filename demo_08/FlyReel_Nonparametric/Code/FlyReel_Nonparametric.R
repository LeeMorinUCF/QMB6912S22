##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Nonlinear Model Specfication
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# March 22, 2022
#
##################################################
#
# FlyReel_Nonparametric gives examples of linear
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   which are estimated using nonparametric methods.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   mgcv to fit the models within a generalized
#   additive model (GAM).
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_08/FlyReel_Nonparametric'
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

# Library mgcv for estimating Generalized Additive Models
library(mgcv)

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





##################################################
# Generalized Additive Model
##################################################



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


##################################################
# End
##################################################
