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
# Tractor_SampleSelection gives examples of
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
# wd_path <- '~/GitHub/QMB6912S22/demo_10/Tractor_SampleSelection'
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
# Loading the Data
##################################################

in_file_name <- sprintf('%s/%s', data_dir, 'TRACTOR7.csv')
tractor_sales <- read.csv(file = in_file_name)

# Inspect the contents.
print('Summary of tractor_sales Dataset:')
print(summary(tractor_sales))

# Make sure there are no problems with the data.



##################################################
# Generating New Variables
##################################################


# In Problem Set #6, we determined that taking logs
# of tractor prices produced a better model with
# a distribution closer to normal.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included a quadratic form for horsepower.

# Estimate a regression model.
lm_7 <- lm(data = tractor_sales,
           formula = log_saleprice ~
             horsepower + squared_horsepower +
             age +
             enghours +
             diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse',
       caption = "Quadratic Model for Tractor Prices")


##################################################
# Sample selection Models
##################################################






#
# # Print the output to a LaTeX file.
# tab_file_name <- 'bt_full.tex'
# out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
# cat("\\begin{verbatim}", file = out_file_name)
# sink(out_file_name, append = TRUE)
# print(bt_full)
# sink()
# cat("\\end{verbatim}", file = out_file_name, append = TRUE)
#
#

##################################################
# End
##################################################
