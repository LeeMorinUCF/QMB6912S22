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

# lattice library to create matrices of scatterplots
library(lattice)



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


# Relative histogram of density.
# hist(flyreels[, 'Density'],
#      main = 'Relative Histogram of Flyreel Density',
#      xlab = 'Density',
#      probability = TRUE)


##################################################
# Generating Scatterplot Matrices.
print('Generating Scatterplot Matrices.')
##################################################


# Create scatterplots of numeric variables.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density')
fig_file_name <- 'slpom_num_only.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()


# Add some categorical variables to scatterplots.
splom_var_list <- c('Price', 'Width', 'Diameter', 'Density',
                    'Sealed', 'Machined')
fig_file_name <- 'slpom_with_cat.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()
# This is a busy figure with multiple categorical variables.


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

# Plot in scatterplot matrix.
fig_file_name <- 'slpom_with_sealed_mach.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
splom(flyreels[, splom_var_list])
dev.off()


##################################################
# Generating Scatterplot Matrices
# Colored by Country of Manufacture.
print(c('Generating Scatterplot Matrices',
        'Colored by Country of Manufacture.'))
##################################################


# Color by country of origin.
fig_file_name <- 'slpom_by_country.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
super.sym <- trellis.par.get("superpose.symbol")
splom(~flyreels[, splom_var_list],
      groups = Country,
      data = flyreels,
      panel = panel.superpose,
      cex = 0.5,
      varname.cex = 0.75,
      axis.text.cex = 0.1,
      axis.text.col = 'white',
      key = list(text = list(levels(flyreels[, 'Country'])),
                 title = "Three Countries of Origin",
                 columns = 3,
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3])
                 ))
dev.off()




##################################################
# End
##################################################
