#############################################################################
#                                                                          ##
##          Project: Outliers detection 9 in R                             ##
##          Outlier Detection: Multivariate                                ##
##          R-package: Performance                                         ##
##-------------------------------------------------------------------------##
##          Programmer:  Ou Zhang                                          ##
##          Request Date: 11-16-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This is the example code to handle multivariate outliers ##
##                use function-check_outliers from 'performance' package   ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When: 11-16-2020                                               ##
##          Who:   Ou Zhang                                                ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
# --- References --- #

## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers",
              "performance","mvoutlier","car")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Load library.
library(MASS)
library(tidyverse)
library(robustbase)
library(car)

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/stat ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)


# Univariate
check_outliers(mtcars$mpg)
#> Warning: 4 outliers detected (cases 18, 19, 20, 28).
#> 

# Multivariate
# select only mpg and disp (continuous)
mt1 <- mtcars[, c(1, 3, 4)]
# create some fake outliers and attach outliers to main df
mt2 <- rbind(mt1, data.frame(mpg = c(37, 40), disp = c(300, 400), hp = c(110, 120)))
# fit model with outliers
model <- lm(disp ~ mpg + hp, data = mt2)

ol <- check_outliers(model)
# plot(ol)
insight::get_data(mode)[ol, ]
#> Warning: Could not get model data.
#> NULL


check_outliers(model, method = c("mahalabonis", "mcd"))
#> Warning: 6 outliers detected (cases 18, 20, 28, 31, 33, 34).
#> 
if (FALSE) {
  # This one takes some seconds to finish...
  check_outliers(model, method = "ics")
  
  # For dataframes
  check_outliers(mtcars)
  check_outliers(mtcars, method = "all")
}

##  --- EOF --- ##
