#############################################################################
#                                                                          ##
##          Project: Outliers detection 7 in R                             ##
##          A New Way to Handle Multivariate Outliers                      ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer:  Will Hipson                                       ##
##          Request Date: 01-09-2019                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This is the example code to handle multivariate outliers ##
##  https://www.r-bloggers.com/2019/01/a-new-way-to-handle-multivariate-outliers/                         ##
##         The R content presented in this document is mostly based on an  ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When: 11-16-2020                                               ##
##          Who:   Ou Zhang                                                ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
# --- References --- #
# Leys, C., Klein, O., Dominicy, Y., & Ley, C. (2018). 
# Detecting multivariate outliers: Use a robust variant of Mahalanobis 
# distance. Journal of Experimental Social Psychology, 74, 150-156.

## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers",
              "mvtnorm","ellipse","car",
              "interactions","mvoutlier")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Load library.
library(psych)
library(tidyverse)
library(simstudy)
# library(jtools) # Now Jtools becomes part of 'interaction' package
library(interactions)

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/stat ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

## Step 4: Generate data 
c <- matrix(c(1, .43, .28, .43, 1, .12, .28, .12, 1), nrow = 3)
c

#  use the correlation matrix when I generate the data. 
# In the function genCorData, mu refers to the sample means and sigma 
# refers to their respective standard deviations.
set.seed(206134)
data <- genCorData(600, 
                   mu = c(2.65, 3.56, 2.21), 
                   sigma = c(.56, 1.12, .70), 
                   corMatrix = c)
data <- data %>%
  select(-id) %>%
  rename(alone_affinity = V1, time_alone = V2, loneliness = V3)
data

# Add outliers.
c2 <- matrix(c(1, .83, .07, .83, 1, .67, .07, .67, 1), nrow = 3)
c2

#  use the correlation matrix when I generate the data. 
# In the function genCorData, mu refers to the sample means and sigma 
# refers to their respective standard deviations.
set.seed(1234567)
outlr <- genCorData(20, 
                   mu = c(4.0, 2.0, 1.0), 
                   sigma = c(.90, 2.20, 1.60), 
                   corMatrix = c2)
outlr  <- outlr %>%
  select(-id) %>%
  rename(alone_affinity = V1, time_alone = V2, loneliness = V3)
outlr 


# merge outlier and data
data_outlier <- rbind(data, outlr)

# take a look at the univariate and multivariate distributions
pairs.panels(data_outlier, stars = TRUE)

## Step 5: Fit into model. 
# 5.1: Model 1: All Data - Including Outliers
model1 <- lm(loneliness ~ .*time_alone, data = data_outlier)
summary(model1)

# visualize this more clearly with simple slopes:
model1_int <- lm(loneliness ~ time_alone * alone_affinity, data = data_outlier)
interactions::interact_plot(model1_int, 
                            pred = "time_alone", 
                            modx = "alone_affinity") 

# Check linearity for each variable.
interactions::interact_plot(model1_int, 
                            pred = "time_alone", 
                            modx = "alone_affinity", 
                            linearity.check = TRUE)

# 5.2: Model 2 - Mahalanobis Distance 
# A popular way to identify and deal with multivariate outliers 
# is to use Mahalanobis Distance (MD).
# MD calculates the distance of each case from the central mean. 
# Larger values indicate that a case is farther from where most of 
# the points cluster. 
psych::outlier(data_outlier)

# Calculate MD.
# Without outlier data
md <- mahalanobis(data, 
                  center = colMeans(data_outlier), 
                  cov = cov(data_outlier))
# Alpha level and cutoff.
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(data_outlier)))

names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH
data_clean_mh <- data_outlier[-excluded_mh, ]
data[excluded_mh, ]

# Using this cut-off, only one outlier was identified.
# > alone_affinity time_alone loneliness
# 1:       2.925864   7.723659   3.498047

# Rerun the model with this outlier omitted:
model2 <- lm(loneliness ~ .*time_alone, data = data_clean_mh)
summary(model2)

# Model 3 - Minimum Covariance Determinant --- #

# MD, which Leys et al. (2018) argue is not a robust way to 
# determine outliers. The problem lies with the fact that MD uses 
# the means and covariances of all the data - including the outliers.
# it makes more sense to base the criteria for removal using a subset of the 
# data that is the most central. This is the idea behind Minimum Covariance 
# Determinant, which calculates the mean and covariance matrix based on 
# the most central subset of the data.
library(MASS)
output75 <- cov.mcd(data_outlier, 
                    quantile.used = nrow(data_outlier)*.75)

mhmcd75 <- mahalanobis(data_outlier, output75$center, output75$cov)
names_outlier_MCD75 <- which(mhmcd75 > cutoff)
excluded_mcd75 <- names_outlier_MCD75
data_clean_mcd <- data_outlier[-excluded_mcd75, ]
data_outlier[excluded_mcd75, ]

# This approach identified 2 outliers, 
# as opposed to the 1 identified with the traditional MD.
# alone_affinity time_alone loneliness
# 1:       2.925864 7.72365899  3.4980467
# 2:       1.873683 0.03987971  0.4160354

# Let's see whether removing these cases changes the results:
model3 <- lm(loneliness ~ .*time_alone, data = data_clean_mcd)
summary(model3)

# This is clearly demonstrated in the simple slopes:
model3_int <- lm(loneliness ~ time_alone * alone_affinity, 
                 data = data_clean_mcd)
interact_plot(model3_int, 
              pred = "time_alone", 
              modx = "alone_affinity")

# Visualization.
pairs.panels(data_clean_mcd, stars = TRUE)

# The Minimum Covariance Determinant version of MD is a more robust method of 
# identifying and removing outliers that would otherwise go unnoticed with 
# traditional MD.

##  --- EOF --- ##

