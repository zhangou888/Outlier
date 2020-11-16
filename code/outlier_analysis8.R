#############################################################################
#                                                                          ##
##          Project: Outliers detection 8 in R                             ##
##          Outlier Detection (Part 2): Multivariate                       ##
##          Mahalanobis distance | Robust estimates (MCD): Example in R    ##
##-------------------------------------------------------------------------##
##          Programmer:  Mishtert T                                        ##
##          Request Date: 12-10-2019                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This is the example code to handle multivariate outliers ##
##  https://medium.com/towards-artificial-intelligence/outlier-detection-part-2-multivariate-df486f658d09 ##
##         The R content presented in this document is mostly based on an  ##
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
              "robustbase","MASS","mvoutlier","car")
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

## ------------ Step 4: Data source ----------- #
# "Animals "data from the "MASS" package in R for demonstration.
data(Animals)
attach(Animals)

# Check data structure.
str(Animals)
head(Animals)

# The variables for the demonstration are body weight and brain weight of 
# Animals which are converted to its log form (to make highly skewed 
# distributions less skewed)
Y <- data.frame(body = log(Animals$body), 
                brain = log(Animals$brain))

# plot scatterplot to check the distribution.
plot_fig <- ggplot(Y, aes(x = body, y = brain)) + 
            geom_point(size = 5) +
            xlab("log(body)") + ylab("log(brain)") + 
            ylim(-5, 15) +
            scale_x_continuous(limits = c(-10, 16), 
                               breaks = seq(-15, 15, 5))

# Display scatterplot.
plot_fig


##  -------- Step 5: Mahalanobis distance  -----------  ##
# Mahalanobis (or generalized) distance for observation is the distance from 
# this observation to the center, taking into account the covariance matrix.

# 1. Classical Mahalanobis distances: sample mean as estimate for location and 
#     sample covariance matrix as estimate for scatter.
# 
# 2. To detect multivariate outliers the Mahalanobis distance is compared with 
#    a cut-off value, which is derived from the chi-square distribution
# 
# 3. In two dimensions we can construct corresponding 97.5% tolerance ellipsoid,
#    which is defined by those observations whose Mahalanobis distance does not 
#    exceed the cut-off value.

Y <- data.frame(body = log(Animals$body), 
                brain = log(Animals$brain))

Y_center <- colMeans(Y) # sample mean
Y_cov <- cov(Y)         # sample cov
Y_radius <- sqrt(qchisq(0.975, df = ncol(Y))) # Circle radius

Y_ellipse <- data.frame(car::ellipse(center = Y_center,
                                shape = Y_cov,
                                radius = Y_radius, 
                                segments = 100, 
                                draw = FALSE))

# give the same colnames.
colnames(Y_ellipse) <- colnames(Y)

# plot scatterplot with Mahalanobis distance ellipse.
plot_fig <- plot_fig + geom_polygon(data=Y_ellipse, 
                                    color = "dodgerblue",
               fill = "dodgerblue", alpha = 0.2) +
  geom_point(aes(x = Y_center[1], y = Y_center[2]),
             color = "blue", size = 6)
plot_fig


##  -------- Step 6: Robust estimates of location and scatter  ------  ##
# Minimum Covariance Determinant (MCD) estimator of Rousseeuw is a popular 
# robust estimator of multivariate location and scatter.
Y_mcd <- covMcd(Y)
# Robust estimate of location
Y_mcd$center
# Robust estimate of scatter
Y_mcd$cov

# --- 6.1 Robust Tolerance Ellipsoid: Animals --- #
Y_mcd <- covMcd(Y)
ellipse_mcd <- data.frame(ellipse(center = Y_mcd$center,
                                  shape = Y_mcd$cov,
                                  radius= Y_radius, 
                                  segments=100,draw=FALSE))
colnames(ellipse_mcd) <- colnames(Y)
plot_fig <- plot_fig +
  geom_polygon(data=ellipse_mcd, color="red", fill="red", 
               alpha=0.3) +
  geom_point(aes(x = Y_mcd$center[1], y = Y_mcd$center[2]),
             color = "red", size = 6)
plot_fig

# ---- 6.2 Distance-Distance plot --- #
# The distance-distance plot shows the robust distance of each observation versus 
# its classical Mahalanobis distance, obtained immediately from MCD object.
plot(Y_mcd, which = "dd")


# Check outliers
Animals[c(6,14,16,26),]

# Minimum Covariance Determinant estimates plugged with Mahalanobis distance 
# provide us better detection capability of outliers than our classical methods.



##  --- EOF --- ##