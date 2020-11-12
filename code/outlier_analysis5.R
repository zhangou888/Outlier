#############################################################################
#                                                                          ##
##          Project: Outliers detection 5 in R                             ##
##          Drawing a 95% confidence interval in R                         ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer:  Nathan Lemoine                                    ##
##          Request Date: 11-03-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This chapter explains the purpose of some of the most    ##
##         commonly used outlier analysis and how to implement them in R   ##
##  https://www.r-bloggers.com/2013/08/drawing-a-95-confidence-interval-in-r/                         ##
##         The R content presented in this document is mostly based on an  ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When: 11-09-2020                                               ##
##          Who:   Ou Zhang                                                ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers",
              "mvtnorm","ellipse","car",
              "EnvStats","mvoutlier")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})


library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()

set.seed(17)
# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)
# Set the means
mu <- c(5,5)

# Get the correlation matrix
# scales a covariance matrix into the corresponding correlation matrix.
P <- cov2cor(sigma2)

# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))

# Plot the data
plot(p)

# Plot the ellipse
# lines(ellipse(P, centre = c(5,5)) -> center is the mean , col='red')
ellipse(P, centre = c(5,5)) %>%
  lines(., col='red')

# obtain Eigenvalue and eigenvector
evals <- eigen(P)$values
evecs <- eigen(P)$vectors

# Angles of a circle
a <- seq(0, 2*pi, len=100)

# Get critical value
c2 <- qchisq(0.95, 2)
c <- sqrt(c2)

# Get the distances
xT <- c * sqrt(evals[1]) * cos(a)
yT <- c * sqrt(evals[2]) * sin(a)
M <- cbind(xT, yT)

# Covert the coordinates
transM <- evecs %*% t(M)
transM <- t(transM)

lines(transM + mu)


# ---- Example 2: from Ellipse package --- #
# Plot the estimate and joint 90% confidence region for the displacement and cylinder
# count linear coefficients in the mtcars dataset
data(mtcars)
fit <- lm(mpg ~ disp + cyl, data=mtcars)

# which indicate IVs.
plot(ellipse(fit, which = c('disp', 'cyl'),
             level = 0.95), type = 'l')

points(fit$coefficients['disp'], fit$coefficients['cyl'])

# --- Example 3: ---- #
mu = c(0,0)
sigma = matrix(c(20,0,0,45),nrow=2)
z = rmvnorm(10000,mu,sigma)

# 95% CI Ellpise
dataEllipse(z,levels=.95)

# with center coordinates
center <- apply(z, 2, mean)
cov_mat <- cov(z)
ellipse(center, cov_mat, col="red", radius=sqrt(2 * qf(.95, 2, 9999)))

# --- EOF --- #
