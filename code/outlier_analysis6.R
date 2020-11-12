#############################################################################
#                                                                          ##
##          Project: Outliers detection in Regression 3                    ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer:  Michaelino Mervisiano
##
##          Request Date: 11-10-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This chapter explains the purpose of some of the most    ##
##         commonly used outlier analysis and how to implement them in R   ##
##  https://towardsdatascience.com/how-to-detect-unusual-observations-on-your-regression-model-with-r-de0eaa38bc5b ##
##         The R content presented in this document is mostly based on an  ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When: 11-10-2020                                               ##
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

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/stat ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

# How to Identify Unusual Observations in Regression?
# Created by Michaelino Mervisiano
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])

# fit the data into the linear regression model
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

# Outliers
# Outliers observations aren't predicted well by regression model.

# From our regression model example, we can start
# investigating outliers observation by using Q-Q plot.
car::qqPlot(fit,labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

# Applying outlierTest function is helping us to
# confirm if potential outliers are indeed outliers.
car::outlierTest(fit)

# -- Check 'Nevada' real murder rate and fitted murder rate
states["Nevada",]
fitted(fit)["Nevada"]

# --- High-leverage points ---- #
# You can compute the high leverage observation by looking at the
# ratio of number of parameters estimated in model and sample size.
# If an observation has a ratio greater than 2-3 times the average ratio,
# then the observation considers as high-leverage points.
# high leverage function
highleverage <- function(fit) {

  p <- length(coefficients(fit)) # number of IVs + Intercept
  n <- length(fitted(fit))       # number of observations (N)
  ratio <- p/n
  # leverage ratio
  plot(hatvalues(fit), main="Index Plot of Ratio")

  # 2-3 times the average ratio as cutoff lines.
  abline(h=c(2,3)*ratio, col="red", lty=2)

  # identify high leverage points from graph.
  graphics::identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

# run function
highleverage(fit)

# The graph above indicates that Alaska and California have the highest ratio.
# It means these two states are quite unusual if we compared their predictor
# values compared to other states.

##  ----- Influential Observations ---- ##
##  Influence = Leverage x Discrepancy
# 1. Cook's distance
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


# 2. Added Variable plots
# It can be easily created using the avPlots() function in car package.
# From the graph below, the straight line in each plot is the actual
# regression coefficient for that predictor variable.
car::avPlots(fit, ask=FALSE, id.method="identify")


# incorporate all information from outlier, high-leverage,
# and influential observations into a single informative plot.
car::influencePlot(fit, id.method="identify",
                   main="Influence Plot",
                   sub="Circle size is proportional to Cook's distance")



# --- EOF --- #
