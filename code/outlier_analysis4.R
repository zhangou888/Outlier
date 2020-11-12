#############################################################################
#                                                                          ##
##          Project: Outliers detection in Regression 2                    ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer: William G. Jacoby  (MSU)                           ##
##          Request Date: 11-06-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note:
##         data : https://quantoid.net/files/702/weakliem2.txt
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When:                                                          ##
##          Who:                                                           ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers","EnvStats",
              "mvoutlier","xtable")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

library(ggplot2)
library(outliers)

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/stat ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

# Ex 1 (use Weakliem.txt DATA)
# Data from World Values Survey 1990.
# Variable- secpay: attitude to two secretaries with the same jobs
# getting paid different amounts if one is better at the job than the other.
# 1=Fair, 2=Unfair.
# gini: the gini coefficient of income inequality in the country.
# 0=perfect equality, 1=perfect inequality.
# gdp: GDP per capita in US dollars;
# democracy: 1=experienced democratic rule for at least 10 years.
# Here we look only at non-democratic countries.
weakliem2 <- read.table("C:/R/projects/outlier/Outlier/data/weakliem.txt",
                        header=TRUE)
attach(weakliem2)

# check the data structure.
str(weakliem2)
head(weakliem2)
weakliem2 <- weakliem2 %>%
  mutate(id = row_number())

plot(gini, secpay, main = "Nondemocratic countries",
     xlab="Gini", ylab="Attitudes towards inequality(mean)")


# Model 1
weakliem.model1 <- lm(secpay~gini+gdp, data=weakliem2)


# draw 95% CI ellipse
# from library(car)

# confidenceEllipse(weakliem.model1, levels=0.95,Scheffe=TRUE)
car::dataEllipse(gini, secpay, levels=0.95, lty=1,
                 main = "Nondemocratic countries",
                 xlab="Gini", ylab="Attitudes towards inequality(mean)")

# adding regression line
abline(weakliem.model1, lwd=2, lty=1, col=1)


# "identify" remove 'slovakia'(49) and 'CzechRepublic'(25) as outliers.
# Model 2
weakliem.model2 <- update(weakliem.model1, subset=-c(25,49))
abline(weakliem.model2, lwd=2, lty=2, col=2)

# On-time locator (no need to use)
legend(locator(1), lty=1:2, col=1:2,
       legend=c('All cases', 'Outliers excluded'))

library(xtable)
print(xtable(weakliem.model1))
print(xtable(weakliem.model2))


# ---- Ex 2: Davis DATA from car --- #
# These data are the Davis data in the car package
library(car)
data(Davis)

attach(Davis)

davis.model.1 <- lm(repwt~weight, data=Davis)
model1 <- lm(weight ~ height, data=Davis)

plot(height, weight, main = "Davis data")


# on-time identify outlier (click on the graph)
identify(height, weight, row.names(Davis))

abline(model1, lty=1, col=1, lwd=3)

# remove outlier from the data then rerun LR model
model2 <- update(model1, subset=-12)

abline(model2, lty=2, col=2, lwd=3)

# On-time locator (no need to use)
legend(locator(1), lty=1:2, col=1:2, lwd=3,
       legend=c('All cases', 'Outliers excluded'))


# --------- Types of Unusual Observations ---- #
# ----- 1. Regression Outliers -----
# A regression outlier is an observation that has an
# unusual value of the dependent variable Y,
# conditional on its value of the independent
# variable X.

# ----- 2. Cases with Leverage -----
# An observation that has an unusual X value-i.e., it is far from
# the mean of X-has leverage on (i.e., the potential to influence)
# the regression line

# The further away from from the mean of X (either in a positive or
# negative direction), the more leverage an observation has on the
# regression fit.

# ----- 3. Influential Observations -----
# Only when an observation has high leverage and is an
# outlier in terms of Y-value will it strongly influence
# the regression line
# - In other words, it must have an unusual X-value
# with an unusual Y-value given its X-value
# In such cases both the intercept and slope are affected,
# as the line chases the observation

#  Influence=Leverage X Discrepancy


## ------------------------------------------ ##
## ----------- High leverage -----------------##
# Assessing Leverage: Hat Values (1)
# Most common measure of leverage is the hat-value
# ---- R script for plot of Hat Values --- #
plot(hatvalues(weakliem.model1), main="Hat Values for Ineqaulity model")

# Draw Hat value cutoff.
abline(h=c(2,3)*3/length(secpay), lty=2)
# "h" signifies horizontal line
# the average hat value = (k+1)/n
# a Rule of thumb is that 2*average hat value
# for small samples should be examined
# These cases have high leverage, but not necessarily high influence.
graphics::identify(1:length(secpay),
         hatvalues(weakliem.model1),
         row.names(weakliem2))

## --------------------------------------------- ##
# Formal Tests for Outliers: Standardized Residuals
## --------------------------------------------- ##

# high leverage observations can
# have small residuals because they pull the line towards them.

out1 <- outlierTest(weakliem.model1)
(out1)
weakliem2$country[49]


## --------------------------------------------- ##
# ----------- Quantile Comparison Plots --------- #
## --------------------------------------------- ##
# compare the distribution of the studentized residuals from our
# regression model to the t-distribution
# Observations that stray outside of the 95% confidence
# envelope are statistically significant outliers
# simulate = TRUE specifies a bootstrap
# 95% confidence envelope
car::qqPlot(weakliem.model1, simulate=TRUE,
       labels = weakliem2$country,
       envelope=.95)


## --------------------------------------------- ##
## Influential Observations: DFBeta and DFBetas  ##
## --------------------------------------------- ##
## an influential observation is one that
#  combines discrepancy with leverage.
weakliem.dfbetas <- dfbetas(weakliem.model1)

# weakliem.dfbetas
# unclass(weakliem.dfbetas)

# c(2,3) specifies the coefficients of interests
plot(weakliem.dfbetas[,c(2,3)],
     xlim = c(-2,2),ylim=c(-1,1),
     main="DFBetas for the Gini and GDP coefficients")

# adds the rule of thumb cut-off line
abline(h=2/sqrt(length(weakliem2)), lty=2)
# identify influential points
graphics::identify(weakliem.dfbetas[,2], weakliem.dfbetas[,3],
         weakliem2$country)

#  A problem with DFBetas is that each observation has several measures of
# influence-one for each coefficient n(k+1) different measures


## -------------------------------------------------------- ##
#       ----------      Cook's D   ---------                ##
## -------------------------------------------------------- ##
# Cook's D overcomes the problem by presenting a single summary
# measure for each observation
plot(cooks.distance(weakliem.model1))
abline(h=4/length(weakliem2), lty=2)
graphics::identify(1:dim(weakliem2)[1], cooks.distance(weakliem.model1),
         weakliem2$country)


## -------------------------------------------------------- ##
#       ----------     Influence Plot   ---------           ##
## -------------------------------------------------------- ##
# Influence plot displays studentized residuals, hat-values
# and Cook's D on a single plot
plot(hatvalues(weakliem.model1),
     rstudent(weakliem.model1), ylim=c(-3,6), type="n",
     main="Influence Plot for Inequality data",
     xlab="Hat Values", ylab="Studentized Residuals")

cook <- sqrt(cooks.distance(weakliem.model1))
points(hatvalues(weakliem.model1),
       rstudent(weakliem.model1), cex=10*cook/max(cook))

# line for hatvalues.
abline(v=3/25, lty=2)

# lines for studentized residuals
abline(h=c(-2,0,2), lty=2)
graphics::identify(hatvalues(weakliem.model1), rstudent(weakliem.model1),
         weakliem2$country)


## Joint Influence

## -------------------------------------------------------- ##
# ----------     Partial Regression plots   ---------       ##
## -------------------------------------------------------- ##
# Added variable plots(Partial regression plot)
# This allows you to choose the variables interactively
car::avPlots(weakliem.model1)

# This method you choose the variable of interest
car::leveragePlot(weakliem.model1, 'gini')

# ------ Unusual Observations and their impact on Standard Errors --- #

# An observation with high leverage (i.e., an X-value far from the mean of X)
# increases the size of the denominator, and thus decreases the standard error

# A regression outlier (i.e., a point with a large residual) that does
# not have leverage (i.e., it does not have an unusual X-value)
# does not change the slope coefficients but will increase the standard error

# --- EOF --- #
