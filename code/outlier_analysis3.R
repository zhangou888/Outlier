#############################################################################
#                                                                          ##
##          Project: Outliers detection in R                               ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer: Oscar Torres-Reyna                                 ##
##          Request Date: 11-03-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This chapter explains the purpose of some of the most    ##
##         commonly used outlier analysis and how to implement them in R   ##
##          http://dss.princeton.edu/training/                             ##
##         The R content presented in this document is mostly based on an  ##
##         early version of Fox, J. and Weisberg, S. (2011) An R Companion to
##  Applied Regression, Second Edition, Sage; and from class notes from the
##  ICPSR’s workshop Introduction to the R Statistical Computing Environment
##  taught by John Fox during the summer of 2010.
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When:                                                          ##
##          Who:                                                           ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers",
              "lmtest","sandwich",
              "EnvStats","mvoutlier")
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

# Datasetis in the following library
library(car)

# If not installed type install.packages("car")
#Type help(Prestige) to access the codebook
help(Prestige)
data(Prestige)

## --- Linear Regression --- ##
# R automatically process the log base 2 of income in the equation.
# log2 computes binary (i.e., base 2) logarithms
reg1 <- lm(prestige ~ education + log2(income) + women,
           data = Prestige)

summary(reg1)

## ----- Linear regression (Heteroskedasticity- robust standard errors)
library(lmtest)   # Testing Linear Regression Models
library(sandwich) # Robust Covariance Matrix Estimators

# Heteroscedasticity-Consistent Covariance Matrix Estimation
# from lib(sandwich)
reg1$robse <- vcovHC(reg1, type = "HC1")

# coeftest is a generic function for performing z and (quasi-) t Wald tests 
# of estimated coefficients.
coeftest(reg1, reg1$robse)

# --- Predicted values/Residuals --- #
# after running the regression
prestige_hat <- fitted(reg1) %>% # predicted values
  as.data.frame()

prestige_resid <- residuals(reg1) %>% # residuals
  as.data.frame()


## --- Dummy regression with no interactions (analysis of covariance, fixed effects)
reg2 <- lm(prestige ~ education + log2(income) + type,
           data = Prestige)

summary(reg2)

# reordering factor variables
Prestige$type <- with(Prestige, factor(type,levels=c("bc","wc","prof")))


## --- Dummy regression with interactions (analysis of covariance, fixed effects)
reg3 <- lm(prestige ~ type*(education + log2(income)),
           data = Prestige)

summary(reg3)

# Other ways to run the same model
reg3a <- lm(prestige ~ education + log2(income) + type +
              log2(income):type + education:type,
           data = Prestige)

summary(reg3a)

reg3b <- lm(prestige ~ education*type + log2(income)*type,
            data = Prestige)

summary(reg3b)

## -------------- Diagnostics for linear regression (residual plots)
library(car)
reg4 <- lm(prestige ~ education + income + type,
           data= Prestige)

# reg2 <- lm(prestige ~ education + log2(income) + type,
           # data= Prestige)

residualPlot(reg4)

# Using 'income' as is.
# Variable 'income' shows some patterns.

# other options:

# Residuals vs fitted only
# get a plot against fitted values only, use the arguments terms = ~ 1
residualPlots(reg4, ~ 1, fitted=TRUE)

# Residuals vs education only
residualPlots(reg4, ~ education, fitted=FALSE)

# What to look for: No patterns, no problems.
# All p's should be non-significant.
# Model OK if residuals have mean=0 and variance = 1 (Fox, 316)
# Tukey test null hypothesis: model is additive.

# ----  Influential variables- added-variables plots.
reg5 <- lm(prestige ~ education + income + type,
           data = Prestige)

# id.n - id most influential observation
# id.cex - font size for id.
avPlots(reg5, id.n=2, id.cex=0.7)

# Graphs outcome vs predictor variables holding the rest constant
# (also called partial regression plots)
# Help identify the effect (or influence) of an observation on the regression
# coefficient of the predictor variable

# ---- Outlier - QQ-Plots
reg6 <- lm(prestige ~ education + income + type,
           data = Prestige)

# # id.n - id most influential observation
# id.n - id observations with high residuals
qqPlot(reg6, id.n=3)



#-------------------------------------##
# ---- Outliers - Bonferonni Test --- ##
#-------------------------------------##
reg7 <- lm(prestige ~ education + income + type,
           data = Prestige)

# null for the Bonferonni adjusted outlier test-the observation is an outlier.
# Here observation related to 'medical.technicians' is an outlier.
outlierTest(reg7)

# --- High leverage (hat) point
reg8 <- lm(prestige ~ education + income + type,
           data = Prestige)

#-------------------------------------##
#------     Cook's Distance      -----##
#-------------------------------------##
# Cook's distance measures how much an observation influences the overall
# model or predicted values

# Studentized residuals are the residuals divided by their estimated
# standard deviation as a way to standardized

# Bonferroni test to identify outliers

#-------------------------------------##
#-----      Hat-points         -------##
#-------------------------------------##
# Hat-points identify influential observations (have a high impact on the
# predictor variables)
influenceIndexPlot(reg8, id.n=3)

# Note: if an observation is an outlier and influential (High leverage) then
# that observation can change the fit of the linear model, it is advisable to
# remove a case(s) type
reg1a <- update(reg8, subset=rownames(Prestige) != "general.managers")
reg1b <- update(reg8,
                subset= !(rownames(Prestige) %in% c("general.managers",
                                                   "medical.technicians")))
influenceIndexPlot(reg1a, id.n=3)
influenceIndexPlot(reg1b, id.n=3)

# --------  Influence Plots -------------- #
reg1 <- lm(prestige ~ education + income + type, data=Prestige)

# Creates a bubble-plot combining the display of Studentized residuals,
# hat-values, and Cook's Distance (represented in circles)
# This function creates a bubble plot of Studentized residuals
# versus hat values, with the areas of the circles representing the
# observations proportional to the value Cook's distance.
# Vertical reference lines are drawn at twice and three times
# the average hat value, horizontal reference lines at -2, 0,
# and 2 on the Studentized-residual scale.
influencePlot(reg1, id.n=3)

# ----- Testing for normality  ------- #
reg1 <- lm(prestige ~ education + income + type, data=Prestige)

qqPlot(reg1)

# look for the tails, points should be close to the line or within the
# confidence intervals.
# Quantile plots compare the studentized residuals vs a t-distribution
# other Tests: shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)


# ------- Testing for heteroskedasticity ------ #
reg1 <- lm(prestige ~ education + income + type, data=Prestige)

# non-constant variance score test
ncvTest(reg1)

# Breush/Pagan and Cook/Weisberg score test for non-constant error variance.
# Null is constant variance, see also residualPlots(reg1)
residualPlots(reg1)

# -----------Testing for multicolinearity ------------- ##
# A gvif > 4 suggests collineartiy.
# when there are strong linear relationships among the predictor in a regression
# analysis, the precision of the estimated regression coefficients in linear
# model declines compared to what it would have been were the predictors uncorrelated
# with each other" (Fox: 359).
vif(reg1)

# --- EOF --- #
































