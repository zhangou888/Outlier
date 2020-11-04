#############################################################################
#                                                                          ##
##          Project: Outliers detection in R                               ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer: Anthoine Setwey                                    ##
##          Request Date: 10-21-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This chapter explains the purpose of some of the most    ##
##         commonly used outlier analysis and how to implement them in R   ##
##          https://www.statsandr.com/blog/outliers-detection-in-r/        ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When:                                                          ##
##          Who:                                                           ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","ggplot2","outliers","EnvStats","mvoutlier")
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

## Step 4: Read-in data.

## ------- Descriptive statistics ----------##
# Minimum and maximum
dat <- ggplot2::mpg
summary(dat$hwy)

# Min and max
min(dat$hwy)
max(dat$hwy)


## -------- Histogram  -----------##
# Another basic way to detect outliers is to draw a histogram of the data.

# Using R base (with the number of bins corresponding to
# the square root of the number of observations in order
# to have more bins than the default option):

hist(dat$hwy,xlab = "hwy",
    main = "Histogram of hwy",
    breaks = sqrt(nrow(dat))) # set number of bins

# or using ggplot2:
library(ggplot2)

ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#  From the histogram, there seems to be a couple of observations higher than
# all other observations (see the bar on the right side of the plot).

## ------------- Boxplot  -------------##
# In addition to histograms,
# boxplots are also useful to detect potential outliers.

# Using R base:
boxplot(dat$hwy, ylab = "hwy")

# using ggplot2:
ggplot(dat) + aes(x = "", y = hwy) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

## ----------- IQR Criterion --- ###
# A boxplot helps to visualize a quantitative variable by displaying five common
# location summary (minimum, median, first and third quartiles and maximum) and
# any observation that was classified as a suspected outlier using the
# interquartile range (IQR) criterion.
# The IQR criterion means that all observations above correspond to first
# and third quartile respectively, and IQR is the difference between
# the third and first quartile) are considered as potential outliers by R.
# In other words, all observations outside of the following interval will be
# considered as potential outliers:

# Removing or keeping an outlier depends on
#   (i) the context of your analysis,
#   (ii) whether the tests you are going to perform on the dataset are
#        robust to outliers or not, and
#   (iii) how far is the outlier from other observations.

# It is also possible to extract the values of the potential outliers
# based on the IQR criterion thanks to the boxplot.stats()$out function:

boxplot.stats(dat$hwy)$out

# Thanks to the which() function it is possible to extract the row number
# corresponding to these outliers:
out <- boxplot.stats(dat$hwy)$out
out_ind <- which(dat$hwy %in% c(out))
out_ind

# With this information you can now easily go back to the specific rows
# in the dataset to verify them, or print all variables for these outliers:
dat[out_ind, ]

# It is also possible to print the values of the outliers directly
# on the boxplot with the mtext() function:

boxplot(dat$hwy,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon")

# print out outlier
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


### -------------- Percentiles -------------- ###
# This method of outliers detection is based on the percentiles.
# With the percentiles method, all observations that lie outside the
# interval formed by the 2.5 and 97.5 percentiles will be considered as
# potential outliers. Other percentiles such as the 1 and 99, or the 5
# and 95 percentiles can also be considered to construct the interval.

# The values of the lower and upper percentiles (and thus the lower and upper limits of the interval) can be computed with the quantile() function:
lower_bound <- quantile(dat$hwy, 0.025)
lower_bound

upper_bound <- quantile(dat$hwy, 0.975)
upper_bound

# So the outlier will be
outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
outlier_ind

#  Then their values of highway miles per gallon can be printed:
dat[outlier_ind, "hwy"]

# Alternatively, all variables for these outliers can be printed:
dat[outlier_ind, ]

# There are 11 potential outliers according to the percentiles method.
# To reduce this number, you can set the percentiles to 1 and 99:
lower_bound <- quantile(dat$hwy, 0.01)
upper_bound <- quantile(dat$hwy, 0.99)

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)

# print out outlier data
dat[outlier_ind, ]


## --------- Hampel filter ------ ##
# Another method, known as Hampel filter, consists of considering as outliers
# the values outside the interval (I) formed by the median,
# plus or minus 3 median absolute deviations (MAD):

#                I = [median -  3xMAD; median + 3xMAD]

# where MAD is the median absolute deviation and is defined as
# the median of the absolute deviation from the data.

#                MAD = median([Xi - X_median])

# For this method we first set the interval limits
# We use the median() and mad() functions:
median(dat$hwy)
mad(dat$hwy)

lower_bound <- median(dat$hwy) - 3 * mad(dat$hwy)
lower_bound

upper_bound <- median(dat$hwy) + 3 * mad(dat$hwy)
upper_bound


# According to this method, all observations below 1.761 and above 46.239
#will be considered as potential outliers.
# The row numbers of the observations outside of the interval
# can then be extracted with the which() function:
outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
outlier_ind

# According to the Hampel filter,
# there is no potential outlier for the hwy variable.

##----------------------------------------------------------------##
## ------------      Section 2: Statistical tests      --------   ##
##----------------------------------------------------------------##
# In this section, we present 3 more formal techniques to detect outliers:
#              Grubbs's test
#              Dixon's test
#              Rosner's test

# Note that the 3 tests are appropriate only when the data (without any outliers)
# are approximately normally distributed.
# The normality assumption must thus be verified before applying these
# tests for outliers.

## ----  Grubbs's test ---- ##
# The Grubbs test allows to detect whether the highest or
# lowest value in a dataset is an outlier.

# Note: The Grubbs test detects one outlier at a time (highest or lowest value),
# Note that the Grubbs test is not appropriate for sample size of 6 or less
# (n<=6).
# so the null and alternative hypotheses are as follows:

# H0: The highest value is not an outlier
# H1: The highest value is an outlier
# if we want to test the highest value, or:

# H0: The lowest value is not an outlier
# H1: The lowest value is an outlier
# if we want to test the lowest value.
# install.packages("outliers")
library(outliers)
test <- grubbs.test(dat$hwy)
test

# If you want to do the test for the lowest value, simply
# add the argument opposite = TRUE in the grubbs.test() function:
test <- grubbs.test(dat$hwy, opposite = TRUE)
test

# For the sake of illustration, we will now replace an observation with a more
# extreme value and perform the Grubbs test on this new dataset. Let's replace the
# 34th row with a value of 212:
dat[34, "hwy"] <- 212

test <- grubbs.test(dat$hwy)
test

## ---- Dixon's test ---- ##
## Dixon test is used to test whether a single low or high value is an outlier.
# So if more than one outliers is suspected, the test has to be performed on
# these suspected outliers individually.
# Note that Dixon test is most useful for small sample size (usually n<=25).
# Dixon test can only be done on small sample size (R will throw an error and
# accepts only dataset of 3 to 30 observations):
subdat <- dat[1:20, ]
test <- dixon.test(subdat$hwy)
test

# To test for the highest value, simply add the opposite = TRUE argument
#to the dixon.test() function:
test <- dixon.test(subdat$hwy, opposite = TRUE)
test

## Reminder: It is a good practice to always check
# the results of the statistical test for outliers
# against the boxplot to make sure we tested all potential outliers:

out <- boxplot.stats(subdat$hwy)$out
boxplot(subdat$hwy,
        ylab = "hwy")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# find and exclude lowest value
remove_ind <- which.min(subdat$hwy)
subsubdat <- subdat[-remove_ind, ]

# Dixon test on dataset without the minimum
test <- dixon.test(subsubdat$hwy)
test

## ---- Rosner's test ---- ##
# Rosner's test for outliers has the advantages that:
#  it is used to detect several outliers at once
# (unlike Grubbs and Dixon test which must be performed iteratively
# to screen for multiple outliers), and it is designed to avoid the
# problem of masking, where an outlier that is close in value to
# another outlier can go undetected.
# Unlike Dixon test, note that Rosner test is most appropriate when the
# sample size is large (n >= 20).
# To perform the Rosner test we use the rosnerTest() function
# from the {EnvStats} package.
# This function requires at least 2 arguments:
#   the data and the number of suspected outliers k
# (with k = 3 as the default number of suspected outliers).
library(EnvStats)
test <- rosnerTest(dat$hwy, k = 3)
test

# The interesting results are provided in the $all.stats table:
test$all.stats

## ------- Additional remarks ---- ##
# You will find many other methods to detect outliers:

# in the {outliers} packages,
# via the lofactor() function from the {DMwR} package:
# Local Outlier Factor (LOF) is an algorithm used to
# identify outliers by comparing the local
# density of a point with that of its neighbors,

# the outlierTest() from the {car} package gives the
# most extreme observation based on the given model
# and allows to test whether it is an outlier,

# in the {OutlierDetection} package, and with the aq.plot() function
# from the {mvoutlier} package (Thanks KTR for the suggestion.):
library(mvoutlier)
Y <- as.matrix(ggplot2::mpg[, c("cyl", "hwy")])
res <- aq.plot(Y)

# Note also that some transformations may "naturally" eliminate outliers.
# The natural log or square root of a value reduces the variation caused
# by extreme values, so in some cases applying these transformations
# will eliminate the outliers.

# Enderlein, G. 1987. "Hawkins, Dm: Identification of Outliers.
# Chapman and Hall, London-New York 1980, 188 S.,? 14, 50."
# Biometrical Journal 29 (2): 198-98.


## --- EOF --- ##

