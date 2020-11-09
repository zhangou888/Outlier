#############################################################################
#                                                                          ##
##          Project: r-statistics.co Outlier Analysis                      ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer: Selva Prabhakaran                                  ##
##          Request Date: 09-15-2020                                       ##
##          Initial Code:                                                  ##
##          Goals:                                                         ##
##          Input:                                                         ##
##          Output:                                                        ##
##          Note: This chapter explains the purpose of some of the most    ##
##         commonly used outlier analysis and how to implement them in R   ##
##         http://r-statistics.co/Outlier-Treatment-With-R.html
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When:                                                          ##
##          Who:                                                           ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages
packages <- c("tidyverse","mlbench","mice","Hmisc","DMwR",
              "rpart","mice","outliers")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

library(mlbench)
library(mice)

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/stat ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

## Step 4: Read-in data.

#------------ Section 2 ------------- #
## Outlier Treatment
# Outliers in data can distort predictions and affect the accuracy,
# if you don't detect and handle them appropriately especially.

# in regression models.
## -- Example -- ##
# Inject outliers into data.
# original data
cars1 <- cars[1:30, ]

# introduce outliers.
cars_outliers <- data.frame(speed=c(19,19,20,20,20),
                            dist=c(190, 186, 210, 220, 218))

cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

# Plot of data with outliers.
par(mfrow=c(1, 2))

plot(cars2$speed, cars2$dist,
     xlim=c(0, 28), ylim=c(0, 230),
     main="With Outliers",
     xlab="speed", ylab="dist",
     pch="*", col="red", cex=2)

# regression reference line
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
plot(cars1$speed, cars1$dist,
     xlim=c(0, 28), ylim=c(0, 230),
     main="Outliers removed \n A much better fit!",
     xlab="speed", ylab="dist",
     pch="*", col="red", cex=2)

abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

## ---------- Detect outliers ------ ##
# 1.1 Univariate approach
# Criterion 1: 1.5 IQR
# Continuous Variable-outliers are those observations that lie outside 1.5 * IQR,
# IQR- 'Inter Quartile Range' is the difference between 75th and 25th quartiles.
url <- "https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv"
# alternate source:  https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv
inputData <- read.csv(url)  # import data

# outlier values.
outlier_values <- boxplot.stats(inputData$pressure_height)$out


boxplot(inputData$pressure_height, main="Pressure Height", boxwex=0.1)

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


# 1.2 Bivariate approach
url <- "https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv"
ozone <- read.csv(url)

# For categorical variable
boxplot(ozone_reading ~ Month, data=ozone,
        main="Ozone reading across months")  # clear pattern is noticeable.

# this may not be significant, as day of week variable is a subset of the month var.
boxplot(ozone_reading ~ Day_of_week, data=ozone,
        main="Ozone reading for \n days of week")

# For continuous variable (convert to categorical if needed.)
boxplot(ozone_reading ~ pressure_height, data=ozone,
        main="Boxplot for Pressure \n height (continuos var) vs Ozone")

boxplot(ozone_reading ~ cut(pressure_height,
                            pretty(inputData$pressure_height)),
        data=ozone, main="Boxplot for Pressure \n height (categorial) vs Ozone",
        cex.axis=0.5)

#------ Example 2--------------  #
# Create sample data
Gene_id <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE", "GeneF")
expA    <- c(5.462109, 2.667692, 4.796976, 3.127125, 4.500583, 4.598430)
expB    <- c(5.006181, 4.208152, 4.122660, 3.676322, 4.104575, 4.853717)

# Calculate log base two for results from each experiment
log2_A <- log2(expA)
log2_B <- log2(expB)

# Plot data
plot(log2_A~log2_B)

# Calculate and display the regression line
regression <- lm(log2_A~log2_B)
abline(regression)

# Show regression formula
print(regression)

# Create data frame for sample data
data <- data.frame(Gene_id, expA, expB)

# Calculate residuals
data$residuals <- residuals(regression)

# Choose a threshhold
outlier_threshold <- 0.3

# Print only names of outliers
outliers <- data[ abs(data$residuals) > outlier_threshold, ]
print(outliers$Gene_id)


# =============== 1.3 Multivariate Model Approach =====================#
# 1.3.1 Cooks Distance
# Cook's distance is a measure computed with respect to a given regression model
# and therefore is impacted only by the X variables included in the model.
mod <- lm(ozone_reading ~ ., data=ozone)
cooksd <- cooks.distance(mod)


# In general use, those observations that have a cook's distance greater than 4
# times the mean may be classified as influential. This is not a hard boundary.

# plot cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")

# add cutoff line (4)
abline(h = 4*mean(cooksd, na.rm=T), col="red")

# add labels
text(x=1:length(cooksd)+1, y=cooksd,
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

# find out the influential rows from the original data
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(ozone[influential, ])  # influential observations.



##  --------- 2.1 Outliers Test ------------  ##
car::outlierTest(mod)

set.seed(1234)
y=rnorm(100)
outlier(y)
#> [1] 2.548991
outlier(y,opposite=TRUE)
#> [1] -2.345698
dim(y) <- c(20,5)  # convert it to a matrix
outlier(y)
#> [1] 2.415835 1.102298 1.647817 2.548991 2.121117
outlier(y,opposite=TRUE)

# There are two aspects to the scores() function.

# Compute the normalized scores based on "z", "t", "chisq" etc
# Find out observations that lie beyond a
# given percentile based on a given score.
set.seed(1234)
x = rnorm(10)

# z-scores => (x-mean)/sd
scores(x)  

# chi-sq scores => (x - mean(x))^2/var(x)
scores(x, type="chisq")  
#> [1] 0.68458034 0.44007451 2.17210689 3.88421971 0.66539631  . . .

# t scores
scores(x, type="t")  
scores(x, type="chisq", prob=0.9)    # beyond 90th %ile based on chi-sq
scores(x, type="chisq", prob=0.95)   # beyond 95th %ile
scores(x, type="z", prob=0.95)       # beyond 95th %ile based on z-scores
scores(x, type="t", prob=0.95)       # beyond 95th %ile based on t-scores

# ------------- 3.0 Treating the outliers ----------- #
# 1. Imputation

# 2. Capping
# For missing values that lie outside the 1.5 * IQR limits,
# we could cap it by replacing those observations outside
# the lower limit with the value of 5th percentile and those that
# lie above the upper limit, with the value of 95th percentile.

x <- ozone$pressure_height
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)

# Calculate 1.5 IQR CI
H <- 1.5 * IQR(x, na.rm = T)


x[x < (qnt[1] - H)] <- caps[1] # Lower end replacement
x[x > (qnt[2] + H)] <- caps[2] # higher end replacement

# 3. prediction.



