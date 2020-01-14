library(semPlot)
library(lavaan)
library(semTools)
library(haven)
library(psych)
library(tidyverse)

set.seed(273)

file.choose()
dataset <- read_spss("C:\\Users\\Gebruiker\\Documents\\Master\\Year 2\\Master Thesis\\ESS Wave 6\\ESS6e02_4.sav")

# Checking the average age:
mean(as.numeric(as.character(dataset$agea)), na.rm = TRUE) # It is indeed 48.3.

# Selecting the necessary variables:
data <- select(dataset, ppltrst, pplfair, pplhlp, sclmeet, inprdsc, sclact, pplahlp, flapppl, rehlppl, contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd, wkvlorg, imbgeco, imueclt, imwbcnt, imdfetn, trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun, stfgov, stfdem, stfedu, stfhlth)

########
# EFA: #
########

# Method 1: https://tkaiser.science/esemR.html
fa.parallel(data)
efa <- fa(data, nfactors = 7, rotate = "geominQ")
fa.diagram(efa)

# Method 2: https://rdrr.io/cran/semTools/man/efaUnrotate.html
unrotated <- efaUnrotate(data = data, nf = 24)
oblqRotate(unrotated, method = "geomin")
# Confused on how to make it efa with geomin rotation strategy.



########
# CFA: #
########
fitCFA <- cfa(model = ModelCFA, data = data)
summary(fitCFA,
        fit.measures = TRUE, # Including model fit measures in the output.
        standardized = TRUE) # Including standardized parameter values in the output.


# Trying with half the dataset:
halfdata <- sample(nrow(data), floor(0.5 * nrow(data))) # Half the data, rounded


# Making training and test set:
train <- data[halfdata,]
test <- data[-halfdata,]

fitCFAhalf <- cfa(model = ModelCFA, data = test, mimic = "mplus")
summary(fitCFAhalf,
        fit.measures = TRUE,
        standardized = TRUE)

#################################
# Higher-order factor analysis: #
#################################
fitHigh <- cfa(model = ModelHigh, data = data, mimic = "mplus", check.gradient = FALSE)
summary(fitHigh,
        fit.measures = TRUE,
        standardized = TRUE)

