## Setup
set.seed(88)
library(lavaan)
library(tidyverse)

## Baseline model used in testing:
model <- " visual =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6 "

## Dataset to test and play around with:
data <- HolzingerSwineford1939

## Sourcing the functions
source("Methods/mod_adj_mi.R")
source("Methods/modindices_cv.R")
source("Methods/mod_adj_mi_cv.R")
source("Methods/modindices_train.R")
source("Methods/mod_adj_chisq_cv.R")


# Testing mod_adj_mi()
mod_adj_mi(baseline.model = model, min.mi = 10, data = HolzingerSwineford1939)

# Testing mod_adj_mi_cv()
mod_adj_mi_cv(baseline.model = model, data = HolzingerSwineford1939, k = 10, min.mi = 10)

