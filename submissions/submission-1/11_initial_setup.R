## Stat 301-3 Prediction Problem - Regression
# 0: Initial setup ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train.rda"))

# set seed
set.seed(191919191)

# Resampling folds ----
reg_folds <- reg_train %>% 
  vfold_cv(v = 5, repeats = 2, strata = price_log10) #reduced repeats for speed

# save datasets

save(
  reg_folds,
  file = here("submissions/submission-1/data/reg_folds.rda")
)
