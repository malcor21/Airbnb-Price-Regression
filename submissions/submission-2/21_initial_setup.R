## Stat 301-3 Prediction Problem - Regression
# 21: Initial setup ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train_v2.rda"))

# set seed
set.seed(2828282)

# Resampling folds ----
reg_folds <- reg_train_v2 %>% 
  vfold_cv(v = 5, repeats = 3, strata = price_log10)

# save datasets

save(
  reg_folds,
  file = here("submissions/submission-2/data/reg_folds.rda")
)
