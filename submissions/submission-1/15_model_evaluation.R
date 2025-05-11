## Stat 301-3 Prediction Problem - Regression
# 15: model eval ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load result objects ----
load(here("submissions/submission-1/results/tune_lasso.rda"))


collect_metrics(tune_lasso) %>% 
  view()
