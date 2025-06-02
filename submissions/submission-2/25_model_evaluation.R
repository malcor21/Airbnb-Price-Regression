## Stat 301-3 Prediction Problem - Regression
# 25: model eval ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load result objects ----
paths <- list.files(
  path = here("submissions/submission-2/results/"), 
  full.names = TRUE,
  pattern = "tune"
)
for (path in paths){
  load(path)
}