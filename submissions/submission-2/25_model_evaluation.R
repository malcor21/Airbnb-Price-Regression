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

# bt analysis ----
tune_bt_lassovars %>% 
  autoplot(metric = "mae")
# mtry: not super clear. Try a little larger (2, 30)
# trees: seem to decrease. Go out further (1200, 2400)
# learn_rate: try smaller (-4, -0.5)
# min_n: smaller seems best. (2, 20)
# loss_reduction: smaller is definitely best - try below -10 (-15, -3)
# tree_depth: maybe increasing is best? try (4, 20)

tune_bt_lassovars %>% 
  select_best(metric = "mae")

                     