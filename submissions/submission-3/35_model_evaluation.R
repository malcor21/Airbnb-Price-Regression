## Stat 301-3 Prediction Problem - Regression
# 35: model eval ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load result objects ----
paths <- list.files(
  path = here("submissions/submission-3/results/"), 
  full.names = TRUE,
  pattern = "tune"
)
for (path in paths){
  load(path)
}

tune_results <-
  as_workflow_set(
    all = tune_bt_lassovars,
    lassovars = tune_bt_lassovars_true
  )

tune_results %>% 
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  arrange(desc(mean)) %>%
  select(-c(preproc, .estimator, n)) %>% 
  view()

tune_results %>% 
  autoplot(metric = "mae") +
  aes(color = wflow_id)

# bt all analysis ----
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

# bt lassovars analysis ----
tune_bt_lassovars_true %>% 
  autoplot(metric = "mae")

tune_bt_lassovars_true %>% 
  select_best(metric = "mae")