## Stat 301-3 Prediction Problem - Regression
# 15: model eval ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load result objects ----
paths <- list.files(
  path = here("submissions/submission-1/results/"), 
  full.names = TRUE,
  pattern = "tune"
)
for (path in paths){
  load(path)
}

# results set ----
tune_results <-
  as_workflow_set(
    lasso = tune_lasso,
    rf = tune_rf,
    bt = tune_bt,
    knn = tune_knn,
    mlp = mlp_tune
  )

tune_results %>% 
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  arrange(desc(mean)) %>%
  select(-c(preproc, .estimator, n)) %>% 
  view()

tune_results %>% 
  autoplot(metric = "mae")

# bt analysis ----
tune_bt %>% 
  autoplot(metric = "mae")
# top end of trees consistently performs best - expand range past 1k
# small learn rate is best by far - explore below
# mtry and min_n don't seem to effect performance at all. 
# Maybe a small mtry is best, but not a huge effect

tune_bt %>% 
  collect_metrics %>% 
  filter(.metric == "mae") %>% 
  view()

# rf analysis ----
tune_rf %>% 
  autoplot(metric = "mae")
# high mtry is best - explore above 25
# low min_n is best: narrow around 2

# knn analysis ----
tune_knn %>% 
  autoplot(metric = "mae")
# test greater # neighbors - explore above 10

# mlp analysis ----
mlp_tune %>% 
  autoplot(metric = "mae")
# Greater # components better - test above 10
# greater # hidden units better - test above 10
# smaller amount ouf regularization better - test below -10

# lasso analysis ----
tune_lasso %>% 
  autoplot(metric = "mae")
# low amount of regularization is best - test below -7.5
# not sure on num components, but maybe try above 8
# lasso penalty proportion doesn't seem to do much