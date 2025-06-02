## Stat 301-5 Prediction Problem - Regression
# 26: final fitting ----

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# load data subsamples
load(here("data/reg_train_v2.rda"))
load(here("data/reg_test_v2.rda"))

# load results objects
load(here("submissions/submission-2/results/tune_bt.rda"))

# setting seed
set.seed(676)

# bt1: 25 ----

# finalize workflow 

bt1_25 <- tune_bt_lassovars %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model25", .metric == "mae")

wflow_bt1_25 <- tune_bt_lassovars %>%  
  extract_workflow() %>% 
  finalize_workflow(bt1_25)

# fitting final model 

# fitting rf model
bt1_25_fit <- fit(wflow_bt1_25, reg_train_v2)

# predict function
bt1_25_predict <- predict(bt1_25_fit, new_data = reg_test_v2)

bt1_25_predict <- bt1_25_predict %>% 
  bind_cols(reg_test_v2) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt1_25_predict, file = here("submissions/submission-2/pred/bt1_25_predict.csv"))