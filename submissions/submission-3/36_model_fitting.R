## Stat 301-5 Prediction Problem - Regression
# 36: final fitting ----

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# load data subsamples
load(here("data/reg_train_v3.rda"))
load(here("data/reg_test_v3.rda"))

# load results objects
load(here("submissions/submission-3/results/tune_bt_all.rda"))
load(here("submissions/submission-3/results/tune_bt_lassovars.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# setting seed
set.seed(122)

# adding price column
reg_test_v3 <- reg_test_v3 %>% 
  mutate(price = NA)

# all: bt1: 07 ----

# finalize workflow 

bt1_07_all <- tune_bt_lassovars %>% # still incorrect name
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model07", .metric == "mae")

wflow_bt1_07_all <- tune_bt_lassovars %>%  
  extract_workflow() %>% 
  finalize_workflow(bt1_07_all)

# fitting final model 

# fitting rf model
bt1_07_all_fit <- fit(wflow_bt1_07_all, reg_train_v3)

# predict function
bt1_07_all_predict <- predict(bt1_07_all_fit, new_data = reg_test_v3)

bt1_07_all_predict <- bt1_07_all_predict %>% 
  bind_cols(reg_test_v3) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt1_07_all_predict, file = here("submissions/submission-3/pred/bt1_07_all_predict.csv"))

# lassovars: bt1: 25 ----

# finalize workflow 

bt1_25_lassovars <- tune_bt_lassovars_true %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model25", .metric == "mae")

wflow_bt1_25_lassovars <- tune_bt_lassovars_true %>%  
  extract_workflow() %>% 
  finalize_workflow(bt1_25_lassovars)

# fitting final model 

# fitting rf model
bt1_25_lassovars_fit <- fit(wflow_bt1_25_lassovars, reg_train_v3)

# predict function
bt1_25_lassovars_predict <- predict(bt1_25_lassovars_fit, new_data = reg_test_v3)

bt1_25_lassovars_predict <- bt1_25_lassovars_predict %>% 
  bind_cols(reg_test_v3) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt1_25_lassovars_predict, file = here("submissions/submission-3/pred/bt1_25_lassovars_predict.csv"))

# all: bt2: 17 ----

# finalize workflow 

bt2_17_all <- tune_bt_lassovars %>% # still incorrect name
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model17", .metric == "mae")

wflow_bt2_17_all <- tune_bt_lassovars %>%  
  extract_workflow() %>% 
  finalize_workflow(bt2_17_all)

# fitting final model 

# fitting rf model
bt2_17_all_fit <- fit(wflow_bt2_17_all, reg_train_v3)

# predict function
bt2_17_all_predict <- predict(bt2_17_all_fit, new_data = reg_test_v3)

bt2_17_all_predict <- bt2_17_all_predict %>% 
  bind_cols(reg_test_v3) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt2_17_all_predict, file = here("submissions/submission-3/pred/bt2_17_all_predict.csv"))
