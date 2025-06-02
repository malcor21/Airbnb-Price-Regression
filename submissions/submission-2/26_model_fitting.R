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
load(here("data/class_train_v2.rda"))
load(here("data/class_test_v2.rda"))

# load results objects
load(here("submissions/submission-2/results/"))

# setting seed
set.seed(00500001)

# bt1: 096 ----

# finalize workflow 

bt1_096 <- tune_bt %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model096", .metric == "mae")

wflow_bt1_096 <- tune_bt %>%  
  extract_workflow() %>% 
  finalize_workflow(bt1_096)

# fitting final model 

# fitting rf model
bt1_096_fit <- fit(wflow_bt1_096, reg_train)

# predict function
bt1_096_predict <- predict(bt1_096_fit, new_data = reg_test)

bt1_096_predict <- bt1_096_predict %>% 
  bind_cols(reg_test) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt1_096_predict, file = here("submissions/submission-1/pred/bt1_096_predict.csv"))