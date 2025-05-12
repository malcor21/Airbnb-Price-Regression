## Stat 301-3 Prediction Problem - Reg
# 16: final fitting ----

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(future)

# handle common conflicts
tidymodels_prefer()

# load data subsamples
load(here("data/reg_train.rda"))
load(here("data/reg_test.rda"))

# adding price column to reg_test
reg_test <- reg_test %>% 
  mutate(price = NA)

# load rf tuned
load(here("submissions/submission-1/results/tune_rf.rda"))

# load bt tuned
load(here("submissions/submission-1/results/tune_bt.rda"))

# load knn tuned
load(here("submissions/submission-1/results/tune_knn.rda"))

# setting seed
set.seed(401)

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

# bt2: 176 ----

# finalize workflow 

bt2_176 <- tune_bt %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model176", .metric == "mae")

wflow_bt2_176 <- tune_bt %>%  
  extract_workflow() %>% 
  finalize_workflow(bt2_176)

# fitting final model 

# fitting rf model
bt2_176_fit <- fit(wflow_bt2_176, reg_train)

# predict function
bt2_176_predict <- predict(bt2_176_fit, new_data = reg_test)

bt2_176_predict <- bt2_176_predict %>% 
  bind_cols(reg_test) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(bt2_176_predict, file = here("submissions/submission-1/pred/bt2_176_predict.csv"))

# rf1: 11 ----

# finalize workflow 

rf1_11 <- tune_rf %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model11", .metric == "mae")

wflow_rf1_11 <- tune_rf %>%  
  extract_workflow() %>% 
  finalize_workflow(rf1_11)

# fitting final model 

# fitting bt model
rf1_11_fit <- fit(wflow_rf1_11, reg_train)

# predict function
rf1_11_predict <- predict(rf1_11_fit, new_data = reg_test)

rf1_11_predict <- rf1_11_predict %>% 
  bind_cols(reg_test) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(rf1_11_predict, file = here("submissions/submission-1/pred/rf1_11_predict.csv"))

# knn1: 6 ----

# finalize workflow 

knn1_6 <- tune_knn %>% 
  collect_metrics() %>% 
  filter(.config == "Preprocessor1_Model6", .metric == "mae")

wflow_knn1_6 <- tune_knn %>%  
  extract_workflow() %>% 
  finalize_workflow(knn1_6)

# fitting final model 

# fitting bt model
knn1_6_fit <- fit(wflow_knn1_6, reg_train)

# predict function
knn1_6_predict <- predict(knn1_6_fit, new_data = reg_test)

knn1_6_predict <- knn1_6_predict %>% 
  bind_cols(reg_test) %>% 
  select(id, .pred) %>% 
  rename(predicted = .pred) %>% 
  mutate(
    predicted = round(10^predicted, digits = 0)
  )

# saving predict object
write_csv(knn1_6_predict, file = here("submissions/submission-1/pred/knn1_6_predict.csv"))
