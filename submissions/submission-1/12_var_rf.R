## Stat 301-3 Prediction Problem - Regression
# 12: Variable selection (rf) ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(future)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# create selection resamples/folds ----
set.seed(7)

rf_folds <- reg_train %>% 
  vfold_cv(
    v = 5,
    repeats = 1,
    strata = price_log10
  )

# basic recipe ----
recipe_basic <- recipe(price_log10 ~ ., data = reg_train) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price") %>% 
  step_rm(
    description, host_location, host_about, host_neighbourhood, host_listings_count,
    neighbourhood_cleansed, amenities, calculated_host_listings_count, calculated_host_listings_count_entire_homes,
    calculated_host_listings_count_private_rooms, calculated_host_listings_count_shared_rooms, has_availability,
    bathrooms_text
  ) %>% 
  step_rm(
    contains("review")
  ) %>% 
  step_other(c(host_verifications, property_type, room_type), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors())
# not applying PCA since I don't want to tune at this stage

# check recipe 
# recipe_basic %>% 
#   prep() %>% 
#   bake(new_data = NULL) %>% 
#   glimpse()

# model specifications ----
rf_mod <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 750
  ) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "impurity")

# define workflows ----
rf_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recipe_basic)

# hyperparameter tuning values ----
rf_params <- extract_parameter_set_dials(rf_mod) %>% 
  update(
    mtry = mtry(c(1, 50)), #72 columns, 54 after recipe
    min_n = min_n()
  )

# build tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# fit workflow/model ----
# extract best model (optimal tuning parameters)
rf_tuned <- 
  rf_wflow %>% 
  tune_grid(
    resamples = rf_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

# extract best model (optimal tuning parameters)
optimal_wflow <- extract_workflow(rf_tuned) %>% 
  finalize_workflow(select_best(rf_tuned, metric = "mae"))

# fit best model/results
var_select_fit_rf <- fit(optimal_wflow, reg_train)

# look at vars
var_select_fit_rf %>% 
  vip::vi() %>% 
  slice_head(n = 30)

# write out variable selection results ----
save(var_select_fit_rf, file = here("submissions/submission-1/results/var_select_fit_rf.rda"))