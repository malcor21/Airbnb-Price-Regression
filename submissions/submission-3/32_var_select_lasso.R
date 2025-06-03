## Stat 301-3 Prediction Problem - Regression
# 32: Variable selection (lasso) ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(future)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train_v3.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# create selection resamples/folds ----
lasso_folds <- reg_train_v3 %>% 
  vfold_cv(
    v = 5,
    repeats = 3,
    strata = price_log10
  )

# basic recipe ----
recipe_basic <- recipe(price_log10 ~ ., data = reg_train_v3) %>% 
  update_role(id, new_role = "id") %>% 
  step_rm(price) %>% 
  step_rm(
    description, host_location, host_about, host_neighbourhood, host_listings_count,
    neighbourhood_cleansed, amenities, has_availability, bathrooms_text,
    review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_unknown(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9)

# check recipe 
recipe_basic %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

# model specifications ----
lasso_mod <- 
  linear_reg(
    mixture = 1,
    penalty = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

# define workflows ----
lasso_wflow <- 
  workflow() %>% 
  add_model(lasso_mod) %>% 
  add_recipe(recipe_basic)

# hyperparameter tuning values ----
lasso_params <- extract_parameter_set_dials(lasso_mod) %>% 
  update(
    penalty = penalty()
  )

# build tuning grid
lasso_grid <- grid_space_filling(lasso_params, size = 20)

# fit workflow/model ----
# extract best model (optimal tuning parameters)
lasso_tuned <- 
  lasso_wflow %>% 
  tune_grid(
    resamples = lasso_folds,
    grid = lasso_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(mae, rmse, rsq)
  )

# lasso_tuned %>% 
#   collect_metrics() %>% 
#   filter(.metric == "roc_auc") %>% 
#   view()

# extract best model (optimal tuning parameters)
optimal_wflow <- extract_workflow(lasso_tuned) %>% 
  finalize_workflow(select_best(lasso_tuned, metric = "mae"))

# fit best model/results
var_select_fit_lasso <- fit(optimal_wflow, reg_train_v3)

# look at vars
var_select_fit_lasso %>% tidy() %>% view()

# write out variable selection results ----
save(var_select_fit_lasso, file = here("submissions/submission-2/results/var_select_fit_lasso.rda"))
