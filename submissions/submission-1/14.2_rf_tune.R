## Stat 301-3 Prediction Problem - regification
# 142: rf tune ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(future)
library(tictoc)

# handle common conflicts
tidymodels_prefer()

# load folds
load(here("submissions/submission-1/data/reg_folds.rda"))

# load recipe
load(here("submissions/submission-1/recipes/recipe_tree.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# model specification ----
rf_spec <-
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 750 # arbitrary but probably sufficient
  ) %>%
  set_engine('ranger') %>%
  set_mode('regression')

# define workflows ----
rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(recipe_tree)

# hyperparameter tuning values ----

# setting parameter values
rf_params <- extract_parameter_set_dials(rf_spec) %>% 
  update(
    mtry = mtry(c(1, 25)), #27 predictors
    min_n = min_n()
  )

# define grid
rf_grid <- grid_random(
  rf_params,
  levels = 20
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s1_rf") # start clock

# tuning code in here
tune_rf <- rf_wflow %>% 
  tune_grid(
    resamples = reg_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_rf <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_rf, # bundle tuning object and time
  tictoc_rf,
  file = here("submissions/submission-1/results/tune_rf.rda")
)
