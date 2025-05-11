## Stat 301-3 Prediction Problem - regification
# 145: mlp tune ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)
library(future)

# Handle conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# load resamples ----
load(here("submissions/submission-1/data/reg_folds.rda"))

# load preprocessing/recipe ----
load(here("submissions/submission-1/recipes/recipe_mlp.rda"))

# model specifications ----
mlp_spec <- mlp(
  mode = "regression",
  hidden_units = tune(),
  penalty = tune()
) %>% 
  set_engine("nnet")

# define workflow ----
mlp_wflow <-
  workflow() %>% 
  add_model(mlp_spec) %>% 
  add_recipe(recipe_mlp)

# hyperparameter tuning values ----
grid_params <- extract_parameter_set_dials(mlp_spec) %>% 
  update(
    hidden_units = hidden_units(),
    penalty = penalty()
  )

mlp_grid <- grid_random(grid_params, size = 20)

# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("s1_mlp") # start clock

# tuning code in here
mlp_tune <- mlp_wflow %>% 
  tune_grid(
    resamples = reg_folds,
    grid = mlp_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE)

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_mlp <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  mlp_tune, # bundle tuning object and time
  tictoc_mlp,
  file = here("submissions/submission-1/results/mlp_tune.rda")
)