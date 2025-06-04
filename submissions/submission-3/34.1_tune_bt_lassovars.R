## Stat 301-3 Prediction Problem - Regression
# 34: bt tune lassovars ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(future)
library(tictoc)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# load folds
load(here("submissions/submission-3/data/reg_folds.rda"))

# load recipe
load(here("submissions/submission-3/recipes/recipe_tree_lassovars.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# set seed
set.seed(709)

# model specification ----
bt_spec <-
  boost_tree(
    mtry = tune(),
    min_n = tune(),
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine('lightgbm') %>%
  set_mode('regression')

# define workflows ----
bt_wflow <- 
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(recipe_tree_lassovars)

# hyperparameter tuning values ----

# setting parameter values
bt_params <- extract_parameter_set_dials(bt_spec) %>% 
  update(
    mtry = mtry(c(2, 30)), 
    min_n = min_n(c(2, 20)),
    trees = trees(c(1200, 2800)),
    learn_rate = learn_rate(c(-2.5, -0.5)), # fixing this based on all vars result
    loss_reduction = loss_reduction(c(-15, -3)),
    tree_depth = tree_depth(c(4, 20))
  )

# define grid
bt_grid <- grid_space_filling(
  bt_params,
  size = 25
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s3_bt_lassovars") # start clock

# tuning code in here
tune_bt_lassovars_true <- bt_wflow %>%  # name was a mistake, will fix next time
  tune_grid(
    resamples = reg_folds,
    grid = bt_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_bt <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_bt_lassovars_true, # bundle tuning object and time
  tictoc_bt,
  file = here("submissions/submission-3/results/tune_bt_lassovars.rda")
)
