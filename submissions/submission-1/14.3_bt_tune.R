## Stat 301-3 Prediction Problem - regification
# 143: bt tune ----

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
bt_spec <-
  boost_tree(
    mtry = tune(),
    min_n = tune(),
    trees = tune(),
    learn_rate = tune()
  ) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# define workflows ----
bt_wflow <- 
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(recipe_tree)

# hyperparameter tuning values ----

# setting parameter values
bt_params <- extract_parameter_set_dials(bt_spec) %>% 
  update(
    mtry = mtry(c(1, 25)), #27 predictors
    min_n = min_n(),
    trees = trees(c(250, 1000)),
    learn_rate = learn_rate(c(-5, -0.2))
  )

# define grid
bt_grid <- grid_regular(
  bt_params,
  levels = c(mtry = 5, min_n = 4, trees = 4, learn_rate = 5)
) # fine keeping this regular, since I to emphasize mtry and learn_rate

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s1_bt") # start clock

# tuning code in here
tune_bt <- bt_wflow %>% 
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
  tune_bt, # bundle tuning object and time
  tictoc_bt,
  file = here("submissions/submission-1/results/tune_bt.rda")
)
