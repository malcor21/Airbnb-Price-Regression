## Stat 301-3 Prediction Problem - regification
# 144: knn tune ----

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
load(here("submissions/submission-1/recipes/recipe_knn.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# model specification ----
knn_spec <-
  nearest_neighbor(
    neighbors = tune()
  ) %>%
  set_engine('kknn') %>%
  set_mode('regression')

# define workflows ----
knn_wflow <- 
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(recipe_knn)

# hyperparameter tuning values ----

# setting parameter values
knn_params <- extract_parameter_set_dials(knn_spec) %>% 
  update(
    neighbors = neighbors()
  )

# define grid
knn_grid <- grid_random(
  knn_params,
  size = 10
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s1_knn") # start clock

# tuning code in here
tune_knn <- knn_wflow %>% 
  tune_grid(
    resamples = reg_folds,
    grid = knn_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_knn <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_knn, # bundle tuning object and time
  tictoc_knn,
  file = here("submissions/submission-1/results/tune_knn.rda")
)
