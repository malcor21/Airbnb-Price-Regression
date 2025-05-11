







# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)

# Handle conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)

# load resamples ----
load(here("data/adult_folds.rda"))

# load preprocessing/recipe ----
load(here("recipes/recipe_complex.rda"))

# model specifications ----
nn_complex_spec <- mlp(
  mode = "classification",
  hidden_units = tune(),
  penalty = tune()
) %>% 
  set_engine("nnet")

# define workflow ----
nn_complex_wflow <-
  workflow() %>% 
  add_model(nn_complex_spec) %>% 
  add_recipe(recipe_complex)

# hyperparameter tuning values ----
grid_params <- extract_parameter_set_dials(nn_complex_spec) %>% 
  update(
    hidden_units = hidden_units(),
    penalty = penalty()
  )

nn_complex_grid <- grid_regular(grid_params, levels = 5)

# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("nn_complex") # start clock

# start cluster
cl <- makePSOCKcluster(num_cores - 2)
registerDoParallel(cl)

# tuning code in here
nn_complex_tune <- nn_complex_wflow %>% 
  tune_grid(
    resamples = adult_folds,
    grid = nn_complex_grid,
    control = control_grid(save_workflow = TRUE)
  )

# ending cluster object
stopCluster(cl)

toc(log = TRUE)

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_mlp_complex <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  nn_complex_tune, # bundle tuning object and time
  tictoc_mlp_complex,
  file = here("results/nn_complex_tune.rda")
)