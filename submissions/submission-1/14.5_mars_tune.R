







# L05 Feature Engineering II ----
# Tuning for kitchen sink MARS model

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)
library(tictoc)
library(doParallel)

# Handle conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)

# load resamples ----
load(here("data/wildfires_folds.rda"))

# load preprocessing/recipe ----
load("recipes/recipe_fe.rda")

# model specifications ----
mars_fe_spec <-
  mars(
    mode = "classification",
    num_terms = tune(),
    prod_degree = tune()
  ) %>% 
  set_engine("earth")

# define workflows ----
mars_fe_wflow <- 
  workflow() %>% 
  add_model(mars_fe_spec) %>% 
  add_recipe(recipe_fe)

# hyperparameter tuning values ----

# setting parameter values
mars_params <- extract_parameter_set_dials(mars_fe_spec) %>% 
  update(
    num_terms = num_terms(c(1, 5)),
    prod_degree = prod_degree()
  )

fe_params <- extract_parameter_set_dials(recipe_fe) %>% 
  update(
    num_comp = num_comp(c(2, 10))
  )

mars_params <- mars_params %>% 
  bind_rows(fe_params)

# define grid
mars_grid <- grid_regular(
  mars_params,
  levels = c(num_terms = 5, prod_degree = 2, num_comp = 5)
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("mars_fe") # start clock

# start cluster
cl <- makePSOCKcluster(num_cores - 2)
registerDoParallel(cl)

# tuning code in here
tune_mars_fe <- mars_fe_wflow %>% 
  tune_grid(
    resamples = wildfires_folds,
    grid = mars_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(accuracy, roc_auc)
  )

# ending cluster object
stopCluster(cl)

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_mars_fe <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_mars_fe, # bundle tuning object and time
  tictoc_mars_fe,
  file = here("results/tune_mars_fe.rda")
)