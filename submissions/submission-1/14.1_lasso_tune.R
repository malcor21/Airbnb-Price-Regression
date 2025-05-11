## Stat 301-3 Prediction Problem - Reg
# 141: Lasso tune ----

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
load(here("submissions/submission-1/recipes/recipe_lasso.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# model specification ----
lasso_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_engine('glmnet') %>%
  set_mode('regression')

# define workflows ----
lasso_wflow <- 
  workflow() %>% 
  add_model(lasso_spec) %>% 
  add_recipe(recipe_lasso)

# hyperparameter tuning values ----

# setting parameter values
lasso_params <- extract_parameter_set_dials(lasso_spec) %>% 
  update(
    penalty = penalty(),
    mixture = mixture()
  )

recipe_params <- extract_parameter_set_dials(recipe_lasso) %>% 
  update(
    num_comp = num_comp(c(2, 10))
  )

lasso_params <- lasso_params %>% 
  bind_rows(recipe_params)

# define grid
lasso_grid <- grid_random(
  lasso_params,
  size = 20
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s1_lasso") # start clock

# tuning code in here
tune_lasso <- lasso_wflow %>% 
  tune_grid(
    resamples = reg_folds,
    grid = lasso_grid,
    control = control_grid(save_workflow = TRUE),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_lasso <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_lasso, # bundle tuning object and time
  tictoc_lasso,
  file = here("submissions/submission-1/results/tune_lasso.rda")
)

# assuming that warning messages are a factor of the random grid's tuning (seem
# to occur for high penalties). Not going to change anything for now
