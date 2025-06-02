## Stat 301-3 Prediction Problem - Reg - NOT WORKING
# 242: Glm tune ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(future)
library(tictoc)
library(stacks)

# handle common conflicts
tidymodels_prefer()

# load folds
load(here("submissions/submission-2/data/reg_folds.rda"))

# load recipe
load(here("submissions/submission-2/recipes/recipe_linear.rda"))

# parallel processing ----
num_cores <- parallel::detectCores(logical= TRUE)
plan(multisession, workers = num_cores - 2)

# model specification ----
glm_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_engine('glmnet') %>%
  set_mode('regression')

# define workflows ----
glm_wflow <- 
  workflow() %>% 
  add_model(glm_spec) %>% 
  add_recipe(recipe_linear)

# hyperparameter tuning values ----

# setting parameter values
glm_params <- extract_parameter_set_dials(glm_spec) %>% 
  update(
    penalty = penalty(c(-15, -5)),
    mixture = mixture()
  )

recipe_params <- extract_parameter_set_dials(recipe_linear) %>% 
  update(
    num_comp = num_comp(c(4, 12))
  )

glm_params <- glm_params %>% 
  bind_rows(recipe_params)

# define grid
glm_grid <- grid_space_filling(
  glm_params,
  size = 20
)

# fit workflow/model ----

# start timer
tic.clearlog() # clear log
tic("s2_glm") # start clock

# tuning code in here
tune_glm <- glm_wflow %>% 
  tune_grid(
    resamples = reg_folds,
    grid = glm_grid,
    control = control_stack_grid(),
    metrics = metric_set(rmse, rsq, mae)
  )

toc(log = TRUE) # stop clock

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_glm <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----
save(
  tune_glm, # bundle tuning object and time
  tictoc_glm,
  file = here("submissions/submission-2/results/tune_glm.rda")
)