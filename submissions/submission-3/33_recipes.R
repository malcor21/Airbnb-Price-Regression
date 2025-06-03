## Stat 301-3 Prediction Problem - Regression
# 33: Recipes ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train_v3.rda"))

# load variable selection
load(here("submissions/submission-3/results/var_select_fit_lasso.rda"))

# trees recipe (all variables) ----
recipe_tree_all <- recipe(price_log10 ~ ., data = reg_train_v3) %>% 
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
  step_other(all_nominal_predictors(), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_unknown(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors())

recipe_tree_all %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse() #90 vars

# trees recipe lassovars ----
var_select_lasso <- var_select_fit_lasso %>% tidy()

important_vars <- var_select_lasso %>% 
  filter(estimate != 0) %>% 
  pull(term) %>% 
  as_tibble()

# clean important var names to handle factors - undo dummy encoding
important_vars_clean <-  important_vars %>% 
  mutate(
    # see which ones are numeric and which are factor
    # direct match is numeric otherwise factor
    # because factor vars were  renamed to factor_level
    class = if_else(value %in% names(reg_train_v3),
                    "numeric", "factor"),
    # if it is a factor remove the "level"
    name = if_else(class == "factor",
                   # remove everything after the LAST underscore)
                   str_remove(value, "_[^_]*$"),
                   value)
    # note: we did not handle cases where level has underscore
  ) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  pull(name)

# check if any levels did not get handles correctly
setdiff(important_vars_clean, names(reg_train_v3))

# variables to keep
var_keep <- intersect(important_vars_clean, names(reg_train_v3))

recipe_tree_lassovars <- recipe(price_log10 ~ ., data = reg_train_v3) %>% 
  update_role(id, new_role = "id") %>% # other vars already removed during variable select
  step_select(all_of(!!(var_keep)), price_log10, skip = TRUE) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_unknown(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors())

recipe_tree_lassovars %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse() #82 vars

# saving recipes ----
save(recipe_tree_all, file = here("submissions/submission-3/recipes/recipe_tree_all.rda"))
save(recipe_tree_lassovars, file = here("submissions/submission-3/recipes/recipe_tree_lassovars.rda"))
