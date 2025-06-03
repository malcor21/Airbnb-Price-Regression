## Stat 301-3 Prediction Problem - Regression
# 23: Recipes ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train_v2.rda"))

# trees recipe ----
recipe_tree <- recipe(price_log10 ~ ., data = reg_train_v2) %>% 
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

recipe_tree %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse() #86 vars

# saving recipes ----
save(recipe_tree, file = here("submissions/submission-2/recipes/recipe_tree.rda"))
