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
  update_role(price, new_role = "price_og") %>% 
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

# linear recipe ----
recipe_linear <- recipe(price_log10 ~ ., data = reg_train_v2) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price_og") %>% 
  step_rm(
    description, host_location, host_about, host_neighbourhood, host_listings_count,
    neighbourhood_cleansed, amenities, has_availability, bathrooms_text,
    review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_unknown(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ all_numeric_predictors()*all_numeric_predictors()) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = tune())

# knn recipe ----
recipe_knn <- recipe(price_log10 ~ ., data = reg_train_v2) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price_og") %>% 
  step_rm(
    description, host_location, host_about, host_neighbourhood, host_listings_count,
    neighbourhood_cleansed, amenities, has_availability, bathrooms_text,
    review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_unknown(all_nominal_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% 
  step_normalize(all_predictors())

# mlp recipe ----
recipe_mlp <- recipe(price_log10 ~ ., data = reg_train_v2) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price_og") %>% 
  step_rm(
    description, host_location, host_about, host_neighbourhood, host_listings_count,
    neighbourhood_cleansed, amenities, has_availability, bathrooms_text,
    review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
    review_scores_communication, review_scores_location
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% # substantial missingness
  step_impute_median(all_numeric_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
  step_normalize(all_predictors())

# saving recipes ----
save(recipe_tree, file = here("submissions/submission-2/recipes/recipe_tree.rda"))
save(recipe_linear, file = here("submissions/submission-2/recipes/recipe_linear.rda"))
save(recipe_knn, file = here("submissions/submission-2/recipes/recipe_knn.rda"))
save(recipe_mlp, file = here("submissions/submission-2/recipes/recipe_mlp.rda"))
