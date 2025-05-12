## Stat 301-3 Prediction Problem - Regression
# 13: Recipes ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(corrplot)

# handle common conflicts
tidymodels_prefer()

# load data
load(here("data/reg_train.rda"))


reg_train %>% 
  glimpse()

# load rf variable selection
load(here("submissions/submission-1/results/var_select_fit_rf.rda"))

# examining selected variables ----
var_select_rf <- var_select_fit_rf %>% 
  vip::vi() %>% 
  slice_max(Importance, n = 23) # this is arbitrary - anything above 10

important_vars <- var_select_rf %>% 
  pull(Variable) %>% 
  as_tibble()

important_vars_clean <- important_vars %>% 
  mutate(
    # see which ones are numeric and which are factor
    # direct match is numeric otherwise factor
    # because factor vars were  renamed to factor_level
    class = if_else(value %in% names(reg_train),
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

# variables to keep
var_keep <- intersect(important_vars_clean, names(reg_train))

reg_train %>% 
  select(all_of(var_keep)) %>% 
  glimpse()

# correlation?
# vars_cor <- reg_train %>% 
#   select(all_of(var_keep)) %>% 
#   select(where(is.numeric)) %>% 
#   cor(use = "pairwise.complete.obs")

# vars_cor %>% 
#   corrplot()

# lasso recipe ----
recipe_lasso <- recipe(price_log10 ~ ., data = reg_train) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price") %>% 
  step_select(all_of(!!(var_keep)), price_log10, skip = TRUE) %>% 
  step_mutate(
    host_since = as.numeric(format(host_since, format = "%Y"))
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(
    host_total_listings_count, host_acceptance_rate, minimum_maximum_nights, minimum_minimum_nights, 
    accommodates, maximum_minimum_nights, beds, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
    num_bathrooms) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_novel(all_nominal_predictors(), new_level = "unknown") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ all_numeric_predictors()*all_numeric_predictors()) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = tune())

# rf/bt recipe ----
recipe_tree <- recipe(price_log10 ~ ., data = reg_train) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price") %>% 
  step_select(all_of(!!(var_keep)), price_log10, skip = TRUE) %>%
  step_mutate(
    host_since = as.numeric(format(host_since, format = "%Y"))
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% # substantial missingness
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(
    host_total_listings_count, host_acceptance_rate, minimum_maximum_nights, minimum_minimum_nights, 
    accommodates, maximum_minimum_nights, beds, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
    num_bathrooms) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_novel(all_nominal_predictors(), new_level = "unknown") %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors())

# recipe_tree %>%
#   prep() %>%
#   bake(new_data = NULL) %>%
#   glimpse()

# knn recipe ----
recipe_knn <- recipe(price_log10 ~ ., data = reg_train) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price") %>% 
  step_select(all_of(!!(var_keep)), price_log10, skip = TRUE) %>% 
  step_mutate(
    host_since = as.numeric(format(host_since, format = "%Y"))
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% # substantial missingness
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(
    host_total_listings_count, host_acceptance_rate, minimum_maximum_nights, minimum_minimum_nights, 
    accommodates, maximum_minimum_nights, beds, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
    num_bathrooms) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_novel(all_nominal_predictors(), new_level = "unknown") %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors())

# mlp ----
recipe_mlp <- recipe(price_log10 ~ ., data = reg_train) %>% 
  update_role(id, new_role = "id") %>% 
  update_role(price, new_role = "price") %>% 
  step_select(all_of(!!(var_keep)), price_log10, skip = TRUE) %>% 
  step_mutate(
    host_since = as.numeric(format(host_since, format = "%Y"))
  ) %>% 
  step_impute_mode(all_nominal_predictors()) %>% # substantial missingness
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(
    host_total_listings_count, host_acceptance_rate, minimum_maximum_nights, minimum_minimum_nights, 
    accommodates, maximum_minimum_nights, beds, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
    num_bathrooms) %>% 
  step_other(c(all_nominal_predictors()), threshold = 0.05) %>% # 10k obs, 2k per fold, say 100 min per fold -> 500 cutoff
  step_novel(all_nominal_predictors(), new_level = "unknown") %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
  step_normalize(all_predictors())

# saving recipes ----
save(recipe_lasso, file = here("submissions/submission-1/recipes/recipe_lasso.rda"))
save(recipe_tree, file = here("submissions/submission-1/recipes/recipe_tree.rda"))
save(recipe_knn, file = here("submissions/submission-1/recipes/recipe_knn.rda"))
save(recipe_mlp, file = here("submissions/submission-1/recipes/recipe_mlp.rda"))