## Stat 301-3 Prediction Problem - Regression
# 15: model eval ----

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)

# handle common conflicts
tidymodels_prefer()

# load result objects ----
paths <- list.files(
  path = here("submissions/submission-1/results/"), 
  full.names = TRUE,
  pattern = "tune"
)
for (path in paths){
  load(path)
}

# results set ----
tune_results <-
  as_workflow_set(
    lasso = tune_lasso,
    rf = tune_rf,
    bt = tune_bt,
    knn = tune_knn,
    mlp = mlp_tune
  )

tune_results %>% 
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  arrange(desc(mean)) %>%
  select(-c(preproc, .estimator, n)) %>% 
  view()

tune_results %>% 
  autoplot(metric = "mae")

# results table - useful for memo

# times <- bind_rows(
#   tictoc_svm_poly_rec1,
#   tictoc_svm_poly_rec2,
#   tictoc_svm_rbf_rec1,
#   tictoc_svm_rbf_rec2
# ) %>% 
#   select(
#     model, runtime
#   ) %>% 
#   mutate(
#     recipe = str_remove(model, "^[^_]*_")
#   ) %>% 
#   mutate(
#     model = str_remove(model, "_[^_]*$")
#   )
# 
# svm_table <- svm_results %>% 
#   collect_metrics() %>% 
#   filter(.metric == "rmse") %>% 
#   slice_min(mean, by = wflow_id) %>% 
#   arrange(mean) %>% 
#   select(-c(.config, preproc, .metric, .estimator, n)) %>% 
#   rename(recipe = wflow_id) %>% 
#   left_join(times, by = c("model", "recipe")) %>% 
#   mutate(
#     recipe = str_remove(recipe, "^[^_]*_"),
#     std_err = signif(std_err, digits = 3)
#   ) %>% 
#   mutate(
#     recipe = if_else(
#       recipe == "rec2",
#       "Random forest",
#       "Lasso"
#     )
#   ) %>% 
#   relocate(model, recipe, mean, std_err, runtime) %>% 
#   rename(
#     Model = model,
#     Recipe = recipe,
#     `Best RMSE` = mean,
#     Std_err = std_err,
#     Runtime = runtime
#   )
# 
# # saving out results table
# save(svm_table, file = here("results/svm_table.rda"))