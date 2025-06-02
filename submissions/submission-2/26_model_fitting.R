## Stat 301-5 Prediction Problem - Regression
# 26: final fitting ----

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(future)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# load data subsamples
load(here("data/class_train_v2.rda"))
load(here("data/class_test_v2.rda"))

# load results objects
load(here("submissions/submission-2/results/"))

# setting seed
set.seed(00500001)