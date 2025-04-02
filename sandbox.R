# Peek at data

# load packages
library(tidyverse)
library(here)

# load data
reg_train <- read.csv(here("data/train.csv"))

reg_train %>% 
  skimr::skim_without_charts()

# price is being treated as character
reg_train %>% 
  mutate(price = parse_number(price)) %>% 
  count(price)
