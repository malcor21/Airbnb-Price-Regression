# Peek at data

# load packages
library(tidyverse)
library(here)

# load data
reg_train <- read.csv(here("data/train.csv"))

reg_train %>% 
  skimr::skim_without_charts()

# price is being treated as character - make numeric
reg_train <- reg_train %>% 
  mutate(price = parse_number(price)) %>% 
  count(price)

# glimpse
reg_train %>% 
  glimpse()
# Want to change bathrooms (remove text), rate (make numeric), id (character)

reg_train <- reg_train %>% 
  mutate(
    id = as.character(id), # if leading zeros, would have had to have read in as number
    host_response_rate = as.numeric(str_remove(host_response_rate, "%")), # this also normalizes NAs
    host_acceptance_rate = as.numeric(str_remove(host_acceptance_rate, "%")) # equivalent handling
  )


# fixing bathrooms
reg_train %>% 
  count(bathrooms_text) %>% 
  view()

# want to try extracting numbers - EXCEPT half-bath
reg_train <- reg_train %>% 
  mutate(
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf-bath", "0.5"),
    num_bathrooms = as.numeric(str_remove_all(bathrooms_text, "[A-z]")) # changes half-bath to just hyphen, so use line above to fix
  )

# checking description values
reg_train %>% 
  count(description) %>% 
  view()

# turn all character to factor
char_to_factor_list <- reg_train %>% 
  select(where(is.character)) %>% 
  select(-id, -description, -host_about, ) %>% # don't want these  to be factor - KEEP CHECKING
  names()

# store as list, apply factor 
reg_train <- reg_train %>% 
  mutate(
    across(
      any_of(char_to_factor_list),
      factor
    )
  )