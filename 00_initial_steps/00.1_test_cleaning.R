## Stat 301-3 Prediction Problem - Regression
# 00: Initial cleaning ----

# load packages
library(tidyverse)
library(here)
library(jsonlite)

# load data
reg_test <- read_csv(here("data/test.csv"))

# load amenities list
load(here("data/amenities_list.rda"))

# glimpse
# reg_test %>%
#   glimpse()
# 
# reg_test %>% 
#   view()
# 
# reg_test %>%
#   skimr::skim_without_charts()
# dataset is same as class, but with price column

# # need to change price from currency
# reg_test <- reg_test %>% 
#   mutate(price = parse_number(price))

# examine target var
# reg_test %>% 
#   ggplot(aes(x = price)) +
#   geom_density()

# need to log transform
# reg_test <- reg_test %>% 
#   mutate(
#     price_log10 = log10(price)
#   )

# factorize: listing_location, host_response_time, property_type (+ other), room_type, ----
## host_has_profile_pic, host_identity_verified, has_availability, instant_bookable
char_to_factor_list <- reg_test %>% 
  select(c(listing_location, host_response_time, property_type,
           room_type, host_has_profile_pic, host_identity_verified,
           has_availability, instant_bookable
  )) %>% 
  names()

logical_to_factor_list <- reg_test %>% 
  select(c(host_has_profile_pic, host_identity_verified,
           has_availability, instant_bookable)) %>% 
  names()

reg_test <- reg_test %>% 
  mutate(
    across(
      any_of(logical_to_factor_list),
      ~ as.integer(as.character(.) == "TRUE")
    )
  ) %>% 
  mutate(
    across(
      any_of(char_to_factor_list),
      factor
    )
  )

# change ID to character
reg_test <- reg_test %>% 
  mutate(
    id = as.character(id)
  )

# numerize: host_response_rate, host_acceptance_rate, bathrooms_text ----
reg_test <- reg_test %>% 
  mutate(
    id = as.character(id), 
    host_response_rate = as.numeric(str_remove(host_response_rate, "%")), 
    host_acceptance_rate = as.numeric(str_remove(host_acceptance_rate, "%")) 
  )


# reg_test %>% 
#   count(host_verifications) %>% 
#   view()

## want to try extracting numbers - EXCEPT half-bath
reg_test <- reg_test %>% 
  mutate(
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf-bath", "0.5"),
    num_bathrooms = as.numeric(str_remove_all(bathrooms_text, "[A-z]")) # changes half-bath to just hyphen, so use line above to fix
  )

# split into binaries: amenities ----

# fixing reg_test formatting
reg_test <- reg_test %>%
  mutate(amenities = map_chr(amenities, ~ paste(fromJSON(.x), collapse = ", "))) %>% 
  mutate(amenities = amenities %>% 
           str_replace_all("[Ww]ifi", "Wifi") %>%
           str_replace_all("HDTV", "HDTV") %>%
           str_replace_all("[Ss]hampoo", "Shampoo") %>%
           str_replace_all("[Ss]oap", "Body soap") %>%
           str_replace_all("[Oo]ven", "Oven") %>% 
           str_remove_all("u\\d+") %>%
           str_remove_all("\\\\")
  )

# mutating amenities
conflicted::conflicts_prefer(stringr::fixed)

for (amenity in amenities_list) {
  reg_test <- reg_test %>%
    mutate(!!str_c("has_", str_replace_all(amenity, "\\s+", "_")) := as.integer(str_detect(amenities, fixed(amenity))))
}

# save file ----
save(reg_test, file = here("data/reg_test.rda"))