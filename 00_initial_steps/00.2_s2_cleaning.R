## Stat 301-3 Prediction Problem - Regression
# 00.2: submission 2 cleaning ---

# load packages
library(tidyverse)
library(here)
library(jsonlite)

# loading data ----
reg_train <- read_csv(here("data/train.csv"))
reg_test <- read_csv(here("data/test.csv"))

# cleaning train set ----

# replicating 00_cleaning except amenities
reg_train <- reg_train %>% 
  mutate(price = parse_number(price))

reg_train <- reg_train %>% 
  mutate(
    price_log10 = log10(price)
  )

char_to_factor_list <- reg_train %>% 
  select(c(listing_location, host_response_time, property_type,
           room_type, host_has_profile_pic, host_identity_verified,
           has_availability, instant_bookable
  )) %>% 
  names()

logical_to_factor_list <- reg_train %>% 
  select(c(host_has_profile_pic, host_identity_verified,
           has_availability, instant_bookable)) %>% 
  names()

reg_train <- reg_train %>% 
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

reg_train <- reg_train %>% 
  mutate(
    id = as.character(id)
  )

reg_train <- reg_train %>% 
  mutate(
    id = as.character(id), 
    host_response_rate = as.numeric(str_remove(host_response_rate, "%")), 
    host_acceptance_rate = as.numeric(str_remove(host_acceptance_rate, "%")) 
  )

reg_train <- reg_train %>% 
  mutate(
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf-bath", "0.5"),
    num_bathrooms = as.numeric(str_remove_all(bathrooms_text, "[A-z]")) # changes half-bath to just hyphen, so use line above to fix
  )

# cleaning NAs
reg_train_v2 <- reg_train %>% 
  mutate(
    across(where(~ is.character(.)),
           ~ na_if(., "NA") %>%
             na_if("") %>% 
             na_if("N/A")
    )
  )

# remove few outliers
reg_train_v2 <- reg_train_v2 %>% 
  filter(minimum_maximum_nights < 1126)

# extracting years
reg_train_v2 <- reg_train_v2 %>% 
  mutate(
    host_since = as.numeric(format(host_since, format = "%Y")),
    first_review = as.numeric(format(first_review, format = "%Y")),
    last_review = as.numeric(format(last_review, format = "%Y"))
  )

# extracting desc info
reg_train_v2 <- reg_train_v2 %>% 
  mutate(
    airbnb_in_about = if_else(str_detect(host_about, "[Aa]ir[Bb][Nn][Bb]"), 1, 0)
  )

reg_train_v2 <- reg_train_v2 %>% 
  mutate(
    condo = if_else(str_detect(description, "[Cc]ondo"), 1, 0)
  )

# cutting review_score vars
cut_review_scores <- function(df) {
  df %>%
    mutate(across(
      starts_with("review_scores"),
      ~ factor(ifelse(. >= 4.75, "high", "low")),
      .names = "{.col}_level"
    ))
}

reg_train_v2 <- reg_train_v2 %>% 
  cut_review_scores()

# numerizing factors
reg_train_v2 <- reg_train_v2 %>% 
  mutate(
    host_identity_verified = as.numeric(host_identity_verified) - 1,
    instant_bookable = as.numeric(instant_bookable) - 1,
    has_availability = as.numeric(has_availability) - 1,
    host_has_profile_pic = as.numeric(host_has_profile_pic) - 1
  )

# adding more amenities ----

# fixing reg_train formatting
reg_train_v2 <- reg_train_v2 %>%
  mutate(amenities = map_chr(amenities, ~ paste(fromJSON(.x), collapse = ", "))) %>% 
  mutate(amenities = amenities %>% 
           str_replace_all("[Ww]ifi", "Wifi") %>%
           str_replace_all("HDTV", "HDTV") %>%
           str_replace_all("[Ss]hampoo", "Shampoo") %>%
           str_replace_all("[Ss]oap", "Body soap") %>%
           str_replace_all("[Oo]ven", "Oven") %>%
           str_replace_all(
             regex("\\b(?<!Hair\\s)dryer\\b", ignore_case = TRUE),
             "Clothes_Dryer"
           ) %>% 
           str_replace_all(
             regex("\\b(?<!Dish\\s)washer\\b", ignore_case = TRUE),
             "Clothes_Washer"
           ) %>%  
           str_remove_all(regex("whirpool", ignore_case = TRUE)) %>% 
           str_replace_all("[Pp]ool", "Pool") %>% 
           str_replace_all("[Hh]ot tub", "Hot tub") %>%
           str_remove_all("u\\d+") %>%
           str_remove_all("\\\\")
  )

amenities_list_v2 <- reg_train_v2 %>%
  pull(amenities) %>%
  str_split(",\\s*") %>%
  flatten_chr() %>%
  str_replace_all(".*[Ww]ifi.*", "Wifi") %>%
  str_replace_all(".*HDTV.*", "HDTV") %>%
  str_replace_all(".*[Ss]hampoo.*", "Shampoo") %>%
  str_replace_all(".*[Ss]oap.*", "Body soap") %>%
  str_replace_all(".*[Oo]ven.*", "Oven") %>%
  str_replace_all(
    regex("^(?!.*Hair\\s+dryer)(?=.*\\bdryer\\b).*", ignore_case = TRUE),
    "Clothes_Dryer"
  ) %>% 
  str_replace_all(
    regex("^(?!.*Dish\\s+washer)(?=.*\\bwasher\\b).*", ignore_case = TRUE),
    "Clothes_Washer"
  ) %>%  # consolidating pools and hot tubs
  str_remove_all(regex("whirpool", ignore_case = TRUE)) %>% # avoid mislabeling
  str_replace_all("[Pp]ool", "Pool") %>% 
  str_replace_all(".*[Hh]ot tub.*", "Hot tub") %>%
  str_remove_all("u\\d+") %>%
  str_remove_all("\\\\") %>% 
  tibble(amenity = .) %>%
  count(amenity, sort = TRUE) %>% 
  slice_max(n, n = 30) %>% 
  pull(1)

amenities_list_v2 <- c(
  amenities_list_v2, "Exterior security cameras on property", "Luggage dropoff allowed",
  "Pool", "Waterfront", "Hot tub", "Clothes_Washer", "Clothes_Dryer"
)

# mutating amenities
conflicted::conflicts_prefer(stringr::fixed)

for (amenity in amenities_list_v2) {
  reg_train_v2 <- reg_train_v2 %>%
    mutate(!!str_c("has_amenity_", str_replace_all(amenity, "\\s+", "_")) := as.integer(str_detect(amenities, fixed(amenity))))
}

save(reg_train_v2, file = here("data/reg_train_v2.rda"))

# replicating for reg_test_v2 ----

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

reg_test <- reg_test %>% 
  mutate(
    id = as.character(id)
  )

reg_test <- reg_test %>% 
  mutate(
    id = as.character(id), 
    host_response_rate = as.numeric(str_remove(host_response_rate, "%")), 
    host_acceptance_rate = as.numeric(str_remove(host_acceptance_rate, "%")) 
  )

reg_test <- reg_test %>% 
  mutate(
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf-bath", "0.5"),
    num_bathrooms = as.numeric(str_remove_all(bathrooms_text, "[A-z]")) # changes half-bath to just hyphen, so use line above to fix
  )

# cleaning NAs
reg_test_v2 <- reg_test %>% 
  mutate(
    across(where(~ is.character(.)),
           ~ na_if(., "NA") %>%
             na_if("") %>% 
             na_if("N/A")
    )
  )

# extracting years
reg_test_v2 <- reg_test_v2 %>% 
  mutate(
    host_since = as.numeric(format(host_since, format = "%Y")),
    first_review = as.numeric(format(first_review, format = "%Y")),
    last_review = as.numeric(format(last_review, format = "%Y"))
  )

# extracting desc info
reg_test_v2 <- reg_test_v2 %>% 
  mutate(
    airbnb_in_about = if_else(str_detect(host_about, "[Aa]ir[Bb][Nn][Bb]"), 1, 0)
  )

reg_test_v2 <- reg_test_v2 %>% 
  mutate(
    condo = if_else(str_detect(description, "[Cc]ondo"), 1, 0)
  )

# cutting review_score vars
cut_review_scores <- function(df) {
  df %>%
    mutate(across(
      starts_with("review_scores"),
      ~ factor(ifelse(. >= 4.75, "high", "low")),
      .names = "{.col}_level"
    ))
}

reg_test_v2 <- reg_test_v2 %>% 
  cut_review_scores()

# numerizing factors
reg_test_v2 <- reg_test_v2 %>% 
  mutate(
    host_identity_verified = as.numeric(host_identity_verified) - 1,
    instant_bookable = as.numeric(instant_bookable) - 1,
    has_availability = as.numeric(has_availability) - 1,
    host_has_profile_pic = as.numeric(host_has_profile_pic) - 1
  )

# adding more amenities ----

# fixing reg_test formatting
reg_test_v2 <- reg_test_v2 %>%
  mutate(amenities = map_chr(amenities, ~ paste(fromJSON(.x), collapse = ", "))) %>% 
  mutate(amenities = amenities %>% 
           str_replace_all("[Ww]ifi", "Wifi") %>%
           str_replace_all("HDTV", "HDTV") %>%
           str_replace_all("[Ss]hampoo", "Shampoo") %>%
           str_replace_all("[Ss]oap", "Body soap") %>%
           str_replace_all("[Oo]ven", "Oven") %>%
           str_replace_all(
             regex("\\b(?<!Hair\\s)dryer\\b", ignore_case = TRUE),
             "Clothes_Dryer"
           ) %>% 
           str_replace_all(
             regex("\\b(?<!Dish\\s)washer\\b", ignore_case = TRUE),
             "Clothes_Washer"
           ) %>%  
           str_remove_all(regex("whirpool", ignore_case = TRUE)) %>% 
           str_replace_all("[Pp]ool", "Pool") %>% 
           str_replace_all("[Hh]ot tub", "Hot tub") %>%
           str_remove_all("u\\d+") %>%
           str_remove_all("\\\\")
  )

# mutating amenities
conflicted::conflicts_prefer(stringr::fixed)

for (amenity in amenities_list_v2) {
  reg_test_v2 <- reg_test_v2 %>%
    mutate(!!str_c("has_amenity_", str_replace_all(amenity, "\\s+", "_")) := as.integer(str_detect(amenities, fixed(amenity))))
}

save(reg_test_v2, file = here("data/reg_test_v2.rda"))
