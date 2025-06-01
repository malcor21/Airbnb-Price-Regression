## Stat 301-3 Prediction Problem - Regression
# 00.2: submission 2 cleaning ---

# load packages
library(tidyverse)
library(here)

# loading data ----
load(here("data/reg_train.rda"))
load(here("data/reg_test.rda"))

# cleaning train set ----

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

# removing old amenities
remove_amenities <- 
  grep("has", names(reg_train_v2), value = TRUE)[-c(1, 2)]

reg_train_v2 <- reg_train_v2 %>% 
  select(-all_of(remove_amenities))

# consolidating shared pool and hot tub
reg_train_v2$amenities[str_detect(reg_train_v2$amenities, regex("shared", ignore_case=TRUE)) &
                         str_detect(reg_train_v2$amenities, regex("pool", ignore_case=TRUE))] <- "Shared pool"

reg_train_v2 %>% 
  pull(amenities) %>% 
  str_split(",\\s*") %>%
  flatten_chr() %>%
  tibble(amenity = .) %>%
  count(amenity, sort = TRUE) %>%
  view()

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
    regex("(?<!Hair\\s)(dryer)", ignore_case = TRUE),
    "Dryer"
  ) %>% 
  str_replace_all(
    regex("(?<!Dish\\s)(washer)", ignore_case = TRUE),
    "Washer"
  ) %>%  # consolidating private pools and hot tubs
  str_remove_all(regex("whirpool", ignore_case = TRUE)) %>% # avoid mislabeling
  str_replace_all(
    regex("(?<!Shared\\s)(pool)(?!\\s*(table|view))", ignore_case = TRUE),
    "Private pool"
  ) %>% 
  str_replace_all(".*[Hh]ot tub*", "Hot tub") %>%
  str_remove_all("u\\d+") %>%
  str_remove_all("\\\\") %>% 
  tibble(amenity = .) %>%
  count(amenity, sort = TRUE) %>%
  view()
  # slice_max(n, n = 26) %>% 
  pull(1)

amenities_list_v2 <- c(
  amenities_list_v2, "Long term stays allowed",
  "Free street parking", "Exterior security cameras on property", "Luggage dropoff allowed",
  "Pool", "Waterfront"
)

# mutating amenities
conflicted::conflicts_prefer(stringr::fixed)

for (amenity in amenities_list_v2) {
  reg_train_v2 <- reg_train_v2 %>%
    mutate(!!str_c("has_amenity_", str_replace_all(amenity, "\\s+", "_")) := as.integer(str_detect(amenities, fixed(amenity))))
}

reg_train_v2 %>% 
  view()