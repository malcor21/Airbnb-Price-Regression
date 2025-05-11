## Stat 301-3 Prediction Problem - Regression
# 01: EDA ----

# Copied from class EDA for convenience ----

# load packages
library(tidyverse)
library(here)
library(tidymodels)
library(naniar)

tidymodels_prefer()

# load data
load(here("data/class_train.rda"))

# set seed
set.seed(1354)

# conducting split for EDA ----
class_split <- class_train %>% 
  initial_split(
    prop = 0.8, 
    strata = host_is_superhost
  )

class_eda <- class_split %>% training()

class_eda %>% 
  glimpse()

# examining potential correlations
class_eda %>% 
  select(
    host_response_rate, host_acceptance_rate, 
    host_listings_count, host_total_listings_count, number_of_reviews,
    review_scores_rating, calculated_host_listings_count
  ) %>% 
  cor(use = "pairwise.complete.obs")
# host_listings_count, calculated_host_listings_count, and host_total_listings_count overlap

# listings count variables
class_eda %>% 
  ggplot(aes(x = host_listings_count, y = host_total_listings_count)) +
  geom_point()
# seems useful to capture host_total_listings_count, which has more variation

# host_location
class_eda %>% 
  count(host_location) %>% 
  arrange(desc(n))

# host_location
class_eda %>% 
  count(host_neighbourhood) %>% 
  arrange(desc(n))

# neighbourhood_cleansed
class_eda %>% 
  count(neighbourhood_cleansed) %>% 
  arrange(desc(n))

# neighbourhood_cleansed
class_eda %>% 
  count(room_type) %>% 
  arrange(desc(n))

# examining potential correlations - availability
class_eda %>% 
  count(availability_30, availability_60, availability_90, availability_365) %>% 
  arrange(desc(n))

class_eda %>% 
  select(
    availability_30, availability_60, availability_90, availability_365
  ) %>% 
  cor(use = "pairwise.complete.obs")

# bedrooms and beds
class_eda %>%
  select(bedrooms, beds) %>% 
  cor(use = "pairwise.complete.obs")

# has_availability
class_eda %>% 
  count(has_availability)

# checking missingness
class_eda %>% 
  vis_miss()
# description, host_location, host_about, host_neighbourhood -> don't care
# impute: response_rate, acceptance_rate, bedrooms, beds

# host_response_time
class_eda %>% 
  count(host_response_time)

# host_identity_verified
class_eda %>% 
  count(host_identity_verified)

# checking selected variable distributions
class_eda %>% 
  ggplot(aes(x = host_total_listings_count)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = host_acceptance_rate)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = latitude)) +
  geom_density()


class_eda %>% 
  ggplot(aes(x = longitude)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = minimum_maximum_nights)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = minimum_minimum_nights)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = accommodates)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = beds)) +
  geom_density()

class_eda %>% 
  ggplot(aes(x = minimum_nights_avg_ntm)) +
  geom_density()

class_eda %>% view()