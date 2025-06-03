## Stat 301-3 Prediction Problem - Regression
# 00.3: submission 3 cleaning ---

# load packages
library(tidyverse)
library(here)

# loading data ----
load(here("data/reg_train_v2.rda"))
load(here("data/reg_test_v2.rda"))

# host_location ----
`%ni%` <- Negate(`%in%`)
native_list <- c("Chicago, IL", "Asheville, NC", "Hawaii, United States")

reg_train_v3 <- reg_train_v2 %>% 
  mutate(
    host_location_foreign = case_when(
      host_location %in% native_list | str_detect(host_location, "HI") ~ "native",
      is.na(host_location) ~ "N_A",
      .default = "foreign"
    )
  ) %>% 
  mutate(host_location_foreign = factor(host_location_foreign)) 
# %>%
#   summarize(
#     median = median(price),
#     .by = host_location_foreign
#     )
# seems useful

reg_train_v3 %>% 
  ggplot(aes(y = price, x = host_location_foreign)) +
  geom_jitter()
# actually not sure, but adding it anyway


# host_neighbourhood ----

# worth creating an NA 
reg_train_v3 <- reg_train_v3 %>% 
  mutate(
    host_neighbourhood_NA = case_when(
      is.na(host_neighbourhood) == 1 ~ 1,
      .default = 0
    )
  ) 
# %>% 
#   ggplot(aes(y = price, x = host_neighbourhood_NA)) +
#   geom_jitter()
# looks useful

# host_verifications ----
reg_train_v3 %>% 
  count(host_verifications)

sum(is.na(reg_train_v3$host_verifications))

reg_train_v3 <- reg_train_v3 %>% 
  mutate(
    number_verifications = str_count(host_verifications, ",") + 1
  )

reg_train_v3 %>% 
  ggplot(aes(y = price, x = number_verifications)) +
  geom_jitter()

reg_train_v3 <- reg_train_v3 %>% 
  mutate(
    number_verifications = factor(number_verifications)
  )

# number of amenities list ----
reg_train_v3 <- reg_train_v3 %>% 
  mutate(
    number_amenities = str_count(amenities, ",") + 1
  )

reg_train_v3 %>% 
  ggplot(aes(y = price, x = number_amenities)) +
  geom_point()

# replicating for test set ----
reg_test_v3 <- reg_test_v2 %>% 
  mutate(
    host_location_foreign = case_when(
      host_location %in% native_list | str_detect(host_location, "HI") ~ "native",
      is.na(host_location) ~ "N_A",
      .default = "foreign"
    )
  ) %>% 
  mutate(host_location_foreign = factor(host_location_foreign)) 

# host_neighbourhood ----

# worth creating an NA 
reg_test_v3 <- reg_test_v3 %>% 
  mutate(
    host_neighbourhood_NA = case_when(
      is.na(host_neighbourhood) == 1 ~ 1,
      .default = 0
    )
  ) 

# host_verifications ----
reg_test_v3 <- reg_test_v3 %>% 
  mutate(
    number_verifications = str_count(host_verifications, ",") + 1
  )

reg_test_v3 <- reg_test_v3 %>% 
  mutate(
    number_verifications = factor(number_verifications)
  )

# number of amenities list ----
reg_test_v3 <- reg_test_v3 %>% 
  mutate(
    number_amenities = str_count(amenities, ",") + 1
  )

# saving v3 datasets ----
save(reg_train_v3, file = here("data/reg_train_v3.rda"))
save(reg_test_v3, file = here("data/reg_test_v3.rda"))