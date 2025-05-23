library(tidyverse)
library(modelsummary)
library(dplyr)
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction_with_ML/Machine-Learning/Assignment 2")

data2025 <- read.csv("Prague2025q1_cleaned.csv")
data2024 <- read.csv("Prague2024q4_cleaned.csv")

data <- data2024 %>%
  dplyr::group_by(property_type) %>%
  dplyr::summarise(n = dplyr::n_distinct(X), .groups = "drop")

table(data$property_type)

data2024 <- data2024 %>%
  filter(property_type %in% c("Entire rental unit", "Entire condo", 
                              "Private room in rental unit", "Entire serviced apartment")) %>%
  mutate(
    property_type = recode(property_type,
                           "Entire rental unit" = "Rental Unit",
                           "Entire condo" = "Condo",
                           "Private room in rental unit" = "Private Room",
                           "Entire serviced apartment" = "Serviced Apt"),
    property_type = factor(property_type))

data2025 <- data2025 %>%
  filter(property_type %in% c("Entire rental unit", "Entire condo", 
                              "Private room in rental unit", "Entire serviced apartment")) %>%
  mutate(
    property_type = recode(property_type,
                           "Entire rental unit" = "Rental Unit",
                           "Entire condo" = "Condo",
                           "Private room in rental unit" = "Private Room",
                           "Entire serviced apartment" = "Serviced Apt"),
    property_type = factor(property_type))

data2024 <- data2024 %>%
  mutate(f_room_type = factor(room_type))
data2025 <- data2025 %>%
  mutate(f_room_type = factor(room_type))

data2024$f_room_type2 <- factor(ifelse(data2024$f_room_type== "Entire home/apt", "Entire/Apt",
                                  ifelse(data2024$f_room_type== "Private room", "Private", ".")))

data2025$f_room_type2 <- factor(ifelse(data2025$f_room_type== "Entire home/apt", "Entire/Apt",
                                       ifelse(data2025$f_room_type== "Private room", "Private", ".")))

## Create Numerical variables
data2024 <- data2024 %>%
  mutate(
    Czech_koruna_price_day = price,
    p_host_response_rate = ifelse(host_response_rate == "N/A", -1,
                                  as.numeric(gsub("%", "", host_response_rate))),
    p_host_acceptance_rate = ifelse(host_acceptance_rate == "N/A", -1,
                                    as.numeric(gsub("%", "", host_acceptance_rate)))
  )

numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data2024 <- data2024 %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

data2025 <- data2025 %>%
  mutate(
    Czech_koruna_price_day = price,
    p_host_response_rate = ifelse(host_response_rate == "N/A", -1,
                                  as.numeric(gsub("%", "", host_response_rate))),
    p_host_acceptance_rate = ifelse(host_acceptance_rate == "N/A", -1,
                                    as.numeric(gsub("%", "", host_acceptance_rate)))
  )

numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data2025 <- data2025 %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


#create days since first review
data2024 <- data2024 %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

data2025 <- data2025 %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

data2024 %>%
  filter(price < 20000) %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 250, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Nightly Prices (Below 10,000 CZK)",
       x = "Price (CZK)",
       y = "Number of Listings") +
  theme_minimal()

data2024 <- data2024 %>%
  mutate(ln_price = log(price))
data2024 <- data2024 %>%
  filter(price < 9000)

data2025 %>%
  filter(price < 50000) %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 250, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Nightly Prices (Below 10,000 CZK)",
       x = "Price (CZK)",
       y = "Number of Listings") +
  theme_minimal()

data2025 <- data2025 %>%
  mutate(ln_price = log(price))
data2025 <- data2025 %>%
  filter(price < 7000)

data2024 <- data2024 %>%
  mutate(accommodates2=accommodates^2, 
         ln_accommodates=log(accommodates) ,
         ln_accommodates2=log(accommodates)^2,
         ln_beds = log(beds),
         ln_number_of_reviews = log(number_of_reviews+1)
  )

data2025 <- data2025 %>%
  mutate(accommodates2=accommodates^2, 
         ln_accommodates=log(accommodates) ,
         ln_accommodates2=log(accommodates)^2,
         ln_beds = log(beds),
         ln_number_of_reviews = log(number_of_reviews+1)
  )

# Pool accomodations with 0,1,2,10 bathrooms
data2024 <- data2024 %>%
  mutate(f_bathroom = cut(bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data2024$number_of_reviews_n <- as.numeric(data2024$number_of_reviews_n)
data2024 <- data2024 %>%
  mutate(f_number_of_reviews = cut(
    number_of_reviews_n,
    breaks = c(0, 1, 51, max(number_of_reviews_n, na.rm = TRUE)),
    labels = c(0, 1, 2),
    right = FALSE
  ))

# Pool and categorize the number of minimum nights: 1,2,3, 3+
data2024$minimum_nights <- as.numeric(data2024$minimum_nights)
data2024 <- data2024 %>%
  mutate(f_minimum_nights = cut(
    minimum_nights,
    breaks = c(0, 2, 3, max(minimum_nights, na.rm = TRUE)),
    labels = c(0, 1, 2),
    right = FALSE
  ))

# Change Infinite values with NaNs
for (j in 1:ncol(data2024) ) data.table::set(data2024,
                                             which(is.infinite(data2024[[j]])), j, NA)

data2025 <- data2025 %>%
  mutate(f_bathroom = cut(bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

data2025$number_of_reviews_n <- as.numeric(data2025$number_of_reviews_n)
data2025 <- data2025 %>%
  mutate(f_number_of_reviews = cut(
    number_of_reviews_n,
    breaks = c(0, 1, 51, max(number_of_reviews_n, na.rm = TRUE)),
    labels = c(0, 1, 2),
    right = FALSE
  ))

data2025$minimum_nights <- as.numeric(data2025$minimum_nights)
data2025 <- data2025 %>%
  mutate(f_minimum_nights = cut(
    minimum_nights,
    breaks = c(0, 2, 3, max(minimum_nights, na.rm = TRUE)),
    labels = c(0, 1, 2),
    right = FALSE
  ))

for (j in 1:ncol(data2025) ) data.table::set(data2025,
                                             which(is.infinite(data2025[[j]])), j, NA)

to_filter <- sapply(data2024, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

vars_to_drop <- c(
  "host_is_superhost", "neighbourhood_group_cleansed",
  "calendar_updated", "number_of_reviews",
  "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_checkin", "review_scores_communication", "review_scores_location",
  "review_scores_value", "license", "reviews_per_month",
  "review_scores_rating_n", "reviews_per_month_n", "n_days_since",
  "ln_beds", "f_number_of_reviews"
)
data2024 <- data2024 %>%
  select(-all_of(vars_to_drop))

to_filter <- sapply(data2025, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

data2025 <- data2025 %>%
  select(-all_of(vars_to_drop))

data2024 <- data2024 %>%
  mutate(
    bathrooms =  ifelse(is.na(bathrooms), median(bathrooms, na.rm = T), bathrooms),
    bedrooms =  ifelse(is.na(bedrooms), median(bedrooms, na.rm = T), bedrooms),
    beds_n = ifelse(is.na(beds_n), accommodates, beds_n),
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights)
  )

data2025 <- data2025 %>%
  mutate(
    bathrooms =  ifelse(is.na(bathrooms), median(bathrooms, na.rm = T), bathrooms),
    bedrooms =  ifelse(is.na(bedrooms), median(bedrooms, na.rm = T), bedrooms),
    beds_n = ifelse(is.na(beds_n), accommodates, beds_n),
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights)
  )

data2024 <- na.omit(data2024)
data2025 <- na.omit(data2025)

write.csv(data2024, 'Prague2024q4_final.csv')
write.csv(data2025, 'Prague2025q1_final.csv')
