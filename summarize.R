library(dplyr)
library(arrow)
library(tidyverse)
library(tidyr)
library(lubridate)

df <- ds %>%
  select(
    CUSTOMER_NUMBER,
    usage_date,
    hour_ending,
    day_type,
    hourly_kwh,
    is_complete_hour
  ) %>%
  filter(is_complete_hour == 1) %>%
  mutate(
    hourly_kwh = as.numeric(hourly_kwh),
    hour = as.integer(hour_ending) %% 24,
    month = lubridate::month(as.Date(usage_date)),
    is_peak = if_else(
      day_type == "Weekday" &
      (
        (
          month %in% c(12, 1, 2) &
          (
            (hour_ending > 7 & hour_ending <= 10) |
            (hour_ending > 17 & hour_ending <= 22)
          )
        ) |
        (
          month %in% 6:8 &
          hour_ending > 11 &
          hour_ending <= 19
        )
      ),
      1L, 0L,
      missing = 0L
    )
  )

overall_features <- df %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    mean_kwh = mean(hourly_kwh),
    max_kwh = max(hourly_kwh),
    min_kwh = min(hourly_kwh),
    n_hours = n(),

    total_usage_kwh = sum(hourly_kwh, na.rm = TRUE),

    peak_usage_kwh = sum(
      if_else(is_peak == 1, hourly_kwh, 0),
      na.rm = TRUE
    ),

    n_peak_hours = sum(is_peak)
  ) %>%
  collect() %>%
  mutate(
    load_factor = if_else(max_kwh > 0, mean_kwh / max_kwh, NA_real_),
    peak_usage_ratio = if_else(
      total_usage_kwh > 0,
      peak_usage_kwh / total_usage_kwh,
      NA_real_
    )
  )

hourly_summary <- df %>%
  group_by(CUSTOMER_NUMBER, hour) %>%
  summarize(
    avg_kwh = mean(hourly_kwh),
    n_hours = n()
  ) %>%
  collect() %>%
  mutate(
    total_kwh = avg_kwh * n_hours
  )

daily_features <- hourly_summary %>%
  mutate(
    night_usage = if_else(
      hour %in% c(22, 23, 0:5),
      total_kwh, 0
    ),
    evening_usage = if_else(
      hour %in% 17:21,
      total_kwh, 0
    ),
    morning_usage = if_else(
      hour %in% 6:10,
      total_kwh, 0
    )
  ) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    total_kwh = sum(total_kwh, na.rm = TRUE),
    night_kwh = sum(night_usage, na.rm = TRUE),
    evening_kwh = sum(evening_usage, na.rm = TRUE),
    morning_kwh = sum(morning_usage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    night_ratio = if_else(total_kwh > 0, night_kwh / total_kwh, NA_real_),
    evening_ratio = if_else(total_kwh > 0, evening_kwh / total_kwh, NA_real_),
    morning_ratio = if_else(total_kwh > 0, morning_kwh / total_kwh, NA_real_)
  )

hourly_features <- hourly_summary %>%
  select(CUSTOMER_NUMBER, hour, avg_kwh) %>%
  pivot_wider(
    names_from = hour,
    values_from = avg_kwh,
    names_prefix = "hour_"
  )

monthly_summary <- df %>%
  group_by(CUSTOMER_NUMBER, month) %>%
  summarize(
    avg_kwh = mean(hourly_kwh),
    peak_kwh = max(hourly_kwh),
    n_hours = n()
  ) %>%
  collect() %>%
  mutate(
    season = case_when(
      month %in% c(6, 7, 8) ~ "summer",
      month %in% c(12, 1, 2) ~ "winter",
      TRUE ~ "other"
    )
  )

seasonal_features <- monthly_summary %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    summer_avg = if (any(season == "summer"))
      weighted.mean(avg_kwh[season == "summer"],
                    n_hours[season == "summer"]) else NA_real_,

    winter_avg = if (any(season == "winter"))
      weighted.mean(avg_kwh[season == "winter"],
                    n_hours[season == "winter"]) else NA_real_,

    summer_peak = if (any(season == "summer"))
      max(peak_kwh[season == "summer"]) else NA_real_,

    winter_peak = if (any(season == "winter"))
      max(peak_kwh[season == "winter"]) else NA_real_,

    .groups = "drop"
  ) %>%
  mutate(
    summer_winter_ratio = if_else(
      winter_avg > 0,
      summer_avg / winter_avg,
      NA_real_
    )
  )

install.packages("duckdb")
library(duckdb)
library(DBI)

con <- dbConnect(duckdb())

duckdb::duckdb_register_arrow(con, "ami_data", ds)

customer_features <- overall_features %>%
  left_join(daily_features, by = "CUSTOMER_NUMBER") %>%
  left_join(hourly_features, by = "CUSTOMER_NUMBER") %>%
  left_join(seasonal_features, by = "CUSTOMER_NUMBER")

# Variable Descriptions

# CUSTOMER_NUMBER: Identifying number for customer
# mean_kwh: Average electricity consumption (kwh) per hour	
# max_kwh: Highest electricity consumption in any recorded hour	
# min_kwh: Lowest electricity consumption in any recorded hour	
# n_hours: Number of hourly observations
# load_factor: Average hourly consumption divided by maximum hourly consumption	(low = occasional spikes, high = consistent usage)

# total_kwh: Total electricity usage across all recorded hours
# night_kwh: Total electricity usage from 10 PM to 5 AM
# evening_kwh: Total electricity usage from 5 PM to 9 PM
# morning_kwh: Total electricity usage from 6 AM to 10 AM
# night_ratio: Fraction of total electricity used overnight
# evening_ratio: Fraction of total electricity used during evening hours
# morning_ratio: Fraction of total electricity used during morning hours

# hour_x: average consumption in the hour ending at x

# summer_avg: Average hourly consumption during June–August
# winter_avg: Average hourly consumption during December–February
# summer_peak: Maximum hourly consumption during summer
# winter_peak: Maximum hourly consumption during winter
# summer_winter_ratio: Summer average divided by winter average

# add in labelled data
library(ggplot2)

ev_customers <- read_csv("data/EV_Heat Pump_Solar PV Customers(EV Customers).csv")
hp_customers <- read_csv("data/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
pv_customers <- read_csv("data/EV_Heat Pump_Solar PV Customers(PV Customers).csv")

ev_ids <- ev_customers %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(known_ev = 1L)

hp_ids <- hp_customers %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(known_hp = 1L)

pv_ids <- pv_customers %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(known_pv = 1L)

customer_labeled <- customer_features %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  left_join(ev_ids, by = "CUSTOMER_NUMBER") %>%
  left_join(hp_ids, by = "CUSTOMER_NUMBER") %>%
  left_join(pv_ids, by = "CUSTOMER_NUMBER") %>%
  mutate(
    across(
      c(known_ev, known_hp, known_pv),
      ~ replace_na(.x, 0L)
    )
  )

# consumers with greatest propertion during peak hours
temp = customer_features %>% select(CUSTOMER_NUMBER, peak_usage_kwh, n_peak_hours, peak_usage_ratio) %>% arrange(desc(peak_usage_ratio)) %>% filter(peak_usage_ratio >= quantile(peak_usage_ratio, 0.75, na.rm = TRUE), na.rm = TRUE)
head(temp, 20)
write.csv(temp["CUSTOMER_NUMBER"], "data/cust_high_prop_peak.csv", row.names = FALSE)

# TOP USERS DURING PEAK HOURS
library(dplyr)
library(lubridate)

# Filter to weekday peak hours
peak_ds <- ds %>%
  mutate(
    month = as.integer(substr(usage_date, 6, 7)),
    hourly_kwh = as.numeric(hourly_kwh)
  ) %>%
  filter(
    day_type == "Weekday",
    (
      month %in% c(12, 1, 2) &
      ((hour_ending > 7 & hour_ending <= 10) |
       (hour_ending > 17 & hour_ending <= 22))
    ) |
    (
      month %in% 6:8 &
      hour_ending > 11 & hour_ending <= 19
    )
  ) %>%
  select(-month)

# Rank customers by total peak usage
top_customers <- peak_ds %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarise(total_peak_kwh = sum(hourly_kwh, na.rm = TRUE)) %>%
  collect() %>%
  slice_max(order_by = total_peak_kwh, prop = 0.25, with_ties = FALSE) %>%
  arrange(desc(total_peak_kwh))


# Return all peak-hour rows for top customers, preserving original columns
top_peak_rows <- peak_ds %>%
  filter(CUSTOMER_NUMBER %in% top_customers$CUSTOMER_NUMBER) %>%
  collect() %>%
  left_join(top_customers, by = "CUSTOMER_NUMBER") %>%
  arrange(desc(total_peak_kwh), CUSTOMER_NUMBER, usage_date, hour_ending)

top_customers
write.csv(top_customers, "data/top_customers.csv", row.names = FALSE)
top_peak_rows

# TOP USERS OVERALL
# Top 25% of customers by total usage across all hours
top_overall_customers <- ds %>%
  mutate(hourly_kwh = as.numeric(hourly_kwh)) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarise(total_kwh = sum(hourly_kwh, na.rm = TRUE)) %>%
  collect() %>%
  slice_max(order_by = total_kwh, prop = 0.25, with_ties = FALSE) %>%
  arrange(desc(total_kwh))

# Return all rows and original columns for these customers
top_overall_rows <- ds %>%
  filter(CUSTOMER_NUMBER %in% top_overall_customers$CUSTOMER_NUMBER) %>%
  collect() %>%
  left_join(top_overall_customers, by = "CUSTOMER_NUMBER") %>%
  arrange(desc(total_kwh), CUSTOMER_NUMBER, usage_date, hour_ending)

top_overall_customers
write.csv(top_overall_customers["CUSTOMER_NUMBER"], "data/top_overall_customers.csv", row.names = FALSE)
top_overall_rows
