library(arrow)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)

pq <- list.files("cache_clean", pattern = "\\.parquet$", full.names = TRUE)
length(pq)  
# Open the dataset
ds <- open_dataset(pq, format = "parquet")
cat(sprintf("Dataset ready: %d rows x %d cols\n", nrow(ds), ncol(ds)))

# join with predicted customer labels
pred_labels = read_csv("customer_characteristics.csv")
pred_labels = pred_labels %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER))
joined <- ds %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  left_join(pred_labels, by = "CUSTOMER_NUMBER")

# Preview
joined %>%
  head(10) %>%
  collect() %>%
  print(., width = Inf)

# data preparation
set.seed(123)

sample <- joined %>%
  distinct(CUSTOMER_NUMBER) %>%
  collect() %>%
  slice_sample(n = 500)

model_data = joined %>%
  filter(CUSTOMER_NUMBER %in% sample$CUSTOMER_NUMBER) %>% collect()

model_data = model_data %>%
  mutate(year = substr(usage_date, 0, 4),
         month = substr(usage_date, 6, 7),
         date = substr(usage_date, 9, 10)) %>%
  select(-usage_date, -interval_count, -interval_type, -is_complete_hour)

# model building
# simple regression 
glm = glm(as.numeric(hourly_kwh) ~ as.numeric(hour_ending) + temp + humidity + windspeed + is_solar + lvl1EV + lvl2EV + heatpump + AC + no_electric_temp_control + month + day_type + as.numeric(date), data = model_data)
summary(glm)

# plotting residuals
model_data <- model_data %>%
  mutate(
    predicted_kwh = as.numeric(
      predict(glm, newdata = model_data)
    ),
    residual = as.numeric(hourly_kwh) - predicted_kwh
  ) %>%
  filter(!is.na(residual))

residual_hourly <- model_data %>%
  group_by(hour_ending) %>%
  summarize(
    mean_residual = median(residual),
    .groups = "drop"
  )

ggplot(residual_hourly,
       aes(x = as.numeric(hour_ending),
           y = mean_residual)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Median Residual Load Curve",
    x = "Hour Ending",
    y = "Mean Residual (kWh)"
  ) +
  theme_minimal()

customer_hourly <- model_data %>%
  group_by(CUSTOMER_NUMBER, hour_ending) %>%
  summarize(
    mean_residual = mean(residual),
    .groups = "drop"
  )

set.seed(123)

sample_ids <- customer_hourly %>%
  distinct(CUSTOMER_NUMBER) %>%
  slice_sample(n = 30) %>%
  pull(CUSTOMER_NUMBER)

customer_hourly %>%
  filter(CUSTOMER_NUMBER %in% sample_ids) %>%
  ggplot(aes(
    x = as.numeric(hour_ending),
    y = mean_residual,
    group = CUSTOMER_NUMBER
  )) +
  geom_line(alpha = 0.35, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Individual Customer Residual Load Curves",
    x = "Hour Ending",
    y = "Mean Residual (kWh)"
  ) +
  theme_minimal()

# model with interactions
model2 <- lm(
  as.numeric(hourly_kwh) ~
    temp + humidity + windspeed +
    factor(month) + day_type +
    factor(hour_ending) +
    is_solar * factor(hour_ending) +
    lvl1EV * factor(hour_ending) +
    lvl2EV * factor(hour_ending)  +
    heatpump * temp +
    AC * temp  + 
    high_overall_use +
    peak_demand_contributor,
  data = model_data
)
summary(model2)

# plotting residuals
model_data <- model_data %>%
  mutate(
    predicted_kwh = as.numeric(
      predict(model2, newdata = model_data)
    ),
    res2 = as.numeric(hourly_kwh) - predicted_kwh
  ) %>%
  filter(!is.na(res2))

residual_hourly <- model_data %>%
  group_by(hour_ending) %>%
  summarize(
    mean_res2 = median(res2),
    .groups = "drop"
  )

ggplot(residual_hourly,
       aes(x = as.numeric(hour_ending),
           y = mean_res2)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Median Residual Load Curve",
    x = "Hour Ending",
    y = "Mean Residual (kWh)"
  ) +
  theme_minimal()

customer_hourly <- model_data %>%
  group_by(CUSTOMER_NUMBER, hour_ending) %>%
  summarize(
    mean_res2 = mean(res2),
    .groups = "drop"
  )

set.seed(123)

sample_ids <- customer_hourly %>%
  distinct(CUSTOMER_NUMBER) %>%
  slice_sample(n = 30) %>%
  pull(CUSTOMER_NUMBER)

customer_hourly %>%
  filter(CUSTOMER_NUMBER %in% sample_ids) %>%
  ggplot(aes(
    x = as.numeric(hour_ending),
    y = mean_res2,
    group = CUSTOMER_NUMBER
  )) +
  geom_line(alpha = 0.35, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Individual Customer Residual Load Curves",
    x = "Hour Ending",
    y = "Mean Residual (kWh)"
  ) +
  theme_minimal()



### model 2 plotting with median resid



customer_hourly <- model_data %>%
  group_by(CUSTOMER_NUMBER, hour_ending) %>%
  summarize(
    mean_res2 = mean(res2, na.rm = TRUE),
    .groups = "drop"
  )

overall_median <- customer_hourly %>%
  group_by(hour_ending) %>%
  summarize(
    median_res2 = median(mean_res2, na.rm = TRUE),
    .groups = "drop"
  )

set.seed(123)
sample_ids <- customer_hourly %>%
  distinct(CUSTOMER_NUMBER) %>%
  slice_sample(n = 30) %>%
  pull(CUSTOMER_NUMBER)


(residual_plot <- customer_hourly %>%
  filter(CUSTOMER_NUMBER %in% sample_ids) %>%
  ggplot(aes(
    x = as.numeric(hour_ending),
    y = mean_res2,
    group = CUSTOMER_NUMBER
  )) +
  geom_line(aes(color = "Sampled Customers (n = 30)"), alpha = 0.35, linewidth = 0.6) +
  geom_line(
    data = overall_median,
    aes(x = as.numeric(hour_ending), y = median_res2, group = 1, color = "Overall Median"),
    linewidth = 1.3
  ) +
  geom_point(
    data = overall_median,
    aes(x = as.numeric(hour_ending), y = median_res2, group = 1, color = "Overall Median"),
    size = 2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(
    name = NULL,
    values = c(
      "Sampled Customers (n = 30)" = "#FE6100",
      "Overall Median" = "#785EF0"
    )
  ) +
  labs(
    title = "Sample of Individual Customer Residual Load Curves with Overall Median",
    x = "Hour of Day",
    y = "Mean Residual (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top"))

ggsave(residual_plot, file = "plots/residual_load_curves.png", width = 10, height = 6, dpi = 300)
