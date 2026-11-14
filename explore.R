# load data
library(ggplot2)
library(dplyr)
library(tidyr)

# general summary statistics
summary(customer_features)

# variables of interest
variables <- c(
  "mean_kwh",
  "max_kwh",
  "load_factor",
  "night_ratio",
  "evening_ratio",
  "morning_ratio",
  "summer_winter_ratio",
  "summer_avg",
  "winter_avg"
)

# plot histogram of distribution of variables of interest
customer_features %>%
  select(all_of(variables)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Distribution of Customer Electricity Usage Features",
    x = "Value",
    y = "Number of Customers"
  ) +
  theme_minimal()

# plot pairwise distributions
library(GGally)

variables <- c(
  "mean_kwh",
  "load_factor",
  "night_ratio",
  "evening_ratio",
  "summer_winter_ratio"
)

customer_features %>%
  select(all_of(variables)) %>%
  drop_na() %>%
  slice_sample(n = 1000) %>%
  ggpairs()

# extreme consumers consumers 
# look at top 5% for each variable and compare usage curve to remaining 95%
variables <- c(
  "mean_kwh",
  "max_kwh",
  "load_factor",
  "night_ratio",
  "evening_ratio",
  "morning_ratio",
  "summer_winter_ratio",
  "summer_avg",
  "winter_avg"
)
high_customers <- customer_features %>%
  select(CUSTOMER_NUMBER, all_of(variables)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    threshold = quantile(value, 0.95, na.rm = TRUE),
    group = if_else(
      !is.na(value) & value >= threshold,
      "Top 5%",
      "Other 95%"
    )
  ) %>%
  ungroup() %>%
  select(CUSTOMER_NUMBER, variable, group)

group_profiles <- hourly_summary %>%
  inner_join(high_customers, by = "CUSTOMER_NUMBER") %>%
  group_by(variable, group, hour) %>%
  summarize(
    avg_kwh = mean(avg_kwh, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(
  group_profiles,
  aes(x = hour, y = avg_kwh, color = group)
) +
  geom_line(linewidth = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("Top 5%" = "red", "Other 95%" = "gray50")
  ) +
  labs(
    title = "Hourly Load Profiles of High-Usage Customer Groups",
    subtitle = "Top 5% vs. remaining customers for each feature",
    x = "Hour of Day",
    y = "Average Hourly Electricity Usage (kWh)",
    color = "Customer Group"
  ) +
  theme_minimal()

# k means clustering
cluster_data <- customer_features %>%
  select(
    CUSTOMER_NUMBER,
    mean_kwh,
    load_factor,
    night_ratio,
    evening_ratio,
    summer_winter_ratio
  ) %>%
  drop_na()

X <- scale(cluster_data %>% select(-CUSTOMER_NUMBER))

set.seed(42)
model <- kmeans(X, centers = 5, nstart = 25)

cluster_data$cluster <- factor(model$cluster)

cluster_data %>%
  group_by(cluster) %>%
  summarize(
    across(
      -CUSTOMER_NUMBER,
      mean,
      na.rm = TRUE
    ),
    n_customers = n()
  )

ggplot(cluster_data,
       aes(x = night_ratio,
           y = load_factor,
           color = cluster)) +
  geom_point(alpha = 0.4, size = 1) +
  labs(
    title = "Customer Clusters",
    x = "Nighttime Usage Ratio",
    y = "Load Factor",
    color = "Cluster"
  ) +
  theme_minimal()
