# logistic regression on solar PV customers
d = customer_labeled %>%
 select(-known_ev, -known_hp, -CUSTOMER_NUMBER)
solar_log = glm(known_pv ~ ., data = d, family = binomial)
summary(solar_log)
data.frame(preds = predict(solar_log, newdata = d, type = 'response'), true = d$known_pv) %>% filter(true == 1)


# analyzing EV distribution
library(ggforce)

# select relevant data (remove other labels)
plot_data <- customer_labeled %>%
 filter(known_ev == 1) %>%
 select(-known_ev, -known_hp, -known_pv, -CUSTOMER_NUMBER) %>%
 select(where(is.numeric)) %>%
 pivot_longer(
   cols = everything(),
   names_to = "variable",
   values_to = "value"
 )


n_pages <- ceiling(n_distinct(plot_data$variable) / 6)

# plot feature distributions for known EV users
for (i in seq_len(n_pages)) {


 p <- ggplot(plot_data, aes(x = value)) +
   geom_histogram(
     bins = 40,
     fill = "steelblue",
     color = "white",
     na.rm = TRUE
   ) +
   facet_wrap_paginate(
     ~ variable,
     scales = "free",
     ncol = 2,
     nrow = 3,
     page = i
   ) +
   labs(
     title = paste("Feature Distributions for Known EV Owners — Page", i),
     x = "Value",
     y = "Number of Customers"
   ) +
   theme_minimal()


 print(p)
}

# making predictions on unlabeled data (results has everything )
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(42)

# 1. Select numeric predictors
# Exclude labels, customer ID, and any non-feature columns

feature_cols <- customer_labeled %>%
  select(
    -CUSTOMER_NUMBER,
    -known_ev,
    -known_hp,
    -known_pv
  ) %>%
  select(where(is.numeric)) %>%
  names()

# 2. Split known EV owners into training and testing

known_ev_ids <- customer_labeled %>%
  filter(known_ev == 1) %>%
  distinct(CUSTOMER_NUMBER) %>%
  pull(CUSTOMER_NUMBER)

train_ev_ids <- sample(
  known_ev_ids,
  size = floor(0.8 * length(known_ev_ids))
)

# 3. Calculate feature intervals using training EV owners only

ev_intervals <- customer_labeled %>%
  filter(CUSTOMER_NUMBER %in% train_ev_ids) %>%
  select(all_of(feature_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarize(
    lower = quantile(value, 0.05, na.rm = TRUE),
    upper = quantile(value, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(lower), is.finite(upper))

# View the typical EV ranges
print(ev_intervals, n = Inf)

# 4. Check each customer's features against EV intervals

customer_matches <- customer_labeled %>%
  select(
    CUSTOMER_NUMBER,
    known_ev,
    all_of(feature_cols)
  ) %>%
  pivot_longer(
    cols = all_of(feature_cols),
    names_to = "variable",
    values_to = "value"
  ) %>%
  inner_join(ev_intervals, by = "variable") %>%
  mutate(
    within_interval = if_else(
      is.na(value),
      NA_integer_,
      as.integer(value >= lower & value <= upper)
    )
  )

# 5. Calculate each customer's match percentage

customer_predictions <- customer_matches %>%
  group_by(CUSTOMER_NUMBER, known_ev) %>%
  summarize(
    features_matched = sum(within_interval, na.rm = TRUE),
    features_available = sum(!is.na(within_interval)),
    match_score = if_else(
      features_available > 0,
      features_matched / features_available,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    predicted_ev = if_else(
      is.na(match_score),
      NA_integer_,
      as.integer(match_score >= 0.70)
    ),
    evaluation_group = case_when(
      CUSTOMER_NUMBER %in% train_ev_ids ~ "Training EV",
      known_ev == 1 ~ "Held-out EV",
      TRUE ~ "Unlabeled"
    )
  )

# 6. Return predicted values alongside known labels

results <- customer_predictions %>%
  select(
    CUSTOMER_NUMBER,
    known_ev,
    predicted_ev,
    match_score,
    features_matched,
    features_available,
    evaluation_group
  ) %>%
  arrange(desc(match_score))

print(results, n = 30, width = Inf)

# 7. Compare predictions with held-out known EV owners

heldout_results <- results %>%
  filter(evaluation_group == "Held-out EV")

table(
  Actual = heldout_results$known_ev,
  Predicted = heldout_results$predicted_ev,
  useNA = "ifany"
)

# 8. Calculate recall on held-out EV owners
recall <- heldout_results %>%
  summarize(
    recall = mean(predicted_ev == 1, na.rm = TRUE)
  )


print(recall)

# 9. Compare match-score distributions

ggplot(
  results,
  aes(x = match_score, fill = evaluation_group)
) +
  geom_histogram(
    bins = 30,
    alpha = 0.5,
    position = "identity"
  ) +
  geom_vline(
    xintercept = 0.70,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "EV Feature Match Scores",
    x = "Proportion of Features Within EV Reference Intervals",
    y = "Number of Customers",
    fill = "Customer Group"
  ) +
  theme_minimal()
