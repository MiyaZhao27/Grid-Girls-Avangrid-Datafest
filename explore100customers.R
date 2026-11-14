# explore sample of 100 customers
library(arrow)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)

pq <- list.files("cache_clean", pattern = "\\.parquet$", full.names = TRUE)
length(pq)  
# Open the dataset
ds <- open_dataset(pq, format = "parquet")
cat(sprintf("Dataset ready: %d rows x %d cols\n", nrow(ds), ncol(ds)))
ev_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(EV Customers).csv")
heatpump_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
solar_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(PV Customers).csv")
set.seed(42)
sample_ids <- ds %>%
  distinct(CUSTOMER_NUMBER) %>%
  collect() %>%
  pull(CUSTOMER_NUMBER) %>%
  sample(size = 50)

df <- ds %>%
  filter(CUSTOMER_NUMBER %in% sample_ids) %>%
  collect()

# Inspect column names of the CSVs to verify the ID column is named CUSTOMER_NUMBER
# (If it is named e.g. "Customer.Number", rename it here before proceeding)
# names(ev_customers); names(heatpump_customers); names(solar_customers)

# 2. Extract ID vectors and standardize types to match Arrow's CUSTOMER_NUMBER schema
arrow_id_type <- ds$schema$CUSTOMER_NUMBER$type$ToString()
cat("Arrow CUSTOMER_NUMBER schema type:", arrow_id_type, "\n")

cast_id <- function(x) {
  if (grepl("int|double|float", arrow_id_type, ignore.case = TRUE)) {
    as.numeric(trimws(as.character(x)))
  } else {
    trimws(as.character(x))
  }
}

ids_sample   <- unique(cast_id(sample_ids))
ids_ev       <- unique(cast_id(ev_customers$CUSTOMER_NUMBER))
ids_heatpump <- unique(cast_id(heatpump_customers$CUSTOMER_NUMBER))
ids_solar    <- unique(cast_id(solar_customers$CUSTOMER_NUMBER))

# 3. Check for overlaps across the four customer groups
cat("\n--- Overlap Checks ---\n")
cat("Sample & EV overlap:       ", length(intersect(ids_sample, ids_ev)), "\n")
cat("Sample & Heat Pump overlap:", length(intersect(ids_sample, ids_heatpump)), "\n")
cat("Sample & Solar overlap:    ", length(intersect(ids_sample, ids_solar)), "\n")
cat("EV & Heat Pump overlap:    ", length(intersect(ids_ev, ids_heatpump)), "\n")
cat("EV & Solar overlap:        ", length(intersect(ids_ev, ids_solar)), "\n")
cat("Heat Pump & Solar overlap: ", length(intersect(ids_heatpump, ids_solar)), "\n")

# 4. Combine all unique customer IDs so each customer is pulled from Arrow ONLY ONCE
all_ids <- unique(c(ids_sample, ids_ev, ids_heatpump, ids_solar))
cat("\nTotal unique customer IDs requested:", length(all_ids), "\n")

# 5. Pull all interval rows from Arrow in one pass and clean column types
df_all <- ds %>%
  filter(CUSTOMER_NUMBER %in% all_ids) %>%
  collect() %>%
  mutate(
    # Standardize ID type in R for exact matching
    CUSTOMER_NUMBER = cast_id(CUSTOMER_NUMBER),
    
    # Cast string numeric columns safely
    hour_ending = as.numeric(hour_ending),
    hourly_kwh  = as.numeric(hourly_kwh),
    
    # Parse usage_date string and derive season
    parsed_date = parse_date_time(usage_date, orders = c("ymd", "ymd HMS", "mdy", "mdy HMS")),
    m = month(parsed_date),
    season = factor(
      case_when(
        m %in% c(12, 1, 2)  ~ "Winter",
        m %in% c(3, 4, 5)   ~ "Spring",
        m %in% c(6, 7, 8)   ~ "Summer",
        m %in% c(9, 10, 11) ~ "Fall"
      ),
      levels = c("Winter", "Spring", "Summer", "Fall")
    ),
    
    # Binary indicator flags (TRUE even if a customer belongs to multiple groups)
    is_sample   = CUSTOMER_NUMBER %in% ids_sample,
    is_ev       = CUSTOMER_NUMBER %in% ids_ev,
    is_heatpump = CUSTOMER_NUMBER %in% ids_heatpump,
    is_solar    = CUSTOMER_NUMBER %in% ids_solar,
    
    # Mutually exclusive group label for easy plotting
    # (Tags multi-tech customers explicitly, and prevents sample overlap from mislabeling)
    tech_count = is_ev + is_heatpump + is_solar,
    source_group = case_when(
      tech_count > 1 ~ "Multiple Tech",
      is_ev          ~ "EV",
      is_heatpump    ~ "Heat Pump",
      is_solar       ~ "Solar PV",
      is_sample      ~ "General Sample",
      TRUE           ~ "Other"
    )
  ) %>%
  select(-m, -tech_count)

# 6. Post-load verification checks
cat("\n--- Post-Load Verification ---\n")
cat("Total rows loaded:", nrow(df_all), "\n")
cat("Unique customers found in ds:", n_distinct(df_all$CUSTOMER_NUMBER), "of", length(all_ids), "\n")
cat("Missing IDs not found in ds: ", length(setdiff(all_ids, unique(df_all$CUSTOMER_NUMBER))), "\n\n")

# Breakdown of unique customers and total rows per mutually exclusive group
df_all %>%
  group_by(source_group) %>%
  summarize(
    unique_customers = n_distinct(CUSTOMER_NUMBER),
    total_rows       = n(),
    na_kwh_rows      = sum(is.na(hourly_kwh)),
    .groups          = "drop"
  ) %>%
  print()

# Breakdown of binary flag combinations (e.g., EV + Solar PV)
df_all %>%
  distinct(CUSTOMER_NUMBER, is_sample, is_ev, is_heatpump, is_solar) %>%
  count(is_sample, is_ev, is_heatpump, is_solar, name = "customer_count") %>%
  print()



library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Compute each customer's average hourly load (equal-weighting every customer)
cust_hourly <- df_all %>%
  filter(!is.na(hourly_kwh), !is.na(hour_ending)) %>%
  group_by(CUSTOMER_NUMBER, is_ev, is_solar, is_heatpump, is_sample, hour_ending) %>%
  summarize(cust_mean_kwh = mean(hourly_kwh, na.rm = TRUE), .groups = "drop")

# 2. Build both comparisons (EV vs. Non-EV and Solar vs. Non-Solar)
comparison_curves <- bind_rows(
  cust_hourly %>%
    mutate(
      comparison = "EV vs. Non-EV",
      status = if_else(is_ev, "Has Technology (Yes)", "Does Not Have (No)")
    ),
  cust_hourly %>%
    mutate(
      comparison = "Solar PV vs. Non-Solar",
      status = if_else(is_solar, "Has Technology (Yes)", "Does Not Have (No)")
    )
) %>%
  group_by(comparison, status, hour_ending) %>%
  summarize(
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    n_cust   = n(),
    .groups  = "drop"
  )

# 3. Plot side-by-side 24-hour average load profiles
ggplot(comparison_curves, aes(x = hour_ending, y = mean_kwh, color = status, fill = status)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  facet_wrap(~ comparison) +
  scale_x_continuous(breaks = seq(1, 24, by = 3)) +
  scale_color_manual(values = c("Has Technology (Yes)" = "#1b9e77", "Does Not Have (No)" = "#d95f02")) +
  scale_fill_manual(values = c("Has Technology (Yes)" = "#1b9e77", "Does Not Have (No)" = "#d95f02")) +
  labs(
    title = "Average 24-Hour Load Profile: EV vs. Non-EV and Solar vs. Non-Solar",
    subtitle = "Equal-weighted across matched customers (n = 505) | Shaded band = ±1 SE",
    x = "Hour Ending",
    y = "Average Hourly kWh",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

library(arrow)
library(dplyr)
library(ggplot2)

# 1. Extract customer-level flags from `df_all` using `is_heatpump` and `is_ev`
cust_flags <- df_all %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    hp_flag = any(as.logical(is_heatpump), na.rm = TRUE),
    ev_flag = any(as.logical(is_ev), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER))

# Build lookup table where HP + EV customers belong to BOTH "All Heat Pumps" and "Heat Pump + EV"
cohort_lookup <- bind_rows(
  cust_flags %>%
    filter(hp_flag) %>%
    transmute(CUSTOMER_NUMBER, cohort = "All Heat Pumps"),
  cust_flags %>%
    filter(hp_flag & ev_flag) %>%
    transmute(CUSTOMER_NUMBER, cohort = "Heat Pump + EV"),
  cust_flags %>%
    filter(!hp_flag) %>%
    transmute(CUSTOMER_NUMBER, cohort = "All Other Data (Non-HP)")
)

cohort_arrow <- arrow_table(cohort_lookup)

# 2. Aggregate hourly usage by customer and 3°F temperature bin across all 24 hours in `ds`
# Note: Any customer in `ds` not listed in `df_all` is treated as "All Other Data (Non-HP)"
ds_cust_temp <- ds %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    temp_num        = cast(temp, float64()),
    kwh_num         = cast(hourly_kwh, float64()),
    temp_bin        = round(temp_num / 3) * 3
  ) %>%
  filter(!is.na(temp_bin), !is.na(kwh_num)) %>%
  group_by(CUSTOMER_NUMBER, temp_bin) %>%
  summarize(cust_mean_kwh = mean(kwh_num, na.rm = TRUE), .groups = "drop") %>%
  left_join(cohort_arrow, by = "CUSTOMER_NUMBER") %>%
  mutate(cohort = coalesce(cohort, "All Other Data (Non-HP)")) %>%
  collect()

# 3. Compute cohort customer counts & equal-weighted group averages per temperature bin
cohort_levels <- c("Heat Pump + EV", "All Heat Pumps", "All Other Data (Non-HP)")

cohort_counts <- ds_cust_temp %>%
  distinct(cohort, CUSTOMER_NUMBER) %>%
  count(cohort, name = "n_cust") %>%
  mutate(
    cohort       = factor(cohort, levels = cohort_levels),
    cohort_label = paste0(cohort, " (n = ", n_cust, ")")
  ) %>%
  arrange(cohort)

thermal_curve_summary <- ds_cust_temp %>%
  group_by(cohort, temp_bin) %>%
  summarize(
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    n_cust   = n(),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 5) %>%
  mutate(cohort = factor(cohort, levels = cohort_levels)) %>%
  left_join(cohort_counts, by = "cohort") %>%
  mutate(cohort_label = factor(cohort_label, levels = cohort_counts$cohort_label))

# 4. Plot All Three Curves: Heat Pump + EV vs. All Heat Pumps vs. All Other Data
ggplot(
  thermal_curve_summary,
  aes(x = temp_bin, y = mean_kwh, color = cohort_label, fill = cohort_label)
) +
  geom_vline(xintercept = c(30, 60, 70), linetype = "dotted", color = "gray55") +
  geom_ribbon(
    aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("#7570b3", "#1b9e77", "#d95f02")) +
  scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02")) +
  labs(
    title = "Average Hourly Usage by Outdoor Temperature: Heat Pump + EV vs. All Heat Pumps vs. All",
    subtitle = "Equal-weighted customer averages across all 24 hours in 3°F bins | Band = ±1 SE",
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(ncol = 1))

library(arrow)
library(dplyr)
library(ggplot2)

# 1. Assign each customer to one of the four cohorts using `df_all`
cohort_lookup <- df_all %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    ev    = any(is_ev, na.rm = TRUE),
    hp    = any(is_heatpump, na.rm = TRUE),
    solar = any(is_solar, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER),
    cohort = case_when(
      ev & !hp & !solar  ~ "EV Only",
      !ev & !hp & !solar ~ "Non-EV (No HP/Solar)",
      ev & hp & !solar   ~ "EV + Heat Pump",
      ev & solar         ~ "EV + Solar",
      TRUE               ~ NA_character_
    )
  ) %>%
  filter(!is.na(cohort)) %>%
  select(CUSTOMER_NUMBER, cohort)

cohort_arrow <- arrow_table(cohort_lookup)

# 2. Single-pass Arrow aggregation on day_type & hour_ending (no Arrow string functions)
weekday_cust_hourly <- ds %>%
  select(CUSTOMER_NUMBER, day_type, hour_ending, hourly_kwh) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    day_type_raw    = cast(day_type, utf8()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64())
  ) %>%
  filter(!is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
  inner_join(cohort_arrow, by = "CUSTOMER_NUMBER") %>%
  group_by(CUSTOMER_NUMBER, cohort, day_type_raw, hr) %>%
  summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  collect()

gc()

# Check exact values in `day_type` and filter to Weekdays in R
cat("Unique day_type values in ds:", paste(unique(weekday_cust_hourly$day_type_raw), collapse = ", "), "\n")

weekday_complete <- weekday_cust_hourly %>%
  filter(tolower(trimws(day_type_raw)) %in% c("weekday", "wd", "weekdays", "1", "true")) %>%
  group_by(CUSTOMER_NUMBER, cohort, hr) %>%
  summarize(cust_hr_kwh = mean(cust_hr_kwh, na.rm = TRUE), .groups = "drop") %>%
  group_by(CUSTOMER_NUMBER, cohort) %>%
  filter(n() == 24L) %>%
  mutate(cust_avg_kwh = mean(cust_hr_kwh, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(cust_avg_kwh >= 0.05)

stopifnot("No weekday rows matched — check the printed unique day_type values above" = nrow(weekday_complete) > 0)

overall_mean <- mean(weekday_complete$cust_hr_kwh, na.rm = TRUE)

weekday_complete <- weekday_complete %>%
  mutate(norm_hr_kwh = cust_hr_kwh * (overall_mean / cust_avg_kwh))

cohort_summary <- weekday_complete %>%
  group_by(cohort, hr) %>%
  summarize(
    n_cust    = n(),
    mean_kwh  = mean(cust_hr_kwh, na.rm = TRUE),
    se_kwh    = sd(cust_hr_kwh, na.rm = TRUE) / sqrt(n()),
    mean_norm = mean(norm_hr_kwh, na.rm = TRUE),
    se_norm   = sd(norm_hr_kwh, na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  ) %>%
  mutate(
    cohort_label = paste0(cohort, " (n = ", scales::comma(n_cust), ")")
  )

palette_4 <- c("#1b9e77", "#7570b3", "#e7298a", "#d95f02")

# 3. Plot 1: Raw Average Weekday 24-Hour Load Curves
p_raw <- ggplot(cohort_summary, aes(x = hr, y = mean_kwh, color = cohort_label, fill = cohort_label)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:24) +
  scale_color_manual(values = palette_4, name = NULL) +
  scale_fill_manual(values = palette_4, name = NULL) +
  labs(
    title = "Weekday 24-Hour Average Load Curves: EV, Non-EV, EV + Heat Pump, EV + Solar",
    subtitle = "Shaded bands = ±1 SE across customers | All days included",
    x = "Hour Ending (1 = 12–1 AM, 12 = 11 AM–12 PM, 24 = 11 PM–12 AM)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_raw)

# 4. Plot 2: Size-Normalized Weekday 24-Hour Load Curves
p_norm <- ggplot(cohort_summary, aes(x = hr, y = mean_norm, color = cohort_label, fill = cohort_label)) +
  geom_hline(yintercept = overall_mean, linetype = "dotted", color = "gray45") +
  geom_ribbon(aes(ymin = mean_norm - se_norm, ymax = mean_norm + se_norm), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:24) +
  scale_color_manual(values = palette_4, name = NULL) +
  scale_fill_manual(values = palette_4, name = NULL) +
  labs(
    title = "Size-Normalized Weekday 24-Hour Load Curves by Cohort",
    subtitle = "Each customer scaled to equal 24h mean | Dotted line = 1.0x typical home usage",
    x = "Hour Ending (1 = 12–1 AM, 12 = 11 AM–12 PM, 24 = 11 PM–12 AM)",
    y = "Size-Normalized Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_norm)



library(dplyr)

if (!exists("ev_clustered") || !exists("steep_cluster_id")) {
  stop("`ev_clustered` or `steep_cluster_id` not found. Run the clustering script first.")
}

# 1. Add `charger_level` to `ev_clustered`
ev_clustered <- ev_clustered %>%
  mutate(
    charger_level = if_else(raw_cluster == steep_cluster_id, "Level 2", "Level 1")
  )

cat("--- Customer Counts by `charger_level` and `is_solar` ---\n")
ev_clustered %>%
  count(charger_level, is_solar, name = "n_customers") %>%
  group_by(is_solar) %>%
  mutate(pct_within_solar = round(100 * n_customers / sum(n_customers), 1)) %>%
  ungroup() %>%
  print()

# 2. Join `charger_level` back onto `df_all` (Non-EVs and unclustered EVs get NA)
df_all <- df_all %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  select(-any_of("charger_level")) %>% # Prevents duplicate columns if re-run
  left_join(
    ev_clustered %>% select(CUSTOMER_NUMBER, charger_level),
    by = "CUSTOMER_NUMBER"
  )

cat("\n--- Row/Customer Check on `df_all$charger_level` ---\n")
df_all %>%
  distinct(CUSTOMER_NUMBER, is_ev, is_solar, charger_level) %>%
  count(is_ev, is_solar, charger_level, name = "n_customers") %>%
  print()

library(arrow)
library(dplyr)
library(ggplot2)

# ==============================================================================
# STEP 1: Filter `df_all` to Known EVs (`is_ev == TRUE`)
# ==============================================================================
ev_only_lookup <- df_all %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    is_ev       = any(is_ev, na.rm = TRUE),
    is_solar    = any(is_solar, na.rm = TRUE),
    is_heatpump = any(is_heatpump, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  filter(is_ev) %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  select(CUSTOMER_NUMBER, is_solar, is_heatpump)

ev_arrow <- arrow_table(ev_only_lookup)

# ==============================================================================
# STEP 2: Detect Date & Temperature Columns and Pull Mild Days (60–70°F)
# ==============================================================================
ds_cols  <- names(ds)
date_col <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]
temp_col <- ds_cols[grepl("temp", tolower(ds_cols))][1]

if (is.na(date_col)) {
  stop(paste("No date column found in `ds`. Available columns:", paste(ds_cols, collapse = ", ")))
}

if (!is.na(temp_col)) {
  ev_mild_hours <- ds %>%
    select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh", temp_col))) %>%
    rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
    mutate(
      CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
      dt              = cast(raw_dt, date32()),
      hr              = cast(hour_ending, int32()),
      kwh             = cast(hourly_kwh, float64()),
      temp_f          = cast(raw_temp, float64())
    ) %>%
    filter(!is.na(dt), !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
    inner_join(ev_arrow, by = "CUSTOMER_NUMBER") %>%
    collect() %>%
    group_by(dt) %>%
    mutate(day_temp_f = mean(temp_f, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(day_temp_f), day_temp_f >= 60, day_temp_f <= 70)
} else {
  df_temp_col <- names(df_all)[grepl("temp", tolower(names(df_all)))][1]
  df_date_col <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), names(df_all))[1]
  if (is.na(df_temp_col) || is.na(df_date_col)) {
    stop("No temperature column found in `ds` or `df_all`.")
  }
  mild_dates <- df_all %>%
    mutate(dt = as.Date(.data[[df_date_col]]), temp_f = as.numeric(.data[[df_temp_col]])) %>%
    group_by(dt) %>%
    summarize(day_temp_f = mean(temp_f, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(day_temp_f), day_temp_f >= 60, day_temp_f <= 70) %>%
    select(dt)

  ev_mild_hours <- ds %>%
    select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh"))) %>%
    rename(raw_dt = all_of(date_col)) %>%
    mutate(
      CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
      dt              = cast(raw_dt, date32()),
      hr              = cast(hour_ending, int32()),
      kwh             = cast(hourly_kwh, float64())
    ) %>%
    filter(!is.na(dt), !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
    inner_join(ev_arrow, by = "CUSTOMER_NUMBER") %>%
    inner_join(arrow_table(mild_dates), by = "dt") %>%
    collect()
}

gc()

# ==============================================================================
# STEP 3A: Overnight (10 PM – 6 AM) 1-Hour Step-Change Features on 60–70°F Days
# ==============================================================================
night_window_mild <- ev_mild_hours %>%
  filter(hr %in% c(22L:24L, 1L:6L)) %>%
  mutate(
    night_date = if_else(hr >= 22L, dt + 1L, dt),
    night_seq  = if_else(hr >= 22L, hr - 24L, hr)
  ) %>%
  arrange(CUSTOMER_NUMBER, night_date, night_seq)

step_features_mild <- night_window_mild %>%
  group_by(CUSTOMER_NUMBER, night_date) %>%
  mutate(
    step_kwh = kwh - lag(kwh),
    seq_gap  = night_seq - lag(night_seq)
  ) %>%
  ungroup() %>%
  filter(seq_gap == 1L, night_seq >= -1L, night_seq <= 6L, !is.na(step_kwh)) %>%
  group_by(CUSTOMER_NUMBER, is_solar, is_heatpump) %>%
  summarize(
    n_night_steps     = n(),
    p95_night_ramp_kw = quantile(step_kwh, probs = 0.95, na.rm = TRUE),
    abs_drop_p05_kw   = abs(quantile(step_kwh, probs = 0.05, na.rm = TRUE)),
    p95_abs_step_kw   = quantile(abs(step_kwh), probs = 0.95, na.rm = TRUE),
    .groups           = "drop"
  )

# ==============================================================================
# STEP 3B: Day-to-Day Overnight Variability Across Nights (10 PM – 6 AM)
# ==============================================================================
night_to_night_mild <- night_window_mild %>%
  filter(night_seq >= -1L, night_seq <= 6L) %>%
  group_by(CUSTOMER_NUMBER, night_date) %>%
  filter(n() >= 5L) %>% # Require >=5 valid overnight hours for a complete night summary
  summarize(
    night_mean_kw = mean(kwh, na.rm = TRUE),
    night_peak_kw = max(kwh, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    n_nights           = n(),
    # Standard deviation across nights of overnight average load (catches charge vs. non-charge nights)
    sd_night_mean_kw   = sd(night_mean_kw, na.rm = TRUE),
    # P90 - P10 spread of nightly peak load across mild nights
    spread_night_pk_kw = quantile(night_peak_kw, 0.90, na.rm = TRUE) - quantile(night_peak_kw, 0.10, na.rm = TRUE),
    .groups            = "drop"
  )

# Combine step-change + day-to-day variability features
ev_features_mild <- step_features_mild %>%
  inner_join(night_to_night_mild, by = "CUSTOMER_NUMBER") %>%
  filter(n_night_steps >= 20L, n_nights >= 5L, !is.na(sd_night_mean_kw))

# ==============================================================================
# STEP 4: Control for `is_solar` + K-Means (k = 2) on Step Changes + Day-to-Day SD
# ==============================================================================
set.seed(42)

resid_scale <- function(x, solar_flag) {
  as.numeric(scale(resid(lm(x ~ as.integer(solar_flag)))))
}

clust_mat_mild <- cbind(
  ramp_adj     = resid_scale(ev_features_mild$p95_night_ramp_kw,  ev_features_mild$is_solar),
  drop_adj     = resid_scale(ev_features_mild$abs_drop_p05_kw,    ev_features_mild$is_solar),
  step_adj     = resid_scale(ev_features_mild$p95_abs_step_kw,    ev_features_mild$is_solar),
  night_sd_adj = resid_scale(ev_features_mild$sd_night_mean_kw,   ev_features_mild$is_solar),
  night_sp_adj = resid_scale(ev_features_mild$spread_night_pk_kw, ev_features_mild$is_solar)
)

km_mild <- kmeans(clust_mat_mild, centers = 2, nstart = 25)
ev_features_mild$raw_cluster <- km_mild$cluster

# Identify which cluster has higher combined overnight step + day-to-day variability ("Level 2")
steep_id_mild <- ev_features_mild %>%
  group_by(raw_cluster) %>%
  summarize(score = mean(p95_abs_step_kw + sd_night_mean_kw, na.rm = TRUE), .groups = "drop") %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  pull(raw_cluster)

ev_clustered_mild <- ev_features_mild %>%
  mutate(
    charger_level = if_else(raw_cluster == steep_id_mild, "Level 2", "Level 1")
  )

# Update `charger_level` on `df_all`
df_all <- df_all %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  select(-any_of("charger_level")) %>%
  left_join(ev_clustered_mild %>% select(CUSTOMER_NUMBER, charger_level), by = "CUSTOMER_NUMBER")

# ==============================================================================
# STEP 5: Plot 24-Hour Load Curves on 60–70°F Days (Level 1 vs. Level 2)
# ==============================================================================
mild_24h_summary <- ev_mild_hours %>%
  select(CUSTOMER_NUMBER, is_solar, hr, kwh) %>%
  inner_join(ev_clustered_mild %>% select(CUSTOMER_NUMBER, charger_level), by = "CUSTOMER_NUMBER") %>%
  group_by(CUSTOMER_NUMBER, charger_level, is_solar, hr) %>%
  summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  group_by(CUSTOMER_NUMBER, charger_level, is_solar) %>%
  filter(n() == 24L) %>%
  group_by(is_solar, charger_level, hr) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_hr_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_hr_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  )

rm(ev_mild_hours, night_window_mild); gc()

p_mild_l1_l2 <- ggplot(mild_24h_summary, aes(x = hr, y = mean_kwh, color = charger_level, fill = charger_level)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  facet_wrap(~ is_solar, ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) +
  scale_color_manual(values = c("Level 2" = "#d95f02", "Level 1" = "#7570b3"), name = NULL) +
  scale_fill_manual(values = c("Level 2" = "#d95f02", "Level 1" = "#7570b3"), name = NULL) +
  labs(
    title = "Known EV 24-Hour Net Load Curves on 60–70°F Days: Level 1 vs. Level 2",
    subtitle = "Clustered on overnight step changes + night-to-night variability (controlling for `is_solar`)",
    x = "Hour Ending (1 = 12–1 AM, 12 = 11 AM–12 PM, 24 = 11 PM–12 AM)",
    y = "Average Hourly Net Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text      = element_text(face = "bold", size = 11)
  )

print(p_mild_l1_l2)

# ==============================================================================
# STEP 6: Print Final Level 1 and Level 2 Counts & Feature Medians
# ==============================================================================
cat("\n=== Final Level 1 vs. Level 2 Counts (60–70°F Days, Step + Night-to-Night Variability) ===\n")
ev_clustered_mild %>%
  group_by(charger_level) %>%
  summarize(
    n_evs            = n(),
    pct              = round(100 * n() / nrow(ev_clustered_mild), 1),
    med_ramp_p95_kw  = round(median(p95_night_ramp_kw), 2),
    med_drop_p05_kw  = round(median(abs_drop_p05_kw), 2),
    med_night_sd_kw  = round(median(sd_night_mean_kw), 2),
    med_night_spr_kw = round(median(spread_night_pk_kw), 2),
    .groups          = "drop"
  ) %>%
  print()



library(arrow)
library(dplyr)
library(ggplot2)

if (!exists("full_ds_features")) {
  stop("`full_ds_features` not found in memory—run the full-`ds` scoring script first, then run this block.")
}

# ==============================================================================
# STEP 1: Set Cutoff to 90th Percentile Across Entire Unregistered `ds` Pool
# ==============================================================================
chosen_pct    <- 0.90
chosen_cutoff <- quantile(full_ds_features$prob_l1[full_ds_features$y_l1 == 0L], probs = chosen_pct, na.rm = TRUE)

l1_candidates <- full_ds_features %>%
  filter(
    y_l1 == 0L,
    prob_l1 >= chosen_cutoff,
    p95_overnight_kw < 4.5
  ) %>%
  arrange(desc(prob_l1))

cat(sprintf("Extracted %s total Level 1 candidates across ENTIRE `ds` at P90 (cutoff = %.4f)\n",
            scales::comma(nrow(l1_candidates)), chosen_cutoff))

# Quick check of labeled Known Level 1 EV recall at this P90 cutoff
known_l1_total   <- sum(full_ds_features$y_l1 == 1L)
known_l1_captured <- sum(full_ds_features$y_l1 == 1L & full_ds_features$prob_l1 >= chosen_cutoff & full_ds_features$p95_overnight_kw < 4.5)
cat(sprintf("Known Level 1 EV capture rate at P90: %d / %d (%.1f%%)\n",
            known_l1_captured, known_l1_total, 100 * known_l1_captured / known_l1_total))

# ==============================================================================
# STEP 2: Pull 24-Hour Curves on Mild 60–70°F Days Across Full `ds` Cohort
# ==============================================================================
candidate_ids <- as.character(l1_candidates$CUSTOMER_NUMBER)

full_verify_lookup <- full_ds_features %>%
  mutate(
    CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER),
    cohort = case_when(
      y_l1 == 1L                         ~ "Known Level 1 EV",
      CUSTOMER_NUMBER %in% candidate_ids ~ "Level 1 Candidate (P90)",
      TRUE                               ~ "Remaining Non-EV"
    )
  ) %>%
  select(CUSTOMER_NUMBER, cohort)

ds_cols  <- names(ds)
date_col <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]
temp_col <- ds_cols[grepl("temp", tolower(ds_cols))][1]

if (!is.na(temp_col) && !is.na(date_col)) {
  mild_dates <- ds %>%
    select(all_of(c(date_col, temp_col))) %>%
    rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
    mutate(dt = cast(raw_dt, date32()), temp_f = cast(raw_temp, float64())) %>%
    filter(!is.na(dt), !is.na(temp_f)) %>%
    group_by(dt) %>%
    summarize(day_temp_f = mean(temp_f, na.rm = TRUE), .groups = "drop") %>%
    collect() %>%
    filter(day_temp_f >= 60, day_temp_f <= 70) %>%
    select(dt)

  full_24h_raw <- ds %>%
    select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh"))) %>%
    rename(raw_dt = all_of(date_col)) %>%
    mutate(
      CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
      dt              = cast(raw_dt, date32()),
      hr              = cast(hour_ending, int32()),
      kwh             = cast(hourly_kwh, float64())
    ) %>%
    filter(!is.na(dt), !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
    inner_join(arrow_table(full_verify_lookup), by = "CUSTOMER_NUMBER") %>%
    inner_join(arrow_table(mild_dates), by = "dt") %>%
    group_by(CUSTOMER_NUMBER, cohort, hr) %>%
    summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
    collect()
} else {
  full_24h_raw <- ds %>%
    select(CUSTOMER_NUMBER, hour_ending, hourly_kwh) %>%
    mutate(
      CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
      hr              = cast(hour_ending, int32()),
      kwh             = cast(hourly_kwh, float64())
    ) %>%
    filter(!is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
    inner_join(arrow_table(full_verify_lookup), by = "CUSTOMER_NUMBER") %>%
    group_by(CUSTOMER_NUMBER, cohort, hr) %>%
    summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
    collect()
}

gc()

# ==============================================================================
# STEP 3: Summarize Complete 24-Hour Profiles & Plot Shape Alignment
# ==============================================================================
full_24h_summary <- full_24h_raw %>%
  group_by(CUSTOMER_NUMBER, cohort) %>%
  filter(n() == 24L) %>%
  group_by(cohort, hr) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_hr_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_hr_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(cohort_label = paste0(cohort, " (n = ", scales::comma(n_cust), ")"))

rm(full_24h_raw); gc()

label_levels <- full_24h_summary %>%
  distinct(cohort, cohort_label) %>%
  arrange(factor(cohort, levels = c("Known Level 1 EV", "Level 1 Candidate (P90)", "Remaining Non-EV"))) %>%
  pull(cohort_label)

full_24h_summary$cohort_label <- factor(full_24h_summary$cohort_label, levels = label_levels)

p_verify_full_90 <- ggplot(full_24h_summary, aes(x = hr, y = mean_kwh, color = cohort_label, fill = cohort_label)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:24) +
  scale_color_manual(values = c("#7570b3", "#e7298a", "#1b9e77"), name = NULL) +
  scale_fill_manual(values = c("#7570b3", "#e7298a", "#1b9e77"), name = NULL) +
  labs(
    title = "Full-Territory Non-Solar 24-Hour Load Shape Verification (90th Percentile Cutoff)",
    subtitle = "60–70°F mild days | Known L1 EV vs. P90 Unregistered Candidates vs. Remaining Non-EV",
    x = "Hour Ending (1 = 12–1 AM, 12 = 11 AM–12 PM, 24 = 11 PM–12 AM)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_verify_full_90)

library(arrow)
library(dplyr)
library(nnet)
library(ggplot2)

if (!"charger_level" %in% names(df_all)) {
  stop("`charger_level` not found in `df_all`. Join your L1/L2 labels onto `df_all` first.")
}

ds_cols    <- names(ds)
date_col   <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]
temp_col   <- ds_cols[grepl("temp", tolower(ds_cols))][1]
ds_has_sol <- "is_solar" %in% ds_cols

# ==============================================================================
# STEP 1: Mild 60–70°F Dates as a Tiny R Vector (Fast, Crash-Free Filter)
# ==============================================================================
mild_dt_vec <- ds %>%
  select(all_of(c(date_col, temp_col))) %>%
  rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
  mutate(dt = cast(raw_dt, date32()), temp_f = cast(raw_temp, float64())) %>%
  filter(!is.na(dt), !is.na(temp_f)) %>%
  group_by(dt) %>%
  summarize(day_temp_f = mean(temp_f, na.rm = TRUE), .groups = "drop") %>%
  collect() %>%
  filter(day_temp_f >= 60, day_temp_f <= 70) %>%
  pull(dt)

# ==============================================================================
# STEP 2A: Arrow C++ Pass 1 — 24-Hour Customer Means (for 7–10 PM Slope & Plot)
# ==============================================================================
hr_summary <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh", if (ds_has_sol) "is_solar"))) %>%
  rename(raw_dt = all_of(date_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64())
  ) %>%
  filter(dt %in% mild_dt_vec, !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%
  group_by(across(any_of(c("CUSTOMER_NUMBER", "is_solar", "hr")))) %>%
  summarize(mean_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  collect()

gc()

# ==============================================================================
# STEP 2B: Arrow C++ Pass 2 — Overnight Swing & Night-to-Night Variability
#          (Aggregates by customer + date in C++ before collect() -> super fast!)
# ==============================================================================
night_daily_arrow <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh"))) %>%
  rename(raw_dt = all_of(date_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64())
  ) %>%
  filter(dt %in% mild_dt_vec, hr %in% c(22L:24L, 1L:6L), !is.na(kwh)) %>%
  # Group by customer & calendar date in C++ (1 compact row per customer-night)
  group_by(CUSTOMER_NUMBER, dt) %>%
  summarize(
    n_hrs         = n(),
    night_mean_kw = mean(kwh, na.rm = TRUE),
    night_peak_kw = max(kwh, na.rm = TRUE),
    night_min_kw  = min(kwh, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  filter(n_hrs >= 5L) %>%
  collect()

gc()

# Summarize across mild nights in R (tiny table: 1 row per customer-night)
night_features_fast <- night_daily_arrow %>%
  mutate(night_swing_kw = night_peak_kw - night_min_kw) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    n_nights           = n(),
    # 1. Steep overnight step/swing (`max - min` within the night -> catches L2 ramps & drops)
    p95_night_swing_kw = quantile(night_swing_kw, probs = 0.95, na.rm = TRUE),
    avg_night_swing_kw = mean(night_swing_kw, na.rm = TRUE),
    # 2. Day-to-day overnight variability (catches intermittent charge vs. non-charge nights)
    sd_night_mean_kw   = sd(night_mean_kw, na.rm = TRUE),
    spread_night_pk_kw = quantile(night_peak_kw, 0.90, na.rm = TRUE) - quantile(night_peak_kw, 0.10, na.rm = TRUE),
    # 3. Elevated nighttime level
    p95_overnight_kw   = quantile(night_peak_kw, 0.95, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  filter(n_nights >= 5L, !is.na(sd_night_mean_kw))

rm(night_daily_arrow); gc()

# ==============================================================================
# STEP 3: Attach Known `df_all` Labels (`charger_level`, `is_ev`, `is_solar`)
# ==============================================================================
df_cust_flags <- df_all %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    in_df_all = TRUE,
    is_ev     = any(is_ev, na.rm = TRUE),
    is_solar  = any(is_solar, na.rm = TRUE),
    c_level   = as.character(charger_level[!is.na(charger_level)][1]),
    .groups   = "drop"
  ) %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER))

features_df <- hr_summary %>%
  group_by(CUSTOMER_NUMBER) %>%
  filter(n() == 24L) %>%
  summarize(
    ds_solar       = if ("is_solar" %in% names(.)) any(as.logical(is_solar), na.rm = TRUE) else NA,
    min_24h_kw     = min(mean_kwh, na.rm = TRUE),
    # Consistent 7–10 PM slope (`HE 20, 21, 22`)
    slope_7pm_10pm = cov(hr[hr %in% 20L:22L], mean_kwh[hr %in% 20L:22L]) / var(hr[hr %in% 20L:22L]),
    .groups        = "drop"
  ) %>%
  inner_join(night_features_fast, by = "CUSTOMER_NUMBER") %>%
  left_join(df_cust_flags, by = "CUSTOMER_NUMBER") %>%
  mutate(
    in_df_all    = coalesce(in_df_all, FALSE),
    is_ev        = coalesce(is_ev, FALSE),
    is_solar     = coalesce(is_solar, ds_solar, min_24h_kw < -0.05),
    solar_num    = as.integer(is_solar),
    target_class = case_when(
      is_ev & !is.na(c_level) & c_level == "Level 1" ~ "Level 1",
      is_ev & !is.na(c_level) & c_level == "Level 2" ~ "Level 2",
      !is_ev                                         ~ "Non-EV",
      TRUE                                           ~ NA_character_
    )
  ) %>%
  filter(!is.na(target_class)) %>%
  mutate(target_class = factor(target_class, levels = c("Non-EV", "Level 1", "Level 2")))

print(count(features_df, is_solar, target_class))

# ==============================================================================
# STEP 4: Fit Fast 3-Class Multinomial Regression & Score Full Territory (`ds`)
# ==============================================================================
fit_fast_exact <- multinom(
  target_class ~ solar_num + slope_7pm_10pm +
                 p95_night_swing_kw + avg_night_swing_kw +
                 sd_night_mean_kw + spread_night_pk_kw + p95_overnight_kw,
  data  = features_df %>% filter(in_df_all),
  trace = FALSE
)

probs <- predict(fit_fast_exact, newdata = features_df, type = "probs")

features_df <- features_df %>%
  mutate(
    prob_non_ev = probs[, "Non-EV"],
    prob_l1     = probs[, "Level 1"],
    prob_l2     = probs[, "Level 2"]
  )

unreg <- features_df %>% filter(target_class == "Non-EV")

th_l1 <- quantile(unreg$prob_l1, 0.97, na.rm = TRUE)
th_l2 <- quantile(unreg$prob_l2, 0.97, na.rm = TRUE)

candidates_l2 <- unreg %>% filter(prob_l2 >= th_l2, prob_l2 >= prob_l1) %>% arrange(desc(prob_l2))
candidates_l1 <- unreg %>% filter(prob_l1 >= th_l1, prob_l1 > prob_l2)  %>% arrange(desc(prob_l1))

cat(sprintf("\nP97 Candidates Found -> Level 2: %s | Level 1: %s\n",
            scales::comma(nrow(candidates_l2)), scales::comma(nrow(candidates_l1))))

# ==============================================================================
# STEP 5: Plot 24-Hour Verification Curves (Faceted by `is_solar`)
# ==============================================================================
plot_df <- hr_summary %>%
  inner_join(
    features_df %>%
      mutate(
        cohort = case_when(
          target_class == "Level 2"                          ~ "Known Level 2 EV",
          CUSTOMER_NUMBER %in% candidates_l2$CUSTOMER_NUMBER ~ "Level 2 Candidate (P97)",
          target_class == "Level 1"                          ~ "Known Level 1 EV",
          CUSTOMER_NUMBER %in% candidates_l1$CUSTOMER_NUMBER ~ "Level 1 Candidate (P97)",
          TRUE                                               ~ "Remaining Non-EV"
        )
      ) %>%
      select(CUSTOMER_NUMBER, is_solar, cohort),
    by = "CUSTOMER_NUMBER"
  ) %>%
  group_by(is_solar, cohort, hr) %>%
  summarize(avg_kwh = mean(mean_kwh, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = hr, y = avg_kwh, color = cohort, linetype = grepl("Candidate", cohort))) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_line(linewidth = 1.15) +
  facet_wrap(~ is_solar, ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) +
  scale_color_manual(values = c(
    "Known Level 2 EV"        = "#d95f02",
    "Level 2 Candidate (P97)" = "#fc8d62",
    "Known Level 1 EV"        = "#7570b3",
    "Level 1 Candidate (P97)" = "#e7298a",
    "Remaining Non-EV"        = "#1b9e77"
  ), name = NULL) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  labs(
    title = "Fast 3-Class EV Classification: Known vs. P97 Candidates",
    subtitle = "60–70°F mild days | Predictors: solar, 7–10 PM slope, overnight swing, night-to-night SD & peak spread",
    x = "Hour Ending",
    y = "Average Hourly Net Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

library(arrow)
library(dplyr)

# Verify upstream objects exist
missing_objs <- setdiff(c("features_df", "candidates_l1", "candidates_l2"), ls())
if (length(missing_objs) > 0) {
  stop(paste(
    "Missing objects in R environment:", paste(missing_objs, collapse = ", "),
    "\nRe-run the fast 3-class classification script first so these objects are created."
  ))
}

# ==============================================================================
# 1. Build `ev_classification_lookup` (Must Run Before Joins / Lookups)
# ==============================================================================
l1_cand_ids <- as.character(candidates_l1$CUSTOMER_NUMBER)
l2_cand_ids <- as.character(candidates_l2$CUSTOMER_NUMBER)

ev_classification_lookup <- features_df %>%
  transmute(
    CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER),
    lvl1 = as.integer(target_class == "Level 1" | CUSTOMER_NUMBER %in% l1_cand_ids),
    lvl2 = as.integer(target_class == "Level 2" | CUSTOMER_NUMBER %in% l2_cand_ids),
    ev_source = case_when(
      target_class == "Level 1"        ~ "Known Level 1",
      CUSTOMER_NUMBER %in% l1_cand_ids ~ "Candidate Level 1 (P97)",
      target_class == "Level 2"        ~ "Known Level 2",
      CUSTOMER_NUMBER %in% l2_cand_ids ~ "Candidate Level 2 (P97)",
      TRUE                             ~ "Non-EV"
    )
  )

print(count(ev_classification_lookup, lvl1, lvl2, ev_source))

# ==============================================================================
# 2. Apply `lvl1` and `lvl2` to `df_all`
# ==============================================================================
df_all <- df_all %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
  select(-any_of(c("lvl1", "lvl2", "ev_source"))) %>%
  left_join(ev_classification_lookup, by = "CUSTOMER_NUMBER") %>%
  mutate(
    lvl1      = coalesce(lvl1, 0L),
    lvl2      = coalesce(lvl2, 0L),
    ev_source = coalesce(ev_source, "Non-EV")
  )

# ==============================================================================
# 3. Apply `lvl1` and `lvl2` Lazily to Full Arrow Dataset (`ds_classified`)
# ==============================================================================
l1_ids <- ev_classification_lookup %>% filter(lvl1 == 1L) %>% pull(CUSTOMER_NUMBER)
l2_ids <- ev_classification_lookup %>% filter(lvl2 == 1L) %>% pull(CUSTOMER_NUMBER)

ds_classified <- ds %>%
  mutate(
    cust_chr = cast(CUSTOMER_NUMBER, utf8()),
    lvl1     = if_else(cust_chr %in% l1_ids, 1L, 0L),
    lvl2     = if_else(cust_chr %in% l2_ids, 1L, 0L)
  ) %>%
  select(-cust_chr)

ds_classified %>%
  group_by(lvl1, lvl2) %>%
  summarize(
    n_rows      = n(),
    n_customers = n_distinct(CUSTOMER_NUMBER),
    .groups     = "drop"
  ) %>%
  collect() %>%
  print()