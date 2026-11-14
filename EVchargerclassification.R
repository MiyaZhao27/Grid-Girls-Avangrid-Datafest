
library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(nnet)


# load in data
pq <- list.files("cache_clean", pattern = "\\.parquet$", full.names = TRUE)
length(pq)  
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
  sample(size = 1000)

df <- ds %>%
  filter(CUSTOMER_NUMBER %in% sample_ids) %>%
  collect()

# Extract ID vectors and standardize types to match Arrow's CUSTOMER_NUMBER schema
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
    title = "Average 24-Hour Load, Known Solar and Likely EV/HP",
    subtitle = "Equal-weighted across matched customers (n = 505) | Shaded band = ±1 SE",
    x = "Hour Ending",
    y = "Average Hourly kWh",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")




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
      ev & !hp & !solar  ~ "Likely EV Only",
      !ev & !hp & !solar ~ "Unknown Sample",
      ev & hp & !solar   ~ "Likely EV + Likely HP",
      ev & solar         ~ "Likely EV + Solar",
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


# Map each base cohort to a distinct shade of green (EVs) or orange (Unknown / Non-EV)
base_palette <- c(
  "EV Only"              = "#648FFF", # Dark forest green
  "Likely EV Only"       = "#648FFF",
  "EV + Heat Pump"       = "#785EF0", # Medium emerald green
  "Likely EV + Likely HP"= "#785EF0",
  "EV + Solar"           = "#DC267F", # Light sage green
  "Likely EV + Solar"    = "#DC267F",
  "Non-EV (No HP/Solar)" = "#FFB000", # Orange
  "Unknown Sample"       = "#FFB000"
)

# Match colors dynamically to the exact `cohort_label` strings (including "(n = ...)")
label_map <- cohort_summary %>%
  distinct(cohort, cohort_label) %>%
  mutate(hex = base_palette[as.character(cohort)])

cohort_colors <- setNames(label_map$hex, label_map$cohort_label)

p_norm <- ggplot(cohort_summary, aes(x = hr, y = mean_norm, color = cohort_label, fill = cohort_label)) +
  geom_hline(yintercept = overall_mean, linetype = "dotted", color = "gray45") +
  geom_ribbon(aes(ymin = mean_norm - se_norm, ymax = mean_norm + se_norm), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:24) +
  scale_color_manual(values = cohort_colors, name = NULL) +
  scale_fill_manual(values = cohort_colors, name = NULL) +
  labs(
    title = "Normalized Weekday Load Curves by Cohort",
    x = "Hour of Day",
    y = "Normalized Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_norm)
# 3. Plot 1: Raw Average Weekday 24-Hour Load Curves
p_raw <- ggplot(cohort_summary, aes(x = hr, y = mean_kwh, color = cohort_label, fill = cohort_label)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:24) +
  scale_color_manual(values = cohort_colors, name = NULL) +
  scale_fill_manual(values = cohort_colors, name = NULL) +
  labs(
    title = "Weekday Average Load by Cohort",
    x = "Hour of Day",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_raw)
ggsave(p_raw, filename = "plots/weekday_avg_load_curves.png", width = 8, height = 6, dpi = 300)

# 4. Plot 2: Size-Normalized Weekday 24-Hour Load Curves
p_norm <- ggplot(cohort_summary, aes(x = hr, y = mean_norm, color = cohort_label, fill = cohort_label)) +
  geom_hline(yintercept = overall_mean, linetype = "dotted", color = "gray45") +
  geom_ribbon(aes(ymin = mean_norm - se_norm, ymax = mean_norm + se_norm), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:24) +
  # scale color so that all EVs different shades of green, unkwnon is orange
  scale_color_manual(values = cohort_colors, name = NULL) +
  scale_fill_manual(values = cohort_colors, name = NULL) +
  labs(
    title = "Normalized Weekday Load Curves by Cohort",
    x = "Hour of Day",
    y = "Normalized Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(p_norm, filename = "plots/weekday_normalized_load_curves.png", width = 8, height = 6, dpi = 300)



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
    title = "Likely EV Daily Load on 60–70°F Days: Level 1 vs. Level 2",
    subtitle = "clustered on overnight step changes + night-to-night variability",
    x = "Hour of Day",
    y = "Average Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text      = element_text(face = "bold", size = 11)
  )

print(p_mild_l1_l2)
ggsave(p_mild_l1_l2, filename = "plots/likely_ev_24h_load_curves.png", width = 8, height = 6, dpi = 300)

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




# ==============================================================================

# STEP 1: Update `charger_level` in `df_all` from `ev_clustered_mild`

# ==============================================================================

if (!exists("ev_clustered_mild")) {

  stop("`ev_clustered_mild` not found. Run the 60–70°F clustering script first.")

}



df_all <- df_all %>%

  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%

  select(-any_of("charger_level")) %>%

  left_join(

    ev_clustered_mild %>% select(CUSTOMER_NUMBER, charger_level),

    by = "CUSTOMER_NUMBER"

  )



cat("--- Updated `charger_level` Counts in `df_all` ---\n")

df_all %>%

  distinct(CUSTOMER_NUMBER, is_ev, charger_level) %>%

  count(is_ev, charger_level, name = "n_customers") %>%

  print()



# ==============================================================================

# STEP 2: Build Lookup for `Level 1 EV` vs. `Non-EV` Customers

# ==============================================================================

l1_vs_nonev_lookup <- df_all %>%

  group_by(CUSTOMER_NUMBER) %>%

  summarize(

    is_ev    = any(is_ev, na.rm = TRUE),

    is_solar = any(is_solar, na.rm = TRUE),

    c_level  = as.character(charger_level[!is.na(charger_level)][1]),

    .groups  = "drop"

  ) %>%

  mutate(

    CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER),

    cohort = case_when(

      is_ev & c_level == "Level 1" ~ "Level 1 EV",

      !is_ev                       ~ "Non-EV",

      TRUE                         ~ NA_character_ # Drops Level 2 EVs

    )

  ) %>%

  filter(!is.na(cohort)) %>%

  select(CUSTOMER_NUMBER, cohort, is_solar)



cat("\n--- Sample Sizes for Plot (`Level 1 EV` vs. `Non-EV`) ---\n")

print(count(l1_vs_nonev_lookup, is_solar, cohort))



lookup_arrow <- arrow_table(l1_vs_nonev_lookup)



# ==============================================================================

# STEP 3: Single-Pass Arrow Aggregation on Mild 60–70°F Days (Clean HVAC Baseline)

# ==============================================================================

ds_cols  <- names(ds)

date_col <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]

temp_col <- ds_cols[grepl("temp", tolower(ds_cols))][1]



if (!is.na(temp_col) && !is.na(date_col)) {

  hourly_raw <- ds %>%

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

    inner_join(lookup_arrow, by = "CUSTOMER_NUMBER") %>%

    collect() %>%

    group_by(dt) %>%

    mutate(day_temp_f = mean(temp_f, na.rm = TRUE)) %>%

    ungroup() %>%

    filter(!is.na(day_temp_f), day_temp_f >= 60, day_temp_f <= 70) %>%

    group_by(CUSTOMER_NUMBER, cohort, is_solar, hr) %>%

    summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop")

} else {

  # Fallback across all available days if temperature column is not in `ds`

  hourly_raw <- ds %>%

    select(CUSTOMER_NUMBER, hour_ending, hourly_kwh) %>%

    mutate(

      CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),

      hr              = cast(hour_ending, int32()),

      kwh             = cast(hourly_kwh, float64())

    ) %>%

    filter(!is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)) %>%

    inner_join(lookup_arrow, by = "CUSTOMER_NUMBER") %>%

    group_by(CUSTOMER_NUMBER, cohort, is_solar, hr) %>%

    summarize(cust_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%

    collect()

}



gc()



# ==============================================================================

# STEP 4: Summarize Complete 24-Hour Profiles & Plot `Level 1 EV` vs. `Non-EV`

# ==============================================================================

l1_nonev_summary <- hourly_raw %>%

  group_by(CUSTOMER_NUMBER, cohort, is_solar) %>%

  filter(n() == 24L) %>%

  group_by(is_solar, cohort, hr) %>%

  summarize(

    n_cust   = n(),

    mean_kwh = mean(cust_hr_kwh, na.rm = TRUE),

    se_kwh   = sd(cust_hr_kwh, na.rm = TRUE) / sqrt(n()),

    .groups  = "drop"

  ) %>%

  mutate(cohort = factor(cohort, levels = c("Level 1 EV", "Non-EV")))



palette_l1 <- c(

  "Level 1 EV" = "#7570b3",

  "Non-EV"     = "#1b9e77"

)



p_l1_vs_nonev <- ggplot(l1_nonev_summary, aes(x = hr, y = mean_kwh, color = cohort, fill = cohort)) +

  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +

  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +

  geom_line(linewidth = 1.25) +

  geom_point(size = 2) +

  facet_wrap(~ is_solar, ncol = 2, labeller = label_both) +

  scale_x_continuous(breaks = seq(2, 24, by = 2)) +

  scale_color_manual(values = palette_l1, name = NULL) +

  scale_fill_manual(values = palette_l1, name = NULL) +

  labs(

    title = "Daily Average Load: Likely Level 1 EV vs. Unknowns",

    x = "Hour of Day",

    y = "Average NetUsage (kWh)"

  ) +

  theme_minimal() +

  theme(

    legend.position = "top",

    strip.text      = element_text(face = "bold", size = 11)

  )



print(p_l1_vs_nonev) 
ggsave(p_l1_vs_nonev, filename = "plots/level1_vs_nonev_load_curves.png", width = 8, height = 6, dpi = 300)



if (!"charger_level" %in% names(df_all)) {
  stop("`charger_level` not found in `df_all`. Run your K-means EV clustering step first.")
}

ds_cols    <- names(ds)
date_col   <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]
temp_col   <- ds_cols[grepl("temp", tolower(ds_cols))][1]
ds_has_sol <- "is_solar" %in% ds_cols

# ==============================================================================
# STEP 1: Arrow C++ Pass 1 — Mild Hourly Baseline (58–72°F) for 24h Shape & Ramp
# ==============================================================================
hr_summary <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh", temp_col,
                  if (ds_has_sol) "is_solar"))) %>%
  rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64()),
    temp_f          = cast(raw_temp, float64())
  ) %>%
  filter(
    !is.na(dt), !is.na(hr), hr >= 1L, hr <= 24L,
    !is.na(kwh), !is.na(temp_f),
    temp_f >= 60, temp_f <= 70
  ) %>%
  group_by(across(any_of(c("CUSTOMER_NUMBER", "is_solar", "hr")))) %>%
  summarize(mean_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  collect()

gc()

# ==============================================================================
# STEP 2: Arrow C++ Pass 2 — True Mild Nights (10 PM–6 AM, Overnight Temp 55–72°F)
# ==============================================================================
night_daily_arrow <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh", temp_col))) %>%
  rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64()),
    temp_f          = cast(raw_temp, float64())
  ) %>%
  filter(!is.na(dt), hr %in% c(22L:24L, 1L:6L), !is.na(kwh), !is.na(temp_f)) %>%
  group_by(CUSTOMER_NUMBER, dt) %>%
  summarize(
    n_hrs          = n(),
    night_min_temp = min(temp_f, na.rm = TRUE),
    night_max_temp = max(temp_f, na.rm = TRUE),
    night_mean_kw  = mean(kwh, na.rm = TRUE),
    night_peak_kw  = max(kwh, na.rm = TRUE),
    night_min_kw   = min(kwh, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  filter(n_hrs >= 5L, night_min_temp >= 55, night_max_temp <= 72) %>%
  collect()

gc()

night_features_clean <- night_daily_arrow %>%
  mutate(night_swing_kw = night_peak_kw - night_min_kw) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    n_nights           = n(),
    p95_night_swing_kw = quantile(night_swing_kw, probs = 0.95, na.rm = TRUE),
    avg_night_swing_kw = mean(night_swing_kw, na.rm = TRUE),
    sd_night_mean_kw   = sd(night_mean_kw, na.rm = TRUE),
    spread_night_pk_kw = quantile(night_peak_kw, 0.90, na.rm = TRUE) - quantile(night_peak_kw, 0.10, na.rm = TRUE),
    p95_overnight_kw   = quantile(night_peak_kw, 0.95, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  filter(!is.na(sd_night_mean_kw))

rm(night_daily_arrow); gc()

# ==============================================================================
# STEP 3: Attach Known `df_all` Labels Across ENTIRE `ds` Population
# ==============================================================================
df_cust_flags <- df_all %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    is_ev       = any(is_ev, na.rm = TRUE),
    is_heatpump = any(is_heatpump, na.rm = TRUE),
    is_solar    = any(is_solar, na.rm = TRUE),
    c_level     = as.character(charger_level[!is.na(charger_level)][1]),
    .groups     = "drop"
  ) %>%
  mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER))

features_df <- hr_summary %>%
  group_by(CUSTOMER_NUMBER) %>%
  filter(n() == 24L) %>%
  summarize(
    ds_solar       = if ("is_solar" %in% names(.)) any(as.logical(is_solar), na.rm = TRUE) else NA,
    min_24h_kw     = min(mean_kwh, na.rm = TRUE),
    mean_24h_kw    = mean(mean_kwh, na.rm = TRUE),
    slope_7pm_10pm = cov(hr[hr %in% 20L:22L], mean_kwh[hr %in% 20L:22L]) / var(hr[hr %in% 20L:22L]),
    .groups        = "drop"
  ) %>%
  inner_join(night_features_clean, by = "CUSTOMER_NUMBER") %>%
  left_join(df_cust_flags, by = "CUSTOMER_NUMBER") %>%
  mutate(
    is_ev        = coalesce(is_ev, FALSE),
    is_heatpump  = coalesce(is_heatpump, FALSE),
    is_solar     = coalesce(is_solar, ds_solar, min_24h_kw < -0.05),
    solar_num    = as.integer(is_solar),
    hp_num       = as.integer(is_heatpump),
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
# STEP 4: Fit Full-Dataset Multinomial Regression & Score Full Territory (`ds`)
# ==============================================================================
model_formula <- target_class ~ solar_num + hp_num + mean_24h_kw + slope_7pm_10pm +
                                p95_night_swing_kw + avg_night_swing_kw +
                                sd_night_mean_kw + spread_night_pk_kw + p95_overnight_kw

# Balanced class weights so rare EV classes aren't drowned out by full `ds`
class_counts <- table(features_df$target_class)
obs_weights  <- as.numeric((sum(class_counts) / (length(class_counts) * class_counts))[as.character(features_df$target_class)])

fit_fast_exact <- multinom(
  model_formula,
  data    = features_df,
  weights = obs_weights,
  maxit   = 300,
  trace   = FALSE
)

probs <- predict(fit_fast_exact, newdata = features_df, type = "probs")

features_df <- features_df %>%
  mutate(
    prob_non_ev = probs[, "Non-EV"],
    prob_l1     = probs[, "Level 1"],
    prob_l2     = probs[, "Level 2"],
    prob_any_ev = prob_l1 + prob_l2
  )

# ==============================================================================
# STEP 5: Extract 99th Percentile (`0.99`) Candidates (~1,000 EVs)
# ==============================================================================
unreg <- features_df %>% filter(target_class == "Non-EV")

# Option A: Separate P99 cutoffs for L1 and L2
th_l1 <- quantile(unreg$prob_l1, 0.99, na.rm = TRUE)
th_l2 <- quantile(unreg$prob_l2, 0.99, na.rm = TRUE)

candidates_l2 <- unreg %>% filter(prob_l2 >= th_l2, prob_l2 >= prob_l1) %>% arrange(desc(prob_l2))
candidates_l1 <- unreg %>% filter(prob_l1 >= th_l1, prob_l1 > prob_l2)  %>% arrange(desc(prob_l1))

cat(sprintf("\nP99 Candidates Found -> Level 2: %s | Level 1: %s | Total: %s\n",
            scales::comma(nrow(candidates_l2)),
            scales::comma(nrow(candidates_l1)),
            scales::comma(nrow(candidates_l2) + nrow(candidates_l1))))

# Also check Known EV capture rate at these exact P99 probability thresholds
cat(sprintf("Known EV Capture at P99 Thresholds -> Level 1: %.1f%% | Level 2: %.1f%%\n",
            100 * mean(features_df$prob_l1[features_df$target_class == "Level 1"] >= th_l1),
            100 * mean(features_df$prob_l2[features_df$target_class == "Level 2"] >= th_l2)))

# ==============================================================================
# STEP 6: Plot 24-Hour Verification Curves (Faceted by `is_solar`)
# ==============================================================================
plot_df <- hr_summary %>%
  inner_join(
    features_df %>%
      mutate(
        cohort = case_when(
          target_class == "Level 2"                          ~ "Known Level 2 EV",
          CUSTOMER_NUMBER %in% candidates_l2$CUSTOMER_NUMBER ~ "Level 2 Candidate (P99)",
          target_class == "Level 1"                          ~ "Known Level 1 EV",
          CUSTOMER_NUMBER %in% candidates_l1$CUSTOMER_NUMBER ~ "Level 1 Candidate (P99)",
          TRUE                                               ~ "Remaining Non-EV"
        )
      ) %>%
      select(CUSTOMER_NUMBER, is_solar, cohort),
    by = "CUSTOMER_NUMBER"
  ) %>%
  group_by(is_solar, cohort, hr) %>%
  summarize(avg_kwh = mean(mean_kwh, na.rm = TRUE), .groups = "drop")


plot <- ggplot(plot_df, aes(x = hr, y = avg_kwh, color = cohort, linetype = grepl("Candidate", cohort))) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_line(linewidth = 1.15) +
  facet_wrap(~ is_solar, ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) +
  scale_color_manual(values = c(
    "Known Level 2 EV"        = "#648FFF",
    "Level 2 Candidate (P99)" = "#648FFF",
    "Known Level 1 EV"        = "#DC267F",
    "Level 1 Candidate (P99)" = "#DC267F",
    "Remaining Non-EV"        = "#785EF0"
  ), name = NULL) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  labs(
    title = "EV Classification: Likely vs. Top Unknown Candidates",
    x = "Hour of Day",
    y = "Average Usage (kWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave(plot, filename = "plots/ev_classification_verification.png", width = 8, height = 6, dpi = 300)





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



if (!exists("ev_classification_lookup")) {
  stop("`ev_classification_lookup` not found in memory—run the classification lookup block first.")
}

# 1. Combined CSV of all Level 1 & Level 2 customer IDs (Known + P97 Candidates)
all_ev_chargers_csv <- ev_classification_lookup %>%
  filter(lvl1 == 1L | lvl2 == 1L) %>%
  arrange(desc(lvl2), desc(lvl1), CUSTOMER_NUMBER)

write.csv(all_ev_chargers_csv, "all_lvl1_lvl2_charger_ids.csv", row.names = FALSE)

# 2. Separate CSVs by charger level (optional convenience files)
lvl1_ids_csv <- ev_classification_lookup %>%
  filter(lvl1 == 1L) %>%
  select(CUSTOMER_NUMBER, ev_source)

lvl2_ids_csv <- ev_classification_lookup %>%
  filter(lvl2 == 1L) %>%
  select(CUSTOMER_NUMBER, ev_source)

write.csv(lvl1_ids_csv, "data/lvl1_charger_ids.csv", row.names = FALSE)
write.csv(lvl2_ids_csv, "data/lvl2_charger_ids.csv", row.names = FALSE)

cat(sprintf(
  "Saved %s total EV IDs (%s Level 1, %s Level 2) to `%s`\n",
  scales::comma(nrow(all_ev_chargers_csv)),
  scales::comma(nrow(lvl1_ids_csv)),
  scales::comma(nrow(lvl2_ids_csv)),
  file.path(getwd(), "all_lvl1_lvl2_charger_ids.csv")
))


df_all <- df_all |> filter(source_group !=  "General Sample")
write.csv(df_all, "data/df_all_with_charger_levels.csv", row.names = FALSE)

library(dplyr)
library(tidyr)

# Extract coefficients and standard errors from `fit_fast_exact`
s <- summary(fit_fast_exact)
coefs <- s$coefficients
ses   <- s$standard.errors
zvals <- coefs / ses
pvals <- 2 * (1 - pnorm(abs(zvals)))

# Clean human-readable labels for the appendix
term_labels <- c(
  "(Intercept)"        = "Intercept",
  "solar_num"          = "Solar PV Indicator (1 = Yes)",
  "hp_num"             = "Heat Pump Indicator (1 = Yes)",
  "mean_24h_kw"        = "24-Hour Mean Load (kW)",
  "slope_7pm_10pm"     = "Evening Ramp Slope, 7–10 PM (kW/hr)",
  "p95_night_swing_kw" = "95th Pct. Overnight Swing (kW)",
  "avg_night_swing_kw" = "Mean Overnight Swing (kW)",
  "sd_night_mean_kw"   = "Night-to-Night Mean Load SD (kW)",
  "spread_night_pk_kw" = "Nightly Peak Spread, P90–P10 (kW)",
  "p95_overnight_kw"   = "95th Pct. Overnight Peak Load (kW)"
)

add_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE      ~ ""
  )
}

# Build long table across classes and pivot into side-by-side Appendix format
appendix_tbl <- lapply(rownames(coefs), function(cls) {
  b  <- coefs[cls, ]
  se <- ses[cls, ]
  p  <- pvals[cls, ]
  or <- exp(b)
  or_lo <- exp(b - 1.96 * se)
  or_hi <- exp(b + 1.96 * se)
  
  tibble(
    Term      = names(b),
    Predictor = coalesce(term_labels[names(b)], names(b)),
    Class     = cls,
    Coef_SE   = sprintf("%.3f%s (%.3f)", b, add_stars(p), se),
    OR_CI     = sprintf("%.2f [%.2f, %.2f]", or, or_lo, or_hi),
    P_Value   = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  )
}) %>%
  bind_rows() %>%
  pivot_wider(
    id_cols     = c(Term, Predictor),
    names_from  = Class,
    values_from = c(Coef_SE, OR_CI, P_Value),
    names_glue  = "{Class}_{.value}"
  ) %>%
  select(
    Predictor,
    `Level 1_Coef_SE`, `Level 1_OR_CI`, `Level 1_P_Value`,
    `Level 2_Coef_SE`, `Level 2_OR_CI`, `Level 2_P_Value`
  )

# Add model fit footer rows (AIC, Residual Deviance, Sample Size)
fit_stats <- tibble(
  Predictor          = c("Reference Category", "Observations (N)", "Residual Deviance", "AIC"),
  `Level 1_Coef_SE`  = c("Non-EV", scales::comma(nrow(fit_fast_exact$fitted.values)),
                         sprintf("%.1f", fit_fast_exact$deviance), sprintf("%.1f", fit_fast_exact$AIC)),
  `Level 1_OR_CI`    = "",
  `Level 1_P_Value`  = "",
  `Level 2_Coef_SE`  = "",
  `Level 2_OR_CI`    = "",
  `Level 2_P_Value`  = ""
)

appendix_final <- bind_rows(appendix_tbl, fit_stats)

# 1. Save as CSV so you can open in Excel/Word and paste cleanly
write.csv(appendix_final, "appendix/appendix_multinom_summary.csv", row.names = FALSE)
library(knitr)
library(kableExtra)
# Extract coefficients and standard errors from your fitted multinomial model
smry  <- summary(fit_fast_exact)
coefs <- smry$coefficients
ses   <- smry$standard.errors

# 2. Clean predictor labels using "Overnight Range"
term_labels <- c(
  "(Intercept)"        = "Intercept",
  "solar_num"          = "Solar PV Indicator (1 = Yes)",
  "hp_num"             = "Heat Pump Indicator (1 = Yes)",
  "mean_24h_kw"        = "24-Hour Mean Load (kW)",
  "slope_7pm_10pm"     = "Evening Ramp Slope, 7–10 PM (kW/hr)",
  "p95_night_swing_kw" = "95th Pct. Overnight Range (kW)",
  "avg_night_swing_kw" = "Mean Overnight Range (kW)",
  "sd_night_mean_kw"   = "Night-to-Night Mean Load SD (kW)",
  "spread_night_pk_kw" = "Nightly Peak Spread, P90–P10 (kW)",
  "p95_overnight_kw"   = "95th Pct. Overnight Peak Load (kW)"
)

# 3. Format Coef (SE), Odds Ratio [95% CI], and p-values for Level 1 & Level 2
format_class_cols <- function(cls_name) {
  b  <- coefs[cls_name, ]
  se <- ses[cls_name, ]
  z  <- b / se
  p  <- 2 * (1 - pnorm(abs(z)))
  
  or    <- exp(b)
  ci_lo <- exp(b - 1.96 * se)
  ci_hi <- exp(b + 1.96 * se)
  
  stars <- case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  
  p_fmt <- ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
  
  tibble(
    term     = names(b),
    coef_se  = sprintf("%.3f (%.3f)%s", b, se, stars),
    or_ci    = sprintf("%.2f [%.2f, %.2f]", or, ci_lo, ci_hi),
    p_val    = p_fmt
  )
}

l1_tbl <- format_class_cols("Level 1") %>%
  rename(l1_coef = coef_se, l1_or = or_ci, l1_p = p_val)

l2_tbl <- format_class_cols("Level 2") %>%
  rename(l2_coef = coef_se, l2_or = or_ci, l2_p = p_val)

reg_rows <- l1_tbl %>%
  inner_join(l2_tbl, by = "term") %>%
  mutate(
    Predictor = recode(term, !!!term_labels),
    order_idx = match(term, names(term_labels))
  ) %>%
  arrange(order_idx) %>%
  select(Predictor, l1_coef, l1_or, l1_p, l2_coef, l2_or, l2_p)

# 4. Append Model Fit Statistics at the bottom
n_obs   <- nrow(fit_fast_exact$fitted.values)
ll_val  <- as.numeric(logLik(fit_fast_exact))
aic_val <- AIC(fit_fast_exact)
edf_val <- fit_fast_exact$edf

fit_rows <- tibble(
  Predictor = c("Observations (N)", "Log-Likelihood", "AIC", "Effective Degrees of Freedom"),
  l1_coef   = c(scales::comma(n_obs), sprintf("%.1f", ll_val), sprintf("%.1f", aic_val), as.character(edf_val)),
  l1_or     = rep("—", 4),
  l1_p      = rep("—", 4),
  l2_coef   = rep("—", 4),
  l2_or     = rep("—", 4),
  l2_p      = rep("—", 4)
)

appendix_final <- bind_rows(reg_rows, fit_rows)

# 5. Render publication-style HTML table in RStudio Viewer pane for screenshotting
appendix_kable <- appendix_final %>%
  kbl(
    format    = "html",
    col.names = c("Predictor", "Coef (SE)", "OR [95% CI]", "p-value",
                  "Coef (SE)", "OR [95% CI]", "p-value"),
    align     = c("l", "r", "r", "r", "r", "r", "r"),
    caption   = "Appendix Table: Multinomial Logistic Regression Results for EV Charging Classification"
  ) %>%
  add_header_above(c(" " = 1, "Level 1 EV (vs. Non-EV)" = 3, "Level 2 EV (vs. Non-EV)" = 3)) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(
    font_size         = 13,
    bootstrap_options = c("condensed")
  ) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(nrow(reg_rows), extra_css = "border-bottom: 2px solid #333333;") %>%
  footnote(
    general       = "Reference category = Non-EV. OR = Relative Odds Ratio exp(Coef). * p < 0.05, ** p < 0.01, *** p < 0.001.",
    general_title = "Notes: ",
    footnote_as_chunk = TRUE
  )

print(appendix_kable)


library(dplyr)
library(knitr)
library(kableExtra)

# 1. Compute log predicted probabilities for selected EV accounts
ev_selected_logprobs <- features_df %>%
  bind_cols(as.data.frame(predict(fit_fast_exact, newdata = ., type = "probs"))) %>%
  mutate(
    assigned_class = if ("ev_class" %in% names(.)) as.character(ev_class) else as.character(predict(fit_fast_exact, newdata = .)),
    assigned_src   = if ("ev_source" %in% names(.)) as.character(ev_source) else "Model Classified"
  ) %>%
  filter(assigned_class != "Non-EV") %>%
  mutate(
    log_p_l1       = log(pmax(`Level 1`, .Machine$double.xmin)),
    log_p_l2       = log(pmax(`Level 2`, .Machine$double.xmin)),
    log_p_nonev    = log(pmax(`Non-EV`,  .Machine$double.xmin)),
    assigned_log_p = if_else(assigned_class == "Level 1", log_p_l1, log_p_l2)
  ) %>%
  arrange(desc(assigned_log_p)) %>%
  select(CUSTOMER_NUMBER, assigned_class, assigned_src, log_p_l1, log_p_l2, log_p_nonev)

# 2. Format values for display (round(.x, 3) + 0 eliminates "-0.000")
ev_logprob_display <- ev_selected_logprobs %>%
  mutate(
    across(c(log_p_l1, log_p_l2, log_p_nonev), ~ sprintf("%.3f", round(.x, 3) + 0))
  )

# 3. Render in RStudio Viewer pane for screenshotting
ev_logprob_kable <- ev_logprob_display %>%
  kbl(
    format    = "html",
    col.names = c("Customer ID", "Assigned Class", "Source", "ln P(Level 1)", "ln P(Level 2)", "ln P(Non-EV)"),
    align     = c("l", "c", "l", "r", "r", "r"),
    caption   = "Appendix Table: Log Predicted Probabilities for Selected EV Accounts"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(
    font_size         = 12,
    bootstrap_options = c("condensed", "striped")
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general       = sprintf("Natural log predicted probabilities ln(P) from multinomial logit fit across N = %s selected EV accounts (0.000 = probability ~ 1.0; more negative = lower probability).", scales::comma(nrow(ev_selected_logprobs))),
    general_title = "Notes: ",
    footnote_as_chunk = TRUE
  )

write.csv(ev_logprob_display, "data/ev_selected_logprobs.csv", row.names = FALSE)
print(ev_logprob_kable)