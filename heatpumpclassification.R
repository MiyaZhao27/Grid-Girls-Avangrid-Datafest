# heat pump exploration


library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(arrow)
library(lubridate)

pq <- list.files("cache_clean", pattern = "\\.parquet$", full.names = TRUE)
length(pq)

# 1. Open the dataset and load known customer CSVs
ds <- open_dataset(pq, format = "parquet")
cat(sprintf("Dataset ready: %d rows x %d cols\n", nrow(ds), ncol(ds)))
ds_cols    <- names(ds)
temp_col   <- ds_cols[grepl("temp", tolower(ds_cols))][1]
ds_has_sol <- "is_solar" %in% ds_cols

ev_customers       <- read.csv("data/EV_Heat Pump_Solar PV Customers(EV Customers).csv")
heatpump_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
solar_customers    <- read.csv("data/EV_Heat Pump_Solar PV Customers(PV Customers).csv")

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

ids_ev       <- unique(na.omit(cast_id(ev_customers$CUSTOMER_NUMBER)))
ids_heatpump <- unique(na.omit(cast_id(heatpump_customers$CUSTOMER_NUMBER)))
ids_solar    <- unique(na.omit(cast_id(solar_customers$CUSTOMER_NUMBER)))

# 3. Check for overlaps across the three known technology groups
cat("\n--- Overlap Checks ---\n")
cat("EV & Heat Pump overlap:   ", length(intersect(ids_ev, ids_heatpump)), "\n")
cat("EV & Solar overlap:       ", length(intersect(ids_ev, ids_solar)), "\n")
cat("Heat Pump & Solar overlap:", length(intersect(ids_heatpump, ids_solar)), "\n")

# 4. Pull each known cohort from Arrow and bind together cleanly
# (Pulling unique IDs once avoids duplicating interval rows for multi-tech accounts)
known_ids <- unique(c(ids_ev, ids_heatpump, ids_solar))
cat("\nTotal unique known customer IDs requested:", length(known_ids), "\n")

df_known_raw <- bind_rows(
  ds %>% filter(CUSTOMER_NUMBER %in% ids_ev)       %>% collect() %>% mutate(cohort_pull = "EV"),
  ds %>% filter(CUSTOMER_NUMBER %in% ids_heatpump) %>% collect() %>% mutate(cohort_pull = "Heat Pump"),
  ds %>% filter(CUSTOMER_NUMBER %in% ids_solar)    %>% collect() %>% mutate(cohort_pull = "Solar PV")
) %>%
  # Deduplicate interval rows in case a customer belongs to >1 CSV list
  distinct(CUSTOMER_NUMBER, usage_date, hour_ending, .keep_all = TRUE) %>%
  select(-cohort_pull)

# 5. Clean column types and attach binary + mutually exclusive group labels
df_all <- df_known_raw %>%
  mutate(
    CUSTOMER_NUMBER = cast_id(CUSTOMER_NUMBER),
    hour_ending     = as.numeric(hour_ending),
    hourly_kwh      = as.numeric(hourly_kwh),
    
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
    
    # Binary indicator flags
    is_ev       = CUSTOMER_NUMBER %in% ids_ev,
    is_heatpump = CUSTOMER_NUMBER %in% ids_heatpump,
    is_solar    = CUSTOMER_NUMBER %in% ids_solar,
    
    # Mutually exclusive group label
    tech_count = is_ev + is_heatpump + is_solar,
    source_group = case_when(
      tech_count > 1 ~ "Multiple Tech",
      is_ev          ~ "EV",
      is_heatpump    ~ "Heat Pump",
      is_solar       ~ "Solar PV",
      TRUE           ~ "Other"
    )
  ) %>%
  select(-m, -tech_count)

rm(df_known_raw); gc()

cat(sprintf("`df_all` ready: %s rows across %s unique known customers\n",
            scales::comma(nrow(df_all)),
            scales::comma(n_distinct(df_all$CUSTOMER_NUMBER))))
if (is.na(temp_col)) {
  stop("No temperature column found in `ds`. Available columns: ", paste(ds_cols, collapse = ", "))
}

# ==============================================================================
# STEP 1: Extract Known Heat Pump IDs (from `df_all` or your existing `hp_ids` vector)
# ==============================================================================
if (!exists("hp_ids")) {
  if (exists("df_all") && "is_heatpump" %in% names(df_all)) {
    hp_ids <- df_all %>%
      filter(is_heatpump == TRUE) %>%
      distinct(CUSTOMER_NUMBER) %>%
      pull(CUSTOMER_NUMBER) %>%
      as.character()
  } else {
    stop("Provide your vector of heat pump IDs as `hp_ids` (character vector) or ensure `df_all$is_heatpump` exists.")
  }
} else {
  hp_ids <- unique(as.character(hp_ids))
}

# Also exclude known solar accounts (or flag them) so negative net-export doesn't pull down daytime averages
sol_ids <- character(0)
if (exists("df_all") && "is_solar" %in% names(df_all)) {
  sol_ids <- df_all %>%
    filter(is_solar == TRUE) %>%
    distinct(CUSTOMER_NUMBER) %>%
    pull(CUSTOMER_NUMBER) %>%
    as.character()
}

cat(sprintf("Loaded %s Known Heat Pump IDs.\n", scales::comma(length(hp_ids))))

# ==============================================================================
# STEP 2: Single-Pass Arrow C++ Aggregation by Customer & 2°F Temperature Bin
#         (Zero Arrow hash-joins; collapses raw rows in C++ before collect())
# ==============================================================================
temp_bin_summary <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", "hourly_kwh", temp_col, if (ds_has_sol) "is_solar"))) %>%
  rename(raw_temp = all_of(temp_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    kwh             = cast(hourly_kwh, float64()),
    temp_f          = cast(raw_temp, float64()),
    # Bin outdoor temperature into 2°F buckets (e.g., 20, 22, ..., 94)
    temp_bin        = cast(floor(temp_f / 2.0) * 2.0, int32())
  ) %>%
  filter(!is.na(kwh), !is.na(temp_bin), temp_bin >= 0L, temp_bin <= 100L) %>%
  group_by(across(any_of(c("CUSTOMER_NUMBER", "is_solar", "temp_bin")))) %>%
  summarize(
    n_obs         = n(),
    cust_mean_kwh = mean(kwh, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  collect()

gc()

# ==============================================================================
# STEP 3: Tag `Known Heat Pump` vs. `Unknown` & Filter Out Solar Accounts
# ==============================================================================
curve_data <- temp_bin_summary %>%
  mutate(
    ds_solar = if ("is_solar" %in% names(.)) coalesce(as.logical(is_solar), FALSE) else FALSE,
    is_solar = ds_solar | (CUSTOMER_NUMBER %in% sol_ids),
    cohort   = if_else(CUSTOMER_NUMBER %in% hp_ids, "Likely Heat Pump", "Unknown")
  ) %>%
  # Exclude solar so net-metering export doesn't distort thermal response curves
  filter(!is_solar, n_obs >= 10L) %>%
  group_by(cohort, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  # Keep temperature bins with sufficient customer support in both cohorts
  filter(n_cust >= 15L)

cohort_counts <- temp_bin_summary %>%
  filter(!(if ("is_solar" %in% names(.)) coalesce(as.logical(is_solar), FALSE) else FALSE) & !(CUSTOMER_NUMBER %in% sol_ids)) %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(cohort = if_else(CUSTOMER_NUMBER %in% hp_ids, "Known Heat Pump", "Unknown")) %>%
  count(cohort)

print(cohort_counts)

# ==============================================================================
# STEP 4: Plot Usage vs. Outdoor Temperature (`Known Heat Pump` vs. `Unknown`)
# ==============================================================================
p_thermal_curve <- ggplot(curve_data, aes(x = temp_bin, y = mean_kwh, color = cohort, fill = cohort))  +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c("Likely Heat Pump" = "#d95f02", "Unknown" = "#1b9e77"), name = NULL) +
  scale_fill_manual(values = c("Likely Heat Pump" = "#d95f02", "Unknown" = "#1b9e77"), name = NULL) +
  labs(
    title = "Load by Temperature: Likely Heat Pump vs. Unknown Customers",
    x = "Outdoor Temperature (°F)",
    y = "Average Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 11, face = "bold")
  )

print(p_thermal_curve)
if (!exists("temp_bin_summary")) {
  stop("`temp_bin_summary` not found—run the temperature binning block first.")
}
ggsave("plots/thermal_curve_heatpump_vs_unknown.png", p_thermal_curve, width = 8, height = 5, dpi = 300)

# ==============================================================================
# STEP 1: Measure Each Customer's Winter & Summer Lift vs. 60–70°F Deadband
# ==============================================================================
cust_thermal_metrics <- temp_bin_summary %>%
  mutate(
    ds_solar = if ("is_solar" %in% names(.)) coalesce(as.logical(is_solar), FALSE) else FALSE,
    is_solar = ds_solar | (CUSTOMER_NUMBER %in% sol_ids),
    is_hp    = CUSTOMER_NUMBER %in% hp_ids
  ) %>%
  filter(!is_solar, n_obs >= 10L) %>%
  group_by(CUSTOMER_NUMBER, is_hp) %>%
  summarize(
    n_bins         = n(),
    mild_kw        = mean(cust_mean_kwh[temp_bin >= 60L & temp_bin <= 70L], na.rm = TRUE),
    winter_kw      = mean(cust_mean_kwh[temp_bin <= 50L], na.rm = TRUE),
    summer_kw      = mean(cust_mean_kwh[temp_bin >= 75L], na.rm = TRUE),
    winter_lift_kw = winter_kw - mild_kw,
    summer_lift_kw = summer_kw - mild_kw,
    total_range_kw = max(cust_mean_kwh, na.rm = TRUE) - min(cust_mean_kwh, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  # Require observations across mild, winter, and summer bins
  filter(!is.na(mild_kw), !is.na(winter_kw), !is.na(summer_kw), n_bins >= 12L) %>%
  mutate(
    is_flat_curve = !is_hp &
                    abs(winter_lift_kw) < 0.25 &
                    abs(summer_lift_kw) < 0.25 &
                    total_range_kw < 0.50,
    thermal_group = case_when(
      is_hp         ~ "Known Heat Pump",
      is_flat_curve ~ "Flat Curve (No Electric Heat/AC)",
      TRUE          ~ "Thermally Sensitive Unknown (AC / Unreg Heat)"
    )
  )

# Inspect group counts and average winter/summer lift per cohort
cust_thermal_metrics %>%
  group_by(thermal_group) %>%
  summarize(
    n_customers        = n(),
    avg_mild_kw        = round(mean(mild_kw, na.rm = TRUE), 3),
    avg_winter_lift_kw = round(mean(winter_lift_kw, na.rm = TRUE), 3),
    avg_summer_lift_kw = round(mean(summer_lift_kw, na.rm = TRUE), 3),
    .groups            = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 2: Re-Plot Thermal Response Curves Across the 3 Separated Cohorts
# ==============================================================================
curve_3group <- temp_bin_summary %>%
  inner_join(cust_thermal_metrics %>% select(CUSTOMER_NUMBER, thermal_group), by = "CUSTOMER_NUMBER") %>%
  filter(n_obs >= 10L) %>%
  group_by(thermal_group, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L) %>%
  mutate(
    thermal_group = factor(thermal_group, levels = c(
      "Known Heat Pump",
      "Thermally Sensitive Unknown (AC / Unreg Heat)",
      "Flat Curve (No Electric Heat/AC)"
    ))
  )

p_thermal_3group <- ggplot(curve_3group, aes(x = temp_bin, y = mean_kwh, color = thermal_group, fill = thermal_group)) +
  annotate("rect", xmin = 60, xmax = 70, ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "gray50") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c(
    "Known Heat Pump"                              = "#d95f02",
    "Thermally Sensitive Unknown (AC / Unreg Heat)" = "#7570b3",
    "Flat Curve (No Electric Heat/AC)"             = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Known Heat Pump"                              = "#d95f02",
    "Thermally Sensitive Unknown (AC / Unreg Heat)" = "#7570b3",
    "Flat Curve (No Electric Heat/AC)"             = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "Thermal Load Curves: Isolating Flat-Curve Accounts from HVAC Loads",
    subtitle = "Flat curve = winter lift < 0.25 kW, summer lift < 0.25 kW, total range < 0.50 kW",
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold")
  )

print(p_thermal_3group)

# Extract the flat-curve customer IDs for downstream filtering
flat_curve_ids <- cust_thermal_metrics %>%
  filter(is_flat_curve) %>%
  pull(CUSTOMER_NUMBER)


if (!exists("cust_thermal_metrics") || !exists("temp_bin_summary")) {
  stop("`cust_thermal_metrics` or `temp_bin_summary` not found—run the thermal binning block first.")
}

# ==============================================================================
# STEP 1: Apply Extra-Loose Threshold (< 0.50 kW lift, < 1.00 kW range)
# ==============================================================================
cust_thermal_xloose <- cust_thermal_metrics %>%
  mutate(
    is_flat_curve = !is_hp &
                    abs(winter_lift_kw) < 0.50 &
                    abs(summer_lift_kw) < 0.50 &
                    total_range_kw < 1.00,
    thermal_group = case_when(
      is_hp         ~ "Known Heat Pump",
      is_flat_curve ~ "Flat Curve (Extra-Loose: < 0.50 kW lift)",
      TRUE          ~ "Thermally Sensitive Unknown (AC / Unreg Heat)"
    )
  )

# Print updated cohort counts & average winter/summer lift
cust_thermal_xloose %>%
  group_by(thermal_group) %>%
  summarize(
    n_customers        = n(),
    avg_mild_kw        = round(mean(mild_kw, na.rm = TRUE), 3),
    avg_winter_lift_kw = round(mean(winter_lift_kw, na.rm = TRUE), 3),
    avg_summer_lift_kw = round(mean(summer_lift_kw, na.rm = TRUE), 3),
    .groups            = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 2: Plot Thermal Load Curves with the Extra-Loose Flat-Curve Cohort
# ==============================================================================
curve_xloose <- temp_bin_summary %>%
  inner_join(cust_thermal_xloose %>% select(CUSTOMER_NUMBER, thermal_group), by = "CUSTOMER_NUMBER") %>%
  filter(n_obs >= 10L) %>%
  group_by(thermal_group, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L) %>%
  mutate(
    thermal_group = factor(thermal_group, levels = c(
      "Known Heat Pump",
      "Thermally Sensitive Unknown (AC / Unreg Heat)",
      "Flat Curve (Extra-Loose: < 0.50 kW lift)"
    ))
  )

p_thermal_xloose <- ggplot(curve_xloose, aes(x = temp_bin, y = mean_kwh, color = thermal_group, fill = thermal_group)) +
  annotate("rect", xmin = 60, xmax = 70, ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "gray50") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c(
    "Known Heat Pump"                              = "#d95f02",
    "Thermally Sensitive Unknown (AC / Unreg Heat)" = "#7570b3",
    "Flat Curve (Extra-Loose: < 0.50 kW lift)"     = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Known Heat Pump"                              = "#d95f02",
    "Thermally Sensitive Unknown (AC / Unreg Heat)" = "#7570b3",
    "Flat Curve (Extra-Loose: < 0.50 kW lift)"     = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "Thermal Load Curves: Isolating Flat-Curve Accounts (Extra-Loose Threshold)",
    subtitle = "Flat curve = winter lift < 0.50 kW, summer lift < 0.50 kW, total range < 1.00 kW",
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold")
  )

print(p_thermal_xloose)

# Update `flat_curve_ids` with the extra-loose cohort
flat_curve_ids <- cust_thermal_xloose %>%
  filter(is_flat_curve) %>%
  pull(CUSTOMER_NUMBER)


library(dplyr)
library(ggplot2)

if (!exists("temp_bin_summary") || !exists("cust_thermal_xloose")) {
  stop("`temp_bin_summary` or `cust_thermal_xloose` not found—run the previous thermal blocks first.")
}

# ==============================================================================
# STEP 1: Compute Customer Linear Slope Across 10°F–40°F Bins (`d(kWh)/d(°F)`)
# ==============================================================================
winter_slopes_10_40 <- temp_bin_summary %>%
  filter(n_obs >= 10L, temp_bin >= 10L, temp_bin <= 40L) %>%
  group_by(CUSTOMER_NUMBER) %>%
  filter(n() >= 4L) %>% # Require at least 4 distinct 2°F bins between 10°F and 40°F
  summarize(
    n_winter_bins    = n(),
    slope_10_40_f    = cov(temp_bin, cust_mean_kwh) / var(temp_bin),
    .groups          = "drop"
  )

# ==============================================================================
# STEP 2: Benchmark Known Heat Pump Slope & Apply Threshold to Unknowns
# ==============================================================================
cust_classified_4group <- cust_thermal_xloose %>%
  left_join(winter_slopes_10_40, by = "CUSTOMER_NUMBER")

# Print Known Heat Pump 10–40°F slope distribution so you can see where the cutoff sits
hp_slope_stats <- cust_classified_4group %>%
  filter(is_hp, !is.na(slope_10_40_f)) %>%
  summarize(
    n_hp        = n(),
    p25_slope   = round(quantile(slope_10_40_f, 0.25), 4),
    median_slope= round(median(slope_10_40_f), 4),
    mean_slope  = round(mean(slope_10_40_f), 4),
    p75_slope   = round(quantile(slope_10_40_f, 0.75), 4)
  )
cat("Known Heat Pump 10–40°F Slope Benchmarks (kWh per °F):\n")
print(hp_slope_stats)

# Set winter slope threshold (more negative than -0.025 kWh/°F = +0.50 kW lift over a 20°F drop)
hp_slope_cutoff <- -0.025

cust_classified_4group <- cust_classified_4group %>%
  mutate(
    thermal_group_4 = case_when(
      is_hp ~ "Known Heat Pump",
      !is_flat_curve & !is.na(slope_10_40_f) & slope_10_40_f <= hp_slope_cutoff ~ "Likely Heat Pump",
      is_flat_curve ~ "Flat Curve (Extra-Loose: < 0.50 kW lift)",
      TRUE ~ "Remaining Unknown (A/C-Only / Other)"
    )
  )

# Summary table of counts and average thermal metrics across all 4 cohorts
cust_classified_4group %>%
  group_by(thermal_group_4) %>%
  summarize(
    n_customers        = n(),
    avg_slope_10_40    = round(mean(slope_10_40_f, na.rm = TRUE), 4),
    avg_winter_lift_kw = round(mean(winter_lift_kw, na.rm = TRUE), 3),
    avg_summer_lift_kw = round(mean(summer_lift_kw, na.rm = TRUE), 3),
    .groups            = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 3: Plot Thermal Load Curves Across All 4 Cohorts
# ==============================================================================
curve_4group <- temp_bin_summary %>%
  inner_join(cust_classified_4group %>% select(CUSTOMER_NUMBER, thermal_group_4), by = "CUSTOMER_NUMBER") %>%
  filter(n_obs >= 10L) %>%
  group_by(thermal_group_4, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L) %>%
  mutate(
    thermal_group_4 = factor(thermal_group_4, levels = c(
      "Known Heat Pump",
      "Likely Heat Pump",
      "Remaining Unknown (A/C-Only / Other)",
      "Flat Curve (Extra-Loose: < 0.50 kW lift)"
    ))
  )

p_thermal_4group <- ggplot(curve_4group, aes(
  x        = temp_bin,
  y        = mean_kwh,
  color    = thermal_group_4,
  fill     = thermal_group_4,
  linetype = thermal_group_4 == "Likely Heat Pump"
)) +
  # Highlight the 10–40°F winter slope window and 60–70°F mild deadband
  annotate("rect", xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf, alpha = 0.07, fill = "#2b8cbe") +
  annotate("text", x = 25, y = Inf, label = "10–40°F\nSlope Window", vjust = 1.4, size = 3.1, color = "#2b8cbe") +
  annotate("rect", xmin = 60, xmax = 70, ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "gray50") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c(
    "Known Heat Pump"                          = "#d95f02",
    "Likely Heat Pump"                         = "#e7298a",
    "Remaining Unknown (A/C-Only / Other)"     = "#7570b3",
    "Flat Curve (Extra-Loose: < 0.50 kW lift)" = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Known Heat Pump"                          = "#d95f02",
    "Likely Heat Pump"                         = "#e7298a",
    "Remaining Unknown (A/C-Only / Other)"     = "#7570b3",
    "Flat Curve (Extra-Loose: < 0.50 kW lift)" = "#1b9e77"
  ), name = NULL) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  labs(
    title = "Thermal Load Curves: Known vs. Likely Heat Pumps (10–40°F Winter Slope)",
    subtitle = sprintf("Likely Heat Pump = non-flat unknown with 10–40°F slope <= %.3f kWh/°F", hp_slope_cutoff),
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold")
  )

print(p_thermal_4group)

# Extract `likely_hp_ids` for downstream verification or export
likely_hp_ids <- cust_classified_4group %>%
  filter(thermal_group_4 == "Likely Heat Pump") %>%
  pull(CUSTOMER_NUMBER)

library(dplyr)
library(ggplot2)

if (!exists("temp_bin_summary")) {
  stop("`temp_bin_summary` not found—run the temperature binning block first.")
}

# ==============================================================================
# STEP 1: Compute Customer Slopes for 10–40°F (Winter) & 80–90°F (Summer)
# ==============================================================================
slopes_2win <- temp_bin_summary %>%
  mutate(
    ds_solar = if ("is_solar" %in% names(.)) coalesce(as.logical(is_solar), FALSE) else FALSE,
    is_solar = ds_solar | (CUSTOMER_NUMBER %in% sol_ids),
    is_hp    = CUSTOMER_NUMBER %in% hp_ids
  ) %>%
  filter(!is_solar, n_obs >= 10L) %>%
  group_by(CUSTOMER_NUMBER, is_hp) %>%
  summarize(
    n_win_bins    = sum(temp_bin >= 10L & temp_bin <= 40L),
    n_sum_bins    = sum(temp_bin >= 80L & temp_bin <= 90L),
    # 1. Winter slope (10–40°F): negative = usage rises as temp drops
    slope_10_40_f = if_else(
      n_win_bins >= 4L,
      cov(temp_bin[temp_bin >= 10L & temp_bin <= 40L], cust_mean_kwh[temp_bin >= 10L & temp_bin <= 40L]) /
        var(temp_bin[temp_bin >= 10L & temp_bin <= 40L]),
      NA_real_
    ),
    # 2. Summer slope (80–90°F): positive = usage rises as temp climbs
    slope_80_90_f = if_else(
      n_sum_bins >= 3L,
      cov(temp_bin[temp_bin >= 80L & temp_bin <= 90L], cust_mean_kwh[temp_bin >= 80L & temp_bin <= 90L]) /
        var(temp_bin[temp_bin >= 80L & temp_bin <= 90L]),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(slope_10_40_f), !is.na(slope_80_90_f))

# Winsorize extreme 0.5% outliers so single noisy meters don't distort Ward's linkage
clip_pct <- function(x, p = 0.005) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

slopes_2win <- slopes_2win %>%
  mutate(
    w_slope_win = clip_pct(slope_10_40_f),
    w_slope_sum = clip_pct(slope_80_90_f)
  )

# ==============================================================================
# STEP 2: Hierarchical Clustering (`ward.D2`, k = 3) on Memory-Safe Sample
#         (Prevents O(N^2) distance matrix crash, then projects to all accounts)
# ==============================================================================
set.seed(42)

# Standardize both slopes using full-territory mean & SD
win_mu <- mean(slopes_2win$w_slope_win); win_sd <- sd(slopes_2win$w_slope_win)
sum_mu <- mean(slopes_2win$w_slope_sum); sum_sd <- sd(slopes_2win$w_slope_sum)

slopes_2win <- slopes_2win %>%
  mutate(
    z_win = (w_slope_win - win_mu) / win_sd,
    z_sum = (w_slope_sum - sum_mu) / sum_sd
  )

# Sample up to 6,000 customers (including all Known Heat Pumps) for `hclust`
hp_rows  <- slopes_2win %>% filter(is_hp)
unk_rows <- slopes_2win %>% filter(!is_hp) %>% slice_sample(n = min(5000L, nrow(.)))
hc_sample <- bind_rows(hp_rows, unk_rows)

dist_mat <- dist(hc_sample %>% select(z_win, z_sum), method = "euclidean")
hc_fit   <- hclust(dist_mat, method = "ward.D2")
hc_sample$raw_clust <- cutree(hc_fit, k = 3L)

# Compute the 3 cluster centroids in standardized (z_win, z_sum) space
centroids <- hc_sample %>%
  group_by(raw_clust) %>%
  summarize(
    c_z_win       = mean(z_win),
    c_z_sum       = mean(z_sum),
    mean_win_raw  = mean(slope_10_40_f),
    mean_sum_raw  = mean(slope_80_90_f),
    hp_share      = mean(is_hp),
    .groups       = "drop"
  )

# Auto-label the 3 clusters by physical signature:
# 1. Heat Pump = most negative winter slope (`mean_win_raw` lowest)
# 2. A/C       = highest summer slope among remaining (`mean_sum_raw` highest)
# 3. Nothing   = flattest on both
hp_cid  <- centroids %>% slice_min(mean_win_raw, n = 1, with_ties = FALSE) %>% pull(raw_clust)
ac_cid  <- centroids %>% filter(raw_clust != hp_cid) %>% slice_max(mean_sum_raw, n = 1, with_ties = FALSE) %>% pull(raw_clust)
non_cid <- setdiff(centroids$raw_clust, c(hp_cid, ac_cid))

centroids <- centroids %>%
  mutate(
    cluster_label = case_when(
      raw_clust == hp_cid  ~ "Heat Pump",
      raw_clust == ac_cid  ~ "A/C",
      raw_clust == non_cid ~ "Nothing"
    )
  )

print(centroids %>% select(cluster_label, mean_win_raw, mean_sum_raw, hp_share))

# ==============================================================================
# STEP 3: Assign Every Customer in Territory to Nearest Hierarchical Centroid
# ==============================================================================
assign_nearest <- function(zw, zs, cent_df) {
  d_mat <- sapply(seq_len(nrow(cent_df)), function(i) {
    (zw - cent_df$c_z_win[i])^2 + (zs - cent_df$c_z_sum[i])^2
  })
  cent_df$cluster_label[max.col(-d_mat, ties.method = "first")]
}

slopes_2win <- slopes_2win %>%
  mutate(
    hclust_class = factor(
      assign_nearest(z_win, z_sum, centroids),
      levels = c("Heat Pump", "A/C", "Nothing")
    )
  )

# Summary table: counts, Known HP capture rate, and average slopes per class
slopes_2win %>%
  group_by(hclust_class) %>%
  summarize(
    n_customers      = n(),
    known_hp_inside  = sum(is_hp),
    avg_slope_10_40  = round(mean(slope_10_40_f), 4),
    avg_slope_80_90  = round(mean(slope_80_90_f), 4),
    .groups          = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 4: Plot 1 — 2D Slope Scatter (`10–40°F` vs. `80–90°F`) Colored by Cluster
# ==============================================================================
p_slope_scatter <- ggplot(
  slopes_2win %>% slice_sample(n = min(15000L, nrow(.))),
  aes(x = w_slope_win, y = w_slope_sum, color = hclust_class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45") +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_point(
    data = centroids,
    aes(x = mean_win_raw, y = mean_sum_raw),
    color = "black", fill = "yellow", shape = 23, size = 4, stroke = 1.2
  ) +
  scale_color_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = "Hierarchical Cluster") +
  labs(
    title = "Hierarchical Clustering (Ward's k = 3): Winter vs. Summer Thermal Slopes",
    subtitle = "Diamonds = cluster centroids | Heat Pump (negative 10–40°F), A/C (positive 80–90°F), Nothing (near origin)",
    x = "Winter Slope: 10–40°F (kWh / °F)",
    y = "Summer Slope: 80–90°F (kWh / °F)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_slope_scatter)

# ==============================================================================
# STEP 5: Plot 2 — Full Temperature Response Curves Across the 3 Clusters
# ==============================================================================
curve_hc3 <- temp_bin_summary %>%
  inner_join(slopes_2win %>% select(CUSTOMER_NUMBER, hclust_class), by = "CUSTOMER_NUMBER") %>%
  filter(n_obs >= 10L) %>%
  group_by(hclust_class, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L)

p_hc3_curves <- ggplot(curve_hc3, aes(x = temp_bin, y = mean_kwh, color = hclust_class, fill = hclust_class)) +
  annotate("rect", xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#d95f02") +
  annotate("rect", xmin = 80, xmax = 90, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#7570b3") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "Thermal Load Curves by Hierarchical Cluster (Heat Pump, A/C, Nothing)",
    subtitle = "Clustered on standardized 10–40°F winter slope & 80–90°F summer slope",
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 11, face = "bold")
  )

print(p_hc3_curves)


library(arrow)
library(dplyr)

# Verify the 3-cluster result object exists
if (!exists("slopes_3feat") && !exists("slopes_2win")) {
  stop("Neither `slopes_3feat` nor `slopes_2win` found in memory—run the 3-cluster `hclust` script first.")
}

thermal_df <- if (exists("slopes_3feat")) slopes_3feat else slopes_2win

# ==============================================================================
# 1. Extract Classified ID Vectors (`Heat Pump`, `A/C`, `Nothing`)
# ==============================================================================
hp_class_ids   <- thermal_df %>% filter(hclust_class == "Heat Pump") %>% pull(CUSTOMER_NUMBER) %>% as.character()
ac_class_ids   <- thermal_df %>% filter(hclust_class == "A/C")       %>% pull(CUSTOMER_NUMBER) %>% as.character()
none_class_ids <- thermal_df %>% filter(hclust_class == "Nothing")   %>% pull(CUSTOMER_NUMBER) %>% as.character()

# ==============================================================================
# 2. Apply Classification Lazily Across Full Arrow Dataset (`ds`) & Summarize
#    (Uses C++ set-membership predicates—zero joins, zero memory spikes)
# ==============================================================================
ds_thermal_summary <- ds %>%
  mutate(
    cust_chr      = cast(CUSTOMER_NUMBER, utf8()),
    thermal_class = case_when(
      cust_chr %in% hp_class_ids   ~ "Heat Pump",
      cust_chr %in% ac_class_ids   ~ "A/C",
      cust_chr %in% none_class_ids ~ "Nothing",
      TRUE                         ~ "Unscored / Solar"
    )
  ) %>%
  group_by(thermal_class) %>%
  summarize(
    n_customers = n_distinct(cust_chr),
    n_rows      = n(),
    .groups     = "drop"
  ) %>%
  collect() %>%
  mutate(
    pct_customers = round(100 * n_customers / sum(n_customers), 2),
    pct_rows      = round(100 * n_rows / sum(n_rows), 2)
  ) %>%
  arrange(desc(n_customers))

print(ds_thermal_summary)

# ==============================================================================
# 3. Also Print Breakdown of Scored Non-Solar Cohort Alone (Excludes Unscored/Solar)
# ==============================================================================
ds_thermal_summary %>%
  filter(thermal_class != "Unscored / Solar") %>%
  mutate(
    pct_of_scored_cust = round(100 * n_customers / sum(n_customers), 2)
  ) %>%
  select(thermal_class, n_customers, pct_of_scored_cust, n_rows) %>%
  print()

# makes sense that this is about 50/50!!

library(arrow)
library(dplyr)
library(ggplot2)

if (!exists("temp_bin_summary")) {
  stop("`temp_bin_summary` not found—run the temperature binning block first.")
}

ds_cols    <- names(ds)
temp_col   <- ds_cols[grepl("temp", tolower(ds_cols))][1]
ds_has_sol <- "is_solar" %in% ds_cols

# ==============================================================================
# STEP 1: Fast Single-Pass Arrow C++ Aggregation for Hot-Day P95 (`temp_f >= 80°F`)
# ==============================================================================
hot_p95_df <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", "hourly_kwh", temp_col))) %>%
  rename(raw_temp = all_of(temp_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    kwh             = cast(hourly_kwh, float64()),
    temp_f          = cast(raw_temp, float64())
  ) %>%
  filter(!is.na(kwh), !is.na(temp_f), temp_f >= 80.0) %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    n_hot_hrs  = n(),
    p95_hot_kw = quantile(kwh, probs = 0.95, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  collect() %>%
  filter(n_hot_hrs >= 20L, !is.na(p95_hot_kw))

gc()

# ==============================================================================
# STEP 2: Combine Winter Slope (10–40°F), Summer Slope (80–90°F), & Hot P95
# ==============================================================================
slopes_3feat <- temp_bin_summary %>%
  mutate(
    ds_solar = if ("is_solar" %in% names(.)) coalesce(as.logical(is_solar), FALSE) else FALSE,
    is_solar = ds_solar | (CUSTOMER_NUMBER %in% sol_ids),
    is_hp    = CUSTOMER_NUMBER %in% hp_ids
  ) %>%
  filter(!is_solar, n_obs >= 10L) %>%
  group_by(CUSTOMER_NUMBER, is_hp) %>%
  summarize(
    n_win_bins    = sum(temp_bin >= 10L & temp_bin <= 40L),
    n_sum_bins    = sum(temp_bin >= 80L & temp_bin <= 90L),
    slope_10_40_f = if_else(
      n_win_bins >= 4L,
      cov(temp_bin[temp_bin >= 10L & temp_bin <= 40L], cust_mean_kwh[temp_bin >= 10L & temp_bin <= 40L]) /
        var(temp_bin[temp_bin >= 10L & temp_bin <= 40L]),
      NA_real_
    ),
    slope_80_90_f = if_else(
      n_sum_bins >= 3L,
      cov(temp_bin[temp_bin >= 80L & temp_bin <= 90L], cust_mean_kwh[temp_bin >= 80L & temp_bin <= 90L]) /
        var(temp_bin[temp_bin >= 80L & temp_bin <= 90L]),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  inner_join(hot_p95_df %>% select(CUSTOMER_NUMBER, p95_hot_kw), by = "CUSTOMER_NUMBER") %>%
  filter(!is.na(slope_10_40_f), !is.na(slope_80_90_f), !is.na(p95_hot_kw))

clip_pct <- function(x, p = 0.005) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

slopes_3feat <- slopes_3feat %>%
  mutate(
    w_slope_win = clip_pct(slope_10_40_f),
    w_slope_sum = clip_pct(slope_80_90_f),
    w_p95_hot   = clip_pct(p95_hot_kw)
  )

# ==============================================================================
# STEP 3: Ward's Hierarchical Clustering (`k = 3`) on 3 Standardized Features
# ==============================================================================
set.seed(42)

win_mu <- mean(slopes_3feat$w_slope_win); win_sd <- sd(slopes_3feat$w_slope_win)
sum_mu <- mean(slopes_3feat$w_slope_sum); sum_sd <- sd(slopes_3feat$w_slope_sum)
hot_mu <- mean(slopes_3feat$w_p95_hot);   hot_sd <- sd(slopes_3feat$w_p95_hot)

slopes_3feat <- slopes_3feat %>%
  mutate(
    z_win = (w_slope_win - win_mu) / win_sd,
    z_sum = (w_slope_sum - sum_mu) / sum_sd,
    z_hot = (w_p95_hot   - hot_mu) / hot_sd
  )

hp_rows   <- slopes_3feat %>% filter(is_hp)
unk_rows  <- slopes_3feat %>% filter(!is_hp) %>% slice_sample(n = min(5000L, nrow(.)))
hc_sample <- bind_rows(hp_rows, unk_rows)

dist_mat <- dist(hc_sample %>% select(z_win, z_sum, z_hot), method = "euclidean")
hc_fit   <- hclust(dist_mat, method = "ward.D2")
hc_sample$raw_clust <- cutree(hc_fit, k = 3L)

centroids <- hc_sample %>%
  group_by(raw_clust) %>%
  summarize(
    c_z_win      = mean(z_win),
    c_z_sum      = mean(z_sum),
    c_z_hot      = mean(z_hot),
    mean_win_raw = mean(slope_10_40_f),
    mean_sum_raw = mean(slope_80_90_f),
    mean_hot_p95 = mean(p95_hot_kw),
    hp_share     = mean(is_hp),
    .groups      = "drop"
  )

# Auto-label clusters:
# 1. Heat Pump = most negative winter slope (`mean_win_raw` lowest)
# 2. A/C       = highest combined summer slope + hot P95 among remaining
# 3. Nothing   = lowest summer slope & hot P95
hp_cid  <- centroids %>% slice_min(mean_win_raw, n = 1, with_ties = FALSE) %>% pull(raw_clust)
ac_cid  <- centroids %>%
  filter(raw_clust != hp_cid) %>%
  slice_max(c_z_sum + c_z_hot, n = 1, with_ties = FALSE) %>%
  pull(raw_clust)
non_cid <- setdiff(centroids$raw_clust, c(hp_cid, ac_cid))

centroids <- centroids %>%
  mutate(
    cluster_label = case_when(
      raw_clust == hp_cid  ~ "Heat Pump",
      raw_clust == ac_cid  ~ "A/C",
      raw_clust == non_cid ~ "Nothing"
    )
  )

print(centroids %>% select(cluster_label, mean_win_raw, mean_sum_raw, mean_hot_p95, hp_share))

# ==============================================================================
# STEP 4: Project All Customers to Nearest 3D Centroid & Compare Counts
# ==============================================================================
assign_nearest_3d <- function(zw, zs, zh, cent_df) {
  d_mat <- sapply(seq_len(nrow(cent_df)), function(i) {
    (zw - cent_df$c_z_win[i])^2 + (zs - cent_df$c_z_sum[i])^2 + (zh - cent_df$c_z_hot[i])^2
  })
  cent_df$cluster_label[max.col(-d_mat, ties.method = "first")]
}

slopes_3feat <- slopes_3feat %>%
  mutate(
    hclust_class = factor(
      assign_nearest_3d(z_win, z_sum, z_hot, centroids),
      levels = c("Heat Pump", "A/C", "Nothing")
    )
  )

slopes_3feat %>%
  group_by(hclust_class) %>%
  summarize(
    n_customers     = n(),
    known_hp_inside = sum(is_hp),
    avg_slope_10_40 = round(mean(slope_10_40_f), 4),
    avg_slope_80_90 = round(mean(slope_80_90_f), 4),
    avg_p95_hot_kw  = round(mean(p95_hot_kw), 3),
    .groups         = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 5: Plot Updated Thermal Curves Across `Heat Pump`, `A/C`, and `Nothing`
# ==============================================================================
curve_hc3_hot <- temp_bin_summary %>%
  inner_join(slopes_3feat %>% select(CUSTOMER_NUMBER, hclust_class), by = "CUSTOMER_NUMBER") %>%
  filter(n_obs >= 10L) %>%
  group_by(hclust_class, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L)

p_hc3_hot <- ggplot(curve_hc3_hot, aes(x = temp_bin, y = mean_kwh, color = hclust_class, fill = hclust_class)) +
  annotate("rect", xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#d95f02") +
  annotate("rect", xmin = 80, xmax = 90, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#7570b3") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_color_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "Thermal Load Curves: 3-Cluster `hclust` with Hot-Day P95 Added",
    subtitle = "Features: 10–40°F winter slope, 80–90°F summer slope, & >= 80°F hot-day P95 kW",
    x = "Outdoor Temperature (°F)",
    y = "Average Hourly Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 11, face = "bold")
  )

print(p_hc3_hot)

library(arrow)
library(dplyr)
library(ggplot2)

if (!exists("slopes_3feat") || !exists("temp_bin_summary")) {
  stop("`slopes_3feat` or `temp_bin_summary` not found—run the 3-feature `hclust` and temperature binning blocks first.")
}

ds_cols  <- names(ds)
date_col <- intersect(c("usage_date", "date", "DATE", "USAGE_DATE", "read_date", "bill_date"), ds_cols)[1]
temp_col <- ds_cols[grepl("temp", tolower(ds_cols))][1]

if (is.na(date_col) || is.na(temp_col)) {
  stop("Could not detect date or temperature column in `ds`.")
}

# ==============================================================================
# STEP 1: Split `Nothing` Cluster into Truly Flat (Loose Threshold) vs. Slight Hot Increase
# ==============================================================================
nothing_subgroups <- temp_bin_summary %>%
  inner_join(
    slopes_3feat %>%
      filter(hclust_class == "Nothing") %>%
      select(CUSTOMER_NUMBER, slope_80_90_f, p95_hot_kw),
    by = "CUSTOMER_NUMBER"
  ) %>%
  filter(n_obs >= 10L) %>%
  group_by(CUSTOMER_NUMBER, slope_80_90_f, p95_hot_kw) %>%
  summarize(
    mild_kw        = mean(cust_mean_kwh[temp_bin >= 60L & temp_bin <= 70L], na.rm = TRUE),
    winter_kw      = mean(cust_mean_kwh[temp_bin <= 50L], na.rm = TRUE),
    summer_kw      = mean(cust_mean_kwh[temp_bin >= 75L], na.rm = TRUE),
    winter_lift_kw = winter_kw - mild_kw,
    summer_lift_kw = summer_kw - mild_kw,
    total_range_kw = max(cust_mean_kwh, na.rm = TRUE) - min(cust_mean_kwh, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  filter(!is.na(mild_kw), !is.na(summer_kw)) %>%
  mutate(
    # Loose flat-curve threshold (< 0.35 kW winter/summer lift & < 0.70 kW range)
    is_truly_flat = abs(coalesce(winter_lift_kw, 0)) < 0.35 &
                    abs(summer_lift_kw) < 0.35 &
                    total_range_kw < 0.70,
    nothing_subcohort = if_else(
      is_truly_flat,
      "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)",
      "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)"
    )
  )

# Print customer counts & thermal summary for the two sub-cohorts
nothing_subgroups %>%
  group_by(nothing_subcohort) %>%
  summarize(
    n_customers        = n(),
    avg_mild_kw        = round(mean(mild_kw, na.rm = TRUE), 3),
    avg_summer_lift_kw = round(mean(summer_lift_kw, na.rm = TRUE), 3),
    avg_slope_80_90    = round(mean(slope_80_90_f, na.rm = TRUE), 4),
    avg_p95_hot_kw     = round(mean(p95_hot_kw, na.rm = TRUE), 3),
    .groups            = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 2: Extract Hot Days (`>= 80°F`) Date Vector & Aggregate 24-Hour Profiles in C++
# ==============================================================================
nothing_ids_all <- as.character(nothing_subgroups$CUSTOMER_NUMBER)

hot_dt_vec <- ds %>%
  select(all_of(c(date_col, temp_col))) %>%
  rename(raw_dt = all_of(date_col), raw_temp = all_of(temp_col)) %>%
  mutate(dt = cast(raw_dt, date32()), temp_f = cast(raw_temp, float64())) %>%
  filter(!is.na(dt), !is.na(temp_f)) %>%
  group_by(dt) %>%
  summarize(
    day_max_temp = max(temp_f, na.rm = TRUE),
    day_avg_temp = mean(temp_f, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  collect() %>%
  # Hot summer days where peak temperature reaches >= 80°F
  filter(day_max_temp >= 80) %>%
  pull(dt)

cat(sprintf("Hot days (daily max >= 80°F): %d days\n", length(hot_dt_vec)))

nothing_hot_hourly <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh"))) %>%
  rename(raw_dt = all_of(date_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64())
  ) %>%
  filter(
    CUSTOMER_NUMBER %in% nothing_ids_all,
    dt %in% hot_dt_vec,
    !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)
  ) %>%
  group_by(CUSTOMER_NUMBER, hr) %>%
  summarize(cust_hot_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  collect()

gc()

# ==============================================================================
# STEP 3: Plot Hot-Day 24-Hour Usage Profiles Comparing Both `Nothing` Sub-Cohorts
# ==============================================================================
plot_nothing_hot <- nothing_hot_hourly %>%
  inner_join(
    nothing_subgroups %>%
      mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
      select(CUSTOMER_NUMBER, nothing_subcohort),
    by = "CUSTOMER_NUMBER"
  ) %>%
  group_by(nothing_subcohort, hr) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_hot_hr_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_hot_hr_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(
    nothing_subcohort = factor(nothing_subcohort, levels = c(
      "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)",
      "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"
    ))
  )

p_nothing_hot_24h <- ggplot(plot_nothing_hot, aes(x = hr, y = mean_kwh, color = nothing_subcohort, fill = nothing_subcohort)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) +
  scale_color_manual(values = c(
    "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)" = "#d95f02",
    "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"            = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)" = "#d95f02",
    "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"            = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "`Nothing` Cluster on Hot Days (Daily Max >= 80°F): Truly Flat vs. Slight Hot Increase",
    subtitle = "Comparing loose flat-curve accounts (< 0.35 kW lift) against `Nothing` accounts with >= 0.35 kW lift",
    x = "Hour Ending",
    y = "Average Hourly Usage on Hot Days (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold")
  )


print(p_nothing_hot_24h)
library(dplyr)
library(ggplot2)

# ==============================================================================
# STEP 2: Aggregate All Summer Days (June–August) 24-Hour Profiles in C++
# ==============================================================================
nothing_ids_all <- as.character(nothing_subgroups$CUSTOMER_NUMBER)

nothing_summer_hourly <- ds %>%
  select(all_of(c("CUSTOMER_NUMBER", date_col, "hour_ending", "hourly_kwh"))) %>%
  rename(raw_dt = all_of(date_col)) %>%
  mutate(
    CUSTOMER_NUMBER = cast(CUSTOMER_NUMBER, utf8()),
    dt              = cast(raw_dt, date32()),
    hr              = cast(hour_ending, int32()),
    kwh             = cast(hourly_kwh, float64())
  ) %>%
  filter(
    CUSTOMER_NUMBER %in% nothing_ids_all,
    !is.na(dt),
    month(dt) %in% c(6L, 7L, 8L),
    !is.na(hr), hr >= 1L, hr <= 24L, !is.na(kwh)
  ) %>%
  group_by(CUSTOMER_NUMBER, hr) %>%
  summarize(cust_summer_hr_kwh = mean(kwh, na.rm = TRUE), .groups = "drop") %>%
  collect()

gc()

# ==============================================================================
# STEP 3: Plot Summer 24-Hour Usage Profiles Comparing Both `Nothing` Sub-Cohorts
# ==============================================================================
plot_nothing_summer <- nothing_summer_hourly %>%
  inner_join(
    nothing_subgroups %>%
      mutate(CUSTOMER_NUMBER = as.character(CUSTOMER_NUMBER)) %>%
      select(CUSTOMER_NUMBER, nothing_subcohort),
    by = "CUSTOMER_NUMBER"
  ) %>%
  group_by(nothing_subcohort, hr) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_summer_hr_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_summer_hr_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(
    nothing_subcohort = factor(nothing_subcohort, levels = c(
      "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)",
      "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"
    ))
  )

p_nothing_summer_24h <- ggplot(plot_nothing_summer, aes(x = hr, y = mean_kwh, color = nothing_subcohort, fill = nothing_subcohort)) +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) +
  scale_color_manual(values = c(
    "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)" = "#d95f02",
    "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"            = "#1b9e77"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Slight Hot-Weather Increase (`Nothing` + >= 0.35 kW Lift)" = "#d95f02",
    "Truly Flat (`Nothing` + Loose < 0.35 kW Lift)"            = "#1b9e77"
  ), name = NULL) +
  labs(
    title = "No AC/HP Cluster on Summer Days Flat vs. Some Increase",
    subtitle = "cutoff slope: < 0.35 kW",
    x = "Hour of Day",
    y = "Average Summer Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 10, face = "bold")
  )

print(p_nothing_summer_24h)
ggsave(p_nothing_summer_24h, filename = "plots/nothing_summer_24h.png", width = 8, height = 5, dpi = 300)

if (!exists("temp_bin_summary") || !exists("slopes_3feat")) {
  stop("`temp_bin_summary` or `slopes_3feat` not found—run the temperature binning and 3-feature `hclust` blocks first.")
}

if (!exists("heatpump_customers")) {
  heatpump_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
}
clean_id <- function(x) trimws(as.character(x))
raw_known_hp_ids <- unique(na.omit(clean_id(heatpump_customers$CUSTOMER_NUMBER)))

# Keep only known HP IDs that actually exist in `temp_bin_summary` / `ds`
active_ds_ids <- temp_bin_summary %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(CUSTOMER_NUMBER = clean_id(CUSTOMER_NUMBER)) %>%
  pull(CUSTOMER_NUMBER)

known_hp_ids <- intersect(raw_known_hp_ids, active_ds_ids)

cat(sprintf(
  "CSV Known HPs: %d | Active in ds: %d | Missing from ds: %d\n",
  length(raw_known_hp_ids), length(known_hp_ids),
  length(setdiff(raw_known_hp_ids, active_ds_ids))
))

# Target total Heat Pumps (Active Known HPs + steepest 0–40°F slope candidates)
target_total_hps <- 400L

# Target total Heat Pumps (Known HPs + steepest 0–40°F slope candidates)
target_total_hps <- 400L

# ==============================================================================
# 1. Compute Customer Winter Slope Across 0°F–40°F (`d(kWh)/d(°F)`)
# ==============================================================================
winter_slopes_0_40 <- temp_bin_summary %>%
  mutate(CUSTOMER_NUMBER = clean_id(CUSTOMER_NUMBER)) %>%
  filter(n_obs >= 10L, temp_bin >= 0L, temp_bin <= 40L) %>%
  group_by(CUSTOMER_NUMBER) %>%
  filter(n() >= 4L) %>% # Require >= 4 distinct 2°F bins in 0–40°F for a reliable slope
  summarize(
    n_bins_0_40  = n(),
    slope_0_40_f = cov(temp_bin, cust_mean_kwh) / var(temp_bin),
    .groups      = "drop"
  )

scored_slope_df <- slopes_3feat %>%
  mutate(
    CUSTOMER_NUMBER = clean_id(CUSTOMER_NUMBER),
    is_known_hp     = as.integer(CUSTOMER_NUMBER %in% known_hp_ids)
  ) %>%
  left_join(winter_slopes_0_40, by = "CUSTOMER_NUMBER")

# ==============================================================================
# 2. Select the Most Extreme Negative `0–40°F` Slopes to Reach ~400 Total HPs
# ==============================================================================
n_cand_slots <- max(0L, target_total_hps - length(known_hp_ids))

selected_cand_hps <- scored_slope_df %>%
  filter(
    is_known_hp == 0L,
    !is.na(slope_0_40_f),
    slope_0_40_f < 0
  ) %>%
  arrange(slope_0_40_f) %>% # Most negative (steepest winter heating lift) first
  slice_head(n = n_cand_slots) %>%
  pull(CUSTOMER_NUMBER)

cutoff_slope <- scored_slope_df %>%
  filter(CUSTOMER_NUMBER %in% selected_cand_hps) %>%
  summarize(max_slope = max(slope_0_40_f)) %>%
  pull(max_slope)

cat(sprintf(
  "Likely HPs (Active): %d | Selected Steepest 0–40°F Candidates: %d | Total HPs: %d (Cutoff slope <= %.4f kWh/°F)\n",
  length(known_hp_ids), length(selected_cand_hps),
  length(known_hp_ids) + length(selected_cand_hps), cutoff_slope
))

# ==============================================================================
# 3. Reconcile Classes (`heatpump`, `ac`, `no-electric-temp-control`) & Export CSV
# ==============================================================================
clustered_thermal_df <- scored_slope_df %>%
  transmute(
    CUSTOMER_NUMBER,
    clustered_class = case_when(
      CUSTOMER_NUMBER %in% selected_cand_hps ~ "heatpump",
      hclust_class == "A/C"                  ~ "ac",
      TRUE                                   ~ "no-electric-temp-control"
    ),
    slope_0_40_f  = round(slope_0_40_f, 5),
    slope_10_40_f = round(slope_10_40_f, 5),
    slope_80_90_f = round(slope_80_90_f, 5),
    p95_hot_kw    = round(p95_hot_kw, 3)
  )

thermal_export_df <- clustered_thermal_df %>%
  full_join(
    tibble(CUSTOMER_NUMBER = known_hp_ids, is_known_hp = 1L),
    by = "CUSTOMER_NUMBER"
  ) %>%
  mutate(
    is_known_hp = coalesce(is_known_hp, 0L),
    thermal_class = if_else(is_known_hp == 1L, "heatpump", clustered_class),
    hp_source = case_when(
      is_known_hp == 1L & !is.na(clustered_class) ~ "Likely HP",
      is_known_hp == 1L & is.na(clustered_class)  ~ "Likely HP (Not in Clustered Set)",
      thermal_class == "heatpump"                 ~ "Candidate HP (Top 0–40°F Slope)",
      TRUE                                        ~ NA_character_
    )
  ) %>%
  select(CUSTOMER_NUMBER, thermal_class, is_known_hp, hp_source, slope_0_40_f, slope_10_40_f, slope_80_90_f, p95_hot_kw) %>%
  arrange(CUSTOMER_NUMBER)

cat("\nFinal Territory Thermal Class Counts:\n")
print(count(thermal_export_df, thermal_class, name = "n_customers"))

write.csv(thermal_export_df, "all_thermal_classifications.csv", row.names = FALSE)

# ==============================================================================
# 4. Plot Updated 3-Cluster Thermal Curves (`0–40°F` Steepest Heat Pumps)
# ==============================================================================
curve_final_0_40 <- temp_bin_summary %>%
  mutate(CUSTOMER_NUMBER = clean_id(CUSTOMER_NUMBER)) %>%
  inner_join(
    thermal_export_df %>%
      mutate(
        final_plot_group = factor(
          case_when(
            thermal_class == "heatpump"                 ~ "Heat Pump (Top 0–40°F Slope)",
            thermal_class == "ac"                       ~ "A/C",
            thermal_class == "no-electric-temp-control" ~ "Nothing"
          ),
          levels = c("Heat Pump (Top 0–40°F Slope)", "A/C", "Nothing")
        )
      ) %>%
      select(CUSTOMER_NUMBER, final_plot_group),
    by = "CUSTOMER_NUMBER"
  ) %>%
  filter(n_obs >= 10L) %>%
  group_by(final_plot_group, temp_bin) %>%
  summarize(
    n_cust   = n(),
    mean_kwh = mean(cust_mean_kwh, na.rm = TRUE),
    se_kwh   = sd(cust_mean_kwh, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  filter(n_cust >= 15L)

p_final_0_40 <- ggplot(curve_final_0_40, aes(x = temp_bin, y = mean_kwh, color = final_plot_group, fill = final_plot_group)) +
  annotate("rect", xmin = 0, xmax = 40, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#d95f02") +
  annotate("rect", xmin = 80, xmax = 90, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#7570b3") +
  geom_ribbon(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_color_manual(values = c(
    "Heat Pump (Top 0–40°F Slope)" = "#648FFF",
    "A/C"                          = "#785EF0",
    "Nothing"                      = "#DC267F"
  ), name = NULL) +
  scale_fill_manual(values = c(
    "Heat Pump (Top 0–40°F Slope)" = "#648FFF",
    "A/C"                          = "#785EF0",
    "Nothing"                      = "#DC267F"
  ), name = NULL) +
  labs(
    title = "Load by Temperature: Classified HP/AC/None",
    x = "Outdoor Temperature (°F)",
    y = "Average Usage (kWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 11, face = "bold")
  )

print(p_final_0_40)
ggsave(p_final_0_40, filename = "plots/final_thermal_curves.png", width = 8, height = 5, dpi = 300)

library(dplyr)
library(ggplot2)

set.seed(123)
plot_data <- scored_slope_df %>%
  filter(!is.na(slope_10_40_f), !is.na(slope_80_90_f)) %>%
  mutate(
    plot_group = case_when(
      CUSTOMER_NUMBER %in% selected_cand_hps ~ "Heat Pump (Selected 400)",
      hclust_class == "A/C"                  ~ "A/C",
      hclust_class == "Nothing"              ~ "Nothing",
      TRUE                                   ~ "Unselected Heat Pump Cluster"
    )
  )

p_slope_scatter <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45") +

  # 1. Muted gray layer for the un-selected Heat Pump cluster points
  geom_point(
    data = plot_data %>%
      filter(plot_group == "Unselected Heat Pump Cluster") %>%
      slice_sample(n = min(15000L, nrow(.))),
    aes(x = slope_10_40_f, y = slope_80_90_f),
    color = "gray82",
    alpha = 0.25,
    size = 1.0
  ) +

  # 2. Colored layer for A/C, Nothing, and ONLY the 400 selected Heat Pumps
  geom_point(
    data = plot_data %>%
      filter(plot_group != "Unselected Heat Pump Cluster") %>%
      slice_sample(n = min(15000L, nrow(.))),
    aes(x = slope_10_40_f, y = slope_80_90_f, color = plot_group),
    alpha = 0.45,
    size = 1.3
  ) +

  # 3. Cluster centroids
  geom_point(
    data = centroids,
    aes(x = mean_win_raw, y = mean_sum_raw),
    color = "black", fill = "yellow", shape = 23, size = 4, stroke = 1.2
  ) +

  scale_color_manual(
    values = c(
      "Heat Pump (Selected 400)" = "#d95f02",
      "A/C"                       = "#7570b3",
      "Nothing"                   = "#1b9e77"
    ),
    name = "Thermal Classification"
  ) +
  labs(
    title = "Clustering (k=3), Selecting 400 Steepest Winter Slopes",
    x = "Cold Slope: 10–40°F (kWh / °F)",
    y = "Hot Slope: 80–90°F (kWh / °F)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(p_slope_scatter, filename = "plots/slope_scatter.png", width = 8, height = 5, dpi = 300)

print(p_slope_scatter)

# Full Ward's k = 3 clustering scatter adapted to `scored_slope_df`
set.seed(123)
p_slope_scatter <- ggplot(
  scored_slope_df %>%
    filter(!is.na(slope_10_40_f), !is.na(slope_80_90_f)) %>%
    slice_sample(n = min(15000L, nrow(.))),
  aes(x = slope_10_40_f, y = slope_80_90_f, color = hclust_class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray45") +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_point(
    data = centroids,
    aes(x = mean_win_raw, y = mean_sum_raw),
    color = "black", fill = "yellow", shape = 23, size = 4, stroke = 1.2
  ) +
  scale_color_manual(values = c(
    "Heat Pump" = "#d95f02",
    "A/C"       = "#7570b3",
    "Nothing"   = "#1b9e77"
  ), name = "Hierarchical Cluster") +
  labs(
    title = "Clustering (k = 3): Overselecting Heat Pumps",
    x = "Cold Slope: 10–40°F (kWh / °F)",
    y = "Hot Slope: 80–90°F (kWh / °F)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_slope_scatter)
ggsave(p_slope_scatter, filename = "plots/hclust_slope_scatter.png", width = 8, height = 5, dpi = 300)
