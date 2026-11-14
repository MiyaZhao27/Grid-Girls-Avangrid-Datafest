# part 2 answers 

# high average overall (could be bigger houses)
# Peak demand contributors: high during peak times, proportion during peak times compared to baseline is large
# seasonal customers: heat pumps and AC
# flexible load customers: 
# customers likely to adopt new technologies: heat pumps and solar customers
# emerging electrification candidates

library(arrow)
library(dplyr)
library(tibble)
library(kableExtra)

# 1. Read saved classification CSVs, known customer lists, and top-customer lists
evs                <- read.csv("data/all_lvl1_lvl2_charger_ids.csv")
thermal            <- read.csv("data/all_thermal_classifications.csv")
heatpump_customers <- read.csv("data/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
solar_customers    <- read.csv("data/EV_Heat Pump_Solar PV Customers(PV Customers).csv")
top_overall        <- read.csv("data/top_overall_customers.csv")
top_peak           <- read.csv("data/top_customers.csv")
high_peak_ratio    <- read.csv("data/cust_high_prop_peak.csv")

# Helper to clean IDs and safely extract the ID column regardless of exact header name
clean_id <- function(x) trimws(as.character(x))

extract_ids <- function(df) {
  id_col <- intersect(c("CUSTOMER_NUMBER", "Customer.Number", "customer_number", "id", "ID"), names(df))[1]
  if (is.na(id_col)) id_col <- names(df)[1]
  unique(na.omit(clean_id(df[[id_col]])))
}
pq <- list.files("cache_clean", pattern = "\\.parquet$", full.names = TRUE)
length(pq)  
ds <- open_dataset(pq, format = "parquet")
# 2. Pull the master universe of all unique customer IDs from `ds`
if (exists("ds")) {
  all_cust_ids <- ds %>%
    distinct(CUSTOMER_NUMBER) %>%
    collect() %>%
    pull(CUSTOMER_NUMBER) %>%
    clean_id() %>%
    unique()
} else {
  all_cust_ids <- unique(c(
    extract_ids(evs),
    extract_ids(thermal),
    extract_ids(heatpump_customers),
    extract_ids(solar_customers),
    extract_ids(top_overall),
    extract_ids(top_peak)
  ))
}

# 3. Extract clean ID lookup sets from each source
solar_ids       <- extract_ids(solar_customers)
known_hp        <- extract_ids(heatpump_customers)
top_overall_ids <- extract_ids(top_overall)
top_peak_ids    <- extract_ids(top_peak)
high_peak_ids   <- extract_ids(high_peak_ratio)

lvl1_ids <- evs %>% filter(lvl1 == 1) %>% extract_ids()
lvl2_ids <- evs %>% filter(lvl2 == 1) %>% extract_ids()

hp_clust_ids <- thermal %>% filter(thermal_class == "heatpump") %>% extract_ids()
ac_clust_ids <- thermal %>% filter(thermal_class == "ac") %>% extract_ids()
no_temp_ids  <- thermal %>% filter(thermal_class == "no-electric-temp-control") %>% extract_ids()

# 4. Assemble `customer_characteristics` (1 row per customer, binary 0/1 flags)
customer_characteristics <- tibble(CUSTOMER_NUMBER = all_cust_ids) %>%
  mutate(
    is_solar = as.integer(CUSTOMER_NUMBER %in% solar_ids),
    lvl1EV   = as.integer(CUSTOMER_NUMBER %in% lvl1_ids),
    lvl2EV   = as.integer(CUSTOMER_NUMBER %in% lvl2_ids),
    
    # Reconciled Heat Pump flag (allows Solar + Known HP accounts to keep both flags)
    heatpump = as.integer(
      CUSTOMER_NUMBER %in% known_hp |
      (CUSTOMER_NUMBER %in% hp_clust_ids & is_solar == 0L)
    ),
    AC = as.integer(CUSTOMER_NUMBER %in% ac_clust_ids & heatpump == 0L & is_solar == 0L),
    no_electric_temp_control = as.integer(
      CUSTOMER_NUMBER %in% no_temp_ids & heatpump == 0L & AC == 0L & is_solar == 0L
    ),
    
    # Mapped directly from your top-customer CSVs
    high_overall_use    = as.integer(CUSTOMER_NUMBER %in% top_overall_ids),
    high_peak_ratio         = as.integer(CUSTOMER_NUMBER %in% high_peak_ids),
    peak_demand_contributor = as.integer(CUSTOMER_NUMBER %in% top_peak_ids)
  ) %>%
  as.data.frame()

# 5. Inspect summary counts and export master table
print(head(customer_characteristics))

colSums(customer_characteristics[, -1]) %>%
  enframe(name = "characteristic", value = "n_customers") %>%
  mutate(pct_of_territory = round(100 * n_customers / nrow(customer_characteristics), 2)) %>%
  print()

write.csv(customer_characteristics, "customer_characteristics.csv", row.names = FALSE)



# 1. Count n and % for every unique combination of 1/0 characteristic columns
combo_counts <- customer_characteristics %>%
  # Exclude ID column(s) so you only group by the 1/0 characteristic flags
  count(across(-any_of(c("CUSTOMER_NUMBER", "customer_id", "id"))), name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  arrange(desc(n))

print(combo_counts)

kable(head(combo_counts))
library(dplyr)
library(knitr)
library(kableExtra)

# 1. Count n and % for every unique combination of 1/0 characteristic columns
combo_counts <- customer_characteristics %>%
  count(across(-any_of(c("CUSTOMER_NUMBER", "customer_id", "id"))), name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 2)
  ) %>%
  arrange(desc(n))

# 2. Format counts with commas and percentages with % sign for display
flag_cols <- setdiff(names(combo_counts), c("n", "pct"))

combo_display <- combo_counts %>%
  mutate(
    n   = scales::comma(n),
    pct = sprintf("%.2f%%", pct)
  ) |> head(9)

# 3. Render in RStudio Viewer pane for screenshotting
combo_kable <- combo_display %>%
  kbl(
    format    = "html",
    col.names = c(flag_cols, "Customers (n)", "Share (%)"),
    align     = c(rep("c", length(flag_cols)), "r", "r"),
    caption   = "Customer Counts by Technology Characteristic Combination"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(
    font_size         = 13,
    bootstrap_options = c("condensed")
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general       = sprintf("1 = Present, 0 = Absent. Total customers N = %s.", scales::comma(sum(combo_counts$n))),
    general_title = "Notes: ",
    footnote_as_chunk = TRUE
  )

print(combo_kable)

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# 1. Sum the number of 1s in each characteristic column independently (no interactions)
total_n <- nrow(customer_characteristics)

category_totals <- customer_characteristics %>%
  select(-any_of(c("CUSTOMER_NUMBER", "customer_id", "id"))) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Category",
    values_to = "flag"
  ) %>%
  group_by(Category) %>%
  summarize(
    n   = sum(flag == 1, na.rm = TRUE),
    pct = round(100 * n / total_n, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# 2. Format numbers and render in RStudio Viewer pane for screenshotting
category_display <- category_totals %>%
  mutate(
    n   = scales::comma(n),
    pct = sprintf("%.2f%%", pct)
  )

category_kable <- category_display %>%
  kbl(
    format    = "html",
    col.names = c("Technology / Characteristic", "Customers (n)", "Share of Territory (%)"),
    align     = c("l", "r", "r"),
    caption   = "Appendix Table: Total Customer Counts by Technology Category"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(
    font_size         = 13,
    bootstrap_options = c("condensed")
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general       = sprintf("Categories are non-mutually exclusive; customers with multiple technologies are counted in each applicable row. Total customers N = %s.", scales::comma(total_n)),
    general_title = "Notes: ",
    footnote_as_chunk = TRUE
  )

print(category_kable)


