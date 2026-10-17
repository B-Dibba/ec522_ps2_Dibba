
#Part 3 – Read In the Data
library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")


unemp_raw = read_rds("data/unemp_raw.rds")

#Part 4 – Wrangle the Data with dplyr

#4.1 Clean up your Data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  )


#4.2 Wrangle your Data
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Step 2: Add month-over-month change column
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change = unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()

# Step 3: Create unemp_recent (January 2015 onward)
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Step 4: Summary stats for full sample
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Step 5: Summary stats for recent sample (2015 onward)
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

#Check
head(unemp)          # should show new unemployment_rate_change column
unemp_summary        # should show 5 rows, one per region
unemp_recent_summary # same structure but based on 2015+ data only


#4.3 Find the Extremes

# Five (plus ties) largest month-over-month INCREASES
unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

# Five (plus ties) largest month-over-month DECREASES
unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)


#4.4 Find the Extremes Excluding 2020

# First create a dataset with 2020 excluded
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

# Five (plus ties) largest INCREASES excluding 2020
unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

# Five (plus ties) largest DECREASES excluding 2020
unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)


#Check
unemp_top_increases       # should be dominated by 2020 COVID spike
unemp_top_decreases       # should be dominated by 2020 COVID recovery
unemp_top_increases_no_covid   # now shows pre-COVID extremes
unemp_top_decreases_no_covid   # same


#Part 5 – Visualize the Data with ggplot2

#5.1 Produce a Line Plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rates by Region (1976–Present)",
    x     = "Date",
    y     = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

#5.2 Produce Individual Line Plots using facet_wrap()
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rates by Region (1976–Present)",
    x     = "Date",
    y     = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

#5.3 Produce a Bar Chart
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region (Full Sample)",
    x     = "Region",
    y     = "Mean Unemployment Rate (%)",
    fill  = "Region"
  ) +
  theme_minimal()

#5.4 Your Choice 
# Month-over-Month Change in Unemployment Rate
ggplot(unemp, aes(x = date, y = unemployment_rate_change, color = region)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ region, scales = "free_x") +  
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Month-over-Month Change in Unemployment Rate by Region (1976–Present)",
    x     = "Date",
    y     = "Change in Unemployment Rate (percentage points)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


