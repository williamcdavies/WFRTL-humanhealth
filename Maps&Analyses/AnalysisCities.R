# ---- Extra packages you may need ----
# install.packages(c("stringr","purrr","tidyr","forcats"))
library(stringr)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)

# =========================
# ANALYSIS: City counts by Smoke_days_sum category (2010-2024)
# =========================

# ---- User inputs ----
input_dir <- "C:/Users/FacundoScordo/Box/WildfireWilliam/Results/UNR WF POPULATED_PLACES_POINTS_COMPOSITE_DATA"
countries_keep <- c("United States of America", "Canada", "Mexico")

# ---- Helper: categorization function ----
# Bins chosen to align with your map limits (0-50) and typical reporting
smoke_breaks  <- c(0, 1, 11, 21, 31, 41, 51, Inf)  # left-closed, right-open bins
smoke_labels  <- c("0", "1-10", "11-20", "21-30", "31-40", "41-50", "???51")

bin_smoke <- function(x) {
  cut(x,
      breaks = smoke_breaks,
      labels = smoke_labels,
      right = FALSE, include.lowest = TRUE)
}

# ---- Discrete palette matching the map gradient ----
grad_cols <- c("#FFF7BC", "#FEC44F", "#FD8D3C", "#E31A1C")
pal_discrete <- colorRampPalette(grad_cols)(length(smoke_labels))
names(pal_discrete) <- smoke_labels
pal_discrete[["0"]] <- "#FFFFE5"   # optional: make "0" very pale

# ---- File list 2010-2024 ----
files <- list.files(
  input_dir,
  pattern = "^wfrtl_composite(201[0-9]|202[0-4])\\.csv$",
  full.names = TRUE
)
if (length(files) == 0) stop("No composite files found for 2010-2024 in: ", input_dir)
message("Found ", length(files), " files")

# ---- Reader for one file ----
read_one <- function(path) {
  yr <- as.integer(stringr::str_extract(basename(path), "\\d{4}"))
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_select = c(LONGITUDE, LATITUDE,
                   Smoke_days_medium_point, Smoke_days_heavy_point, ADM0NAME)
  ) |>
    dplyr::filter(
      ADM0NAME %in% countries_keep,
      !is.na(LONGITUDE), !is.na(LATITUDE)
    ) |>
    dplyr::mutate(
      year = yr,
      Smoke_days_sum = Smoke_days_medium_point + Smoke_days_heavy_point,
      smoke_cat = bin_smoke(Smoke_days_sum)
    )
}

# ---- Build the full table ----
all_years <- purrr::map_dfr(files, read_one)

# ---- Restrict analysis to 2010-2024 ----
all_years <- all_years |> 
  dplyr::filter(year >= 2010, year <= 2024)

# ---- Sanity check ----
stopifnot(all(!is.na(all_years$smoke_cat)))
message("Rows: ", nrow(all_years), " | Years: ", paste(range(all_years$year), collapse = "-"))

# ---- Summaries ----
counts_year_cat <- all_years |>
  dplyr::count(year, smoke_cat, name = "n_cities") |>
  dplyr::arrange(year, smoke_cat)

counts_year_country_cat <- all_years |>
  dplyr::count(year, ADM0NAME, smoke_cat, name = "n_cities") |>
  dplyr::arrange(year, ADM0NAME, smoke_cat)

# ---- Save ----
out_csv_overall <- file.path(input_dir, "city_counts_by_year_smokecat_2010_2024.csv")
out_csv_by_ctry <- file.path(input_dir, "city_counts_by_year_country_smokecat_2010_2024.csv")
readr::write_csv(counts_year_cat, out_csv_overall)
readr::write_csv(counts_year_country_cat, out_csv_by_ctry)
message("Saved: ", normalizePath(out_csv_overall))
message("Saved: ", normalizePath(out_csv_by_ctry))

# ---- Stacked bar (overall) ----
counts_year_cat$smoke_cat <- forcats::fct_relevel(counts_year_cat$smoke_cat, smoke_labels)

p_counts <- ggplot(counts_year_cat,
                   aes(x = factor(year), y = n_cities, fill = smoke_cat)) +
  geom_col() +
  scale_fill_manual(values = pal_discrete, drop = FALSE,
                    guide = guide_legend(title = "Smoke days (sum of Medium + Heavy)")) +
  labs(
    x = "Year",
    y = "Number of cities",
    title = "Cities by Smoke_days_sum category (2010-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

out_png_counts <- file.path(input_dir, "city_counts_by_year_smokecat_2010_2024.png")
ggsave(out_png_counts, p_counts, width = 10, height = 6, dpi = 300, bg = "white")
message("Wrote: ", normalizePath(out_png_counts))

# ============================================================
# SUMMARY: Mean, Max, and Mean Daily Exposure per Year
# ============================================================

# Helper to get days in each year
days_in_year <- function(y) ifelse((y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0), 366, 365)

smoke_summary_year <- all_years |>
  group_by(year) |>
  summarise(
    mean_smoke_days = mean(Smoke_days_sum, na.rm = TRUE),  # average smoke days per city
    max_smoke_days  = max(Smoke_days_sum, na.rm = TRUE),   # maximum smoke days experienced by any city
    n_cities        = n(),
    total_smoke_days_all_cities = sum(Smoke_days_sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mean_cities_exposed_per_day = total_smoke_days_all_cities / days_in_year(year)
  )

# ---- Save to CSV ----
out_csv_summary <- file.path(input_dir, "mean_max_cities_exposed_per_year_2010_2024.csv")
readr::write_csv(smoke_summary_year, out_csv_summary)
message("Saved: ", normalizePath(out_csv_summary))


# ============================================================
# TREND ANALYSIS: Mean & Max Smoke Days Over Time
# ============================================================

library(trend)    # for Mann-Kendall test
library(broom)    # for tidy regression output

# ---- Linear regression (parametric trend) ----
lm_mean <- lm(mean_smoke_days ~ year, data = smoke_summary_year)
lm_max  <- lm(max_smoke_days  ~ year, data = smoke_summary_year)
lm_mean_cities_exposed_per_day  <- lm(mean_cities_exposed_per_day ~ year, data = smoke_summary_year)

summary(lm_mean)
summary(lm_max)
summary(lm_mean_cities_exposed_per_day)

# Tidy summary tables
trend_lm_summary <- bind_rows(
  broom::tidy(lm_mean) |> mutate(metric = "Mean smoke days"),
  broom::tidy(lm_max)  |> mutate(metric = "Max smoke days")
) |>
  dplyr::select(metric, term, estimate, std.error, statistic, p.value)

out_csv_lm <- file.path(input_dir, "trend_linear_regression_mean_max_smokedays.csv")
readr::write_csv(trend_lm_summary, out_csv_lm)
message("Saved linear trend results: ", normalizePath(out_csv_lm))

# ---- Mann-Kendall (non-parametric monotonic trend test) ----
mk_mean <- trend::mk.test(smoke_summary_year$mean_smoke_days)
mk_max  <- trend::mk.test(smoke_summary_year$max_smoke_days)

# Collect MK results into a table
trend_mk_summary <- tibble::tibble(
  metric = c("Mean smoke days", "Max smoke days"),
  tau = c(mk_mean$estimates[["tau"]], mk_max$estimates[["tau"]]),
  p_value = c(mk_mean$p.value, mk_max$p.value),
  trend_direction = ifelse(c(mk_mean$estimates[["tau"]], mk_max$estimates[["tau"]]) > 0, "Increasing", "Decreasing")
)

out_csv_mk <- file.path(input_dir, "trend_mannkendall_mean_max_smokedays.csv")
readr::write_csv(trend_mk_summary, out_csv_mk)
message("Saved Mann-Kendall trend results: ", normalizePath(out_csv_mk))

# ---- Print results nicely ----
cat("\n========== TREND TESTS ==========\n")
cat("\nLinear model (mean smoke days):\n")
print(summary(lm_mean))
cat("\nLinear model (max smoke days):\n")
print(summary(lm_max))
cat("\nMann-Kendall tests:\n")
print(trend_mk_summary)


# ============================================================
# PLOT: Mean, Max, and Daily Exposure
# ============================================================

library(scales)

library(ggplot2)

# Define scaling factor for secondary axis
# This scales the bars to roughly match the smoke-day values
scale_factor <- max(smoke_summary_year$mean_smoke_days, smoke_summary_year$max_smoke_days) /
  max(smoke_summary_year$mean_cities_exposed_per_day)

p_smoke_trend <- ggplot(smoke_summary_year, aes(x = factor(year))) +
  
  # Bars: mean cities exposed per day (scaled for secondary axis)
  geom_col(aes(y = mean_cities_exposed_per_day * scale_factor, fill = "Mean cities exposed per day"), width = 0.6) +
  
  # Max smoke days as dots + line
  geom_line(aes(y = max_smoke_days, group = 1, color = "Max smoke days"), size = 1.2) +
  geom_point(aes(y = max_smoke_days, color = "Max smoke days"), size = 3) +
  
  # Mean smoke days as dots + line
  geom_line(aes(y = mean_smoke_days, group = 1, color = "Mean smoke days"), size = 1.2) +
  geom_point(aes(y = mean_smoke_days, color = "Mean smoke days"), size = 3) +
  
  # Primary and secondary axes
  scale_y_continuous(
    name = "Smoke days per city",
    sec.axis = sec_axis(~./scale_factor, name = "Mean cities exposed per day")  # reverse scaling
  ) +
  
  # Colors and fill
  scale_color_manual(
    name = "",
    values = c("Mean smoke days" = "#FD8D3C",  # warm orange
               "Max smoke days" = "#E31A1C")   # red
  ) +
  scale_fill_manual(
    name = "",
    values = c("Mean cities exposed per day" = "grey70")
  ) +
  
  # Labels and theme
  labs(
    x = "Year",
    title = "Trends in Smoke Days and Cities Exposed per Day (2010-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold", color = "black")
  ) +
  scale_x_discrete(breaks = smoke_summary_year$year)  # show all years

# Save the plot
out_png_trend <- file.path(input_dir, "smoke_days_mean_max_cities_trend.png")
ggsave(out_png_trend, p_smoke_trend, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved plot: ", normalizePath(out_png_trend))

out_svg_trend <- file.path(input_dir, "smoke_days_mean_max_cities_trend.svg")
ggsave(out_svg_trend, p_smoke_trend, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved SVG for editing: ", normalizePath(out_svg_trend))

# ============================================
# ANALYSIS: Mean Smoke_days_sum by LATITUDE and LONGITUDE (2010-2024)
# ============================================

library(dplyr)
library(ggplot2)

# ---- Latitude & Longitude bins (0.5° increments) ----
lat_bin_size <- 1
lon_bin_size <- 1

all_years <- all_years |>
  mutate(
    lat_bin = floor(LATITUDE  / lat_bin_size) * lat_bin_size,
    lon_bin = floor(LONGITUDE / lon_bin_size) * lon_bin_size
  )

# ---- Mean by LATITUDE (0.5°) ----
mean_lat <- all_years |>
  group_by(year, lat_bin) |>
  summarise(
    mean_smoke_days = mean(Smoke_days_sum, na.rm = TRUE),
    .groups = "drop"
  )

p_lat <- ggplot(mean_lat, aes(x = lat_bin, y = mean_smoke_days, color = factor(year))) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Year") +
  labs(
    x = "Latitude (°)",
    y = "Mean smoke days (Medium + Heavy)",
    title = "Mean Smoke Days by Latitude (0.5° bins, 2010-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

out_png_lat <- file.path(input_dir, "mean_smoke_days_by_latitude_0.5deg_2010_2024.png")
ggsave(out_png_lat, p_lat, width = 10, height = 9, dpi = 300, bg = "white")
message("Wrote: ", normalizePath(out_png_lat))

out_svg_lat <- file.path(input_dir, "mean_smoke_days_by_latitude_0.5deg_2010_2024.svg")
ggsave(out_svg_lat, p_lat, width = 10, height = 9, dpi = 300, bg = "white")
message("Saved SVG for editing: ", normalizePath(out_svg_lat))

# ---- Mean by LONGITUDE (0.5°, constrained to -175 to -50) ----
mean_lon <- all_years |>
  group_by(year, lon_bin) |>
  summarise(
    mean_smoke_days = mean(Smoke_days_sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(lon_bin >= -175, lon_bin <= -50)

p_lon <- ggplot(mean_lon, aes(x = lon_bin, y = mean_smoke_days, color = factor(year))) +
  geom_line(size = 1) +
  coord_cartesian(xlim = c(-175, -50)) +
  scale_color_viridis_d(name = "Year") +
  labs(
    x = "Longitude (°)",
    y = "Mean smoke days (Medium + Heavy)",
    title = "Mean Smoke Days by Longitude (0.5° bins, 2010-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

out_png_lon <- file.path(input_dir, "mean_smoke_days_by_longitude_0.5deg_2010_2024.png")
ggsave(out_png_lon, p_lon, width = 10, height = 9, dpi = 300, bg = "white")
message("Wrote: ", normalizePath(out_png_lon))

out_svg_lon <- file.path(input_dir, "mean_smoke_days_by_longitude_0.5deg_2010_2024.svg")
ggsave(out_svg_lon, p_lon, width = 10, height = 9, dpi = 300, bg = "white")
message("Saved SVG for editing: ", normalizePath(out_svg_lon))


# ============================================================
# Cities affected by smoke per day
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)

# ---- Set working directory ----
setwd("C:/Users/FacundoScordo/Box/WildfireWilliam/Results/UNR WF POPULATED_PLACES_POINTS_COMPOSITE_DATA")

# ---- Load data ----
df <- read.csv("smokes_places_composite.csv", stringsAsFactors = FALSE)

# If R already reads dates as yyyy-mm-dd, convert explicitly:
df <- df %>%
  mutate(
    date = as.Date(date)   # ensures correct format
  ) %>%
  arrange(date)

# ---- Extract year and fill missing dates ----
df_complete <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%
  mutate(
    sum = replace_na(sum, 0),
    yday = yday(date)
  ) %>%
  ungroup()

# ============================================================
# Cities affected by smoke per day IN PERIODS 2010-2017 AND 2018-2024
# ============================================================

# ---- Assign time period labels ----
df_periods <- df_complete %>%
  mutate(
    period = case_when(
      year >= 2010 & year <= 2017 ~ "2010-2017",
      year >= 2018 & year <= 2024 ~ "2018-2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

# ---- Compute mean and 95% CI by day-of-year for each period ----
df_stats <- df_periods %>%
  group_by(period, yday) %>%
  summarise(
    mean_sum = mean(sum, na.rm = TRUE),
    sd_sum   = sd(sum, na.rm = TRUE),
    n        = n(),
    se       = sd_sum / sqrt(n),
    ci_lower = mean_sum - 1.96 * se,
    ci_upper = mean_sum + 1.96 * se,
    .groups = "drop"
  )


# ---- Plot with custom line and ribbon colors ----
p <- ggplot() +
  # 95% CI ribbons colored by period
  geom_ribbon(data = df_stats,
              aes(x = yday, ymin = ci_lower, ymax = ci_upper, fill = period),
              alpha = 0.25) +
  # Mean lines
  geom_line(data = df_stats,
            aes(x = yday, y = mean_sum, color = period),
            size = 1.2) +
  # Custom colors
  scale_color_manual(
    name = "",
    values = c("2010-2017" = "#FEC44F",  # warm orange
               "2018-2024" = "#E31A1C")  # red
  ) +
  scale_fill_manual(
    name = "",
    values = c("2010-2017" = "#FEC44F",  # match line grad_cols <- c("#FFF7BC", "#FEC44F", "#FD8D3C", "#E31A1C")
               "2018-2024" = "#E31A1C")  # match line
  ) +
  labs(
    x = "Day of Year (1-365)",
    y = "Average cities affected by smoke",
    title = "Mean Daily Smoke Exposure with 95% Confidence Interval",
    subtitle = "Comparing periods 2010-2017 vs 2018-2024"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Save output
ggsave("smoke_mean_CI_by_period.png", p, width = 10, height = 7, dpi = 300, bg = "white")
ggsave("smoke_mean_CI_by_period.svg", p, width = 10, height = 7, dpi = 300, bg = "white")

p


# ---- Compute start and end of smoke season per year ----
yearly_season <- df_periods %>%
  group_by(year) %>%
  filter(sum > 20) %>%
  summarise(
    start = min(date),
    end   = max(date),
    mean_cities = mean(sum, na.rm = TRUE),
    max_cities  = max(sum, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Compute mean start/end day-of-year per period ----
season_summary <- yearly_season %>%
  mutate(period = case_when(
    year >= 2010 & year <= 2017 ~ "2010-2017",
    year >= 2018 & year <= 2024 ~ "2018-2024"
  )) %>%
  group_by(period) %>%
  summarise(
    mean_start_doy = round(mean(yday(start))),
    mean_end_doy   = round(mean(yday(end))),
    mean_cities    = mean(mean_cities),
    max_cities     = max(max_cities),
    .groups = "drop"
  )

# ---- Optional: Convert to approximate month/day (ignoring year) ----
season_summary <- season_summary %>%
  mutate(
    season_start_md = format(as.Date(mean_start_doy, origin = "2000-01-01"), "%b-%d"),
    season_end_md   = format(as.Date(mean_end_doy, origin = "2000-01-01"), "%b-%d")
  )

print(season_summary)


