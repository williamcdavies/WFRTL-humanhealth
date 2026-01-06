# ---- Packages ----
# Install any missing packages once:
# install.packages(c("sf","dplyr","readr","ggplot2","rnaturalearth","rnaturalearthdata","stringi"))

# ---- Packages ----
# install.packages(c("sf","dplyr","readr","ggplot2","rnaturalearth","rnaturalearthdata","ggspatial"))
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# ---- User inputs ----
csv_path <- "C:/Users/FacundoScordo/Box/WildfireWilliam/Results/UNR WF POPULATED_PLACES_POINTS_COMPOSITE_DATA/wfrtl_composite2024.csv"

out_png  <- "Cities_Smoke_Categories_2024.png"
# ---- CRS and extent (LAEA "round hemisphere") ----
crs_laea <- "+proj=laea +lat_0=40 +lon_0=-96 +datum=WGS84 +units=m +no_defs"

lon_min <- -135; lon_max <- -60
lat_min <-   5;  lat_max <-  80

# ---- Read data and compute summed smoke variable ----
dat <- read_csv(
  csv_path,
  show_col_types = FALSE,
  col_select = c(LONGITUDE, LATITUDE, Smoke_days_medium_point, Smoke_days_heavy_point, ADM0NAME)
) %>%
  filter(
    ADM0NAME %in% c("United States of America", "Canada", "Mexico"),
    !is.na(LONGITUDE), !is.na(LATITUDE)
  ) %>%
  mutate(
    Smoke_days_sum = Smoke_days_medium_point + Smoke_days_heavy_point
  )

# ---- Convert to sf points ----
pts_wgs84 <- st_as_sf(dat, coords = c("LONGITUDE","LATITUDE"), crs = 4326, remove = FALSE)
pts_proj  <- st_transform(pts_wgs84, crs_laea)

# ---- Country polygons ----
na_countries <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf") |>
  st_transform(crs_laea)

# ---- Graticules every 10° ----
grat_laea <- st_graticule(
  lon = seq(lon_min, lon_max, by = 10),
  lat = seq(lat_min, lat_max, by = 10)
) |> st_transform(crs_laea)

# ---- Nice pastel yellow ??? orange ??? red palette ----
smoke_pal <- scale_color_gradientn(
  colours = c("#FFF7BC", "#FEC44F", "#FD8D3C", "#E31A1C"),
  name = "Smoke days",
  limits = c(0, 50),
  oob = scales::squish
)

# ---- Build and plot map ----

bb <- st_bbox(na_countries)
xlim_vals <- c(bb["xmin"], bb["xmax"])
ylim_vals <- c(bb["ymin"], bb["ymax"])

p <- ggplot() +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "#D6ECFF", color = NA),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_text(color = "black", size = 7)
  ) +
  geom_sf(data = grat_laea, color = "white", linewidth = 0.28) +
  geom_sf(data = na_countries, fill = "white", color = "grey65", linewidth = 0.25) +
  geom_sf(data = pts_proj, aes(color = Smoke_days_sum), size = 0.7, alpha = 1, show.legend = TRUE) +
  smoke_pal +
  coord_sf(crs = crs_laea, default_crs = NULL,
           xlim = xlim_vals, ylim = ylim_vals, expand = FALSE) +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    line_width = 0.6,
    text_cex = 1.2,
    unit_category = "metric",
    bar_cols = c("white", "grey30"),
    pad_x = unit(0.8, "cm"),
    pad_y = unit(0.8, "cm")
  ) +
  labs(title = "Weighted mean number of smoke days (Medium + Heavy)") +
  theme(
    legend.position = c(0.15, 0.32),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.background = element_rect(fill = scales::alpha("white", 1), color = NA),
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(0,0,8,0))
  )

p

# ---- Save high-res PNG ----
ggsave(out_png, p, width = 8, height = 10, dpi = 300, bg = "white")
message("Saved: ", normalizePath(out_png))

