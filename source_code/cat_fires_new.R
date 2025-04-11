# if (!require("nngeo")) remotes::install_github("michaeldorman/nngeo")
# if (!require("lwgeom")) install.packages("lwgeom")
install.packages("units", dependencies = TRUE)
install.packages("patchwork")
install.packages("fuzzyjoin")
library(sf)
library(tidyverse)
library(spData)
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lwgeom)
library(units)
library(patchwork)
library(fuzzyjoin)

setwd("/mnt/0E403953403942AF/Drive pablo.fdzalb/BSE - DSDM/2-Geospatial_DS_&_Economic_Spatial_Models/geospatial_final_project/")

# Load the data
secciones <- st_read("data/2_secciones_censales/España_Seccionado2023_ETRS89H30/SECC_CE_20230101.shp")
fires_combined_clean <- st_read("data/3_fires/Catalunya/fires_combined_clean.shp")

head(secciones)

head(fires_combined_clean)

# ======== PROCESS SECCIONES DATA ============ #

secciones_cat <- secciones %>% filter(NCA == "Cataluña") # Keep Catalunya only
rm(secciones)
secciones_cat <- subset(secciones_cat, select = c(CUSEC, NPRO, NCA, NMUN, geometry)) # Drop unnecessary columns

# ======== PROCESS FIRES DATA ============ #

summary(fires_combined_clean)

# Dropping NAs
fires_combined_clean <- fires_combined_clean %>% filter(!is.na(CODI_FINAL))

# 1. Convert 2-digit years to 4-digit (e.g., "01" → 2001)
fires_processed <- fires_combined_clean %>%
  mutate(
    year = case_when(
      nchar(year) == 2 ~ paste0("20", year),
      TRUE ~ year
    ),
    # Create proper Date column using day/month/year
    fire_date = ymd(paste(year, month, day, sep = "-"))
  )

# 2. Define election periods and their date ranges
periods <- tribble(
  ~election_period,       ~start_date,     ~end_date,
  "2000-2004",   "2000-04-01",    "2004-03-31",
  "2004-2008",   "2004-04-01",    "2008-03-31",
  "2008-2011",   "2008-04-01",    "2011-11-30",
  "2011-2015",   "2011-12-01",    "2015-12-31",
  "2015-2016",   "2015-01-01",    "2016-06-30",
  "2016-2019",   "2016-07-01",    "2019-04-30",
  "2019-2019",   "2019-05-01",    "2019-11-30",
  "2019-2023",   "2019-12-01",    "2023-07-31",
  "2023-2023",   "2023-08-01",    "2023-12-31"
) %>% mutate(
  start_date = ymd(start_date),
  end_date = ymd(end_date)
)

# 3. Assign election periods using fuzzy join
fires_processed <- fires_processed %>%
  fuzzyjoin::fuzzy_left_join(
    periods,
    by = c("fire_date" = "start_date", "fire_date" = "end_date"),
    match_fun = list(`>=`, `<=`)
  )

head(fires_processed)
summary(fires_processed)

# 4. Handle invalid data errors:

# Disable S2 geometry processing
sf_use_s2(FALSE)

# Convert to sf object first
fires_repaired <- fires_processed %>%
  st_as_sf() %>%                # Convert to sf object explicitly
  st_transform(25831) %>%       # Catalunya's UTM 31N (EPSG:25831)
  st_make_valid() %>%           # Geometry repair
  st_buffer(dist = 0.5) %>%     # Clean gaps (0.5 meters)
  st_simplify(dTolerance = 1)   # Simplify with 1m tolerance

# 5. Group and combine geometries by election period
fires_grouped <- fires_repaired %>%
  group_by(election_period) %>%
  summarise(
    CODI_FINAL = list(unique(CODI_FINAL)),  # Preserve original codes
    month = list(unique(month)),            # Preserve months
    year = list(unique(year)),              # Preserve years
    geometry = st_union(geometry)           # Combine geometries
  ) %>%
  filter(!is.na(election_period)) %>%       # Remove unclassified fires
  st_as_sf()                                # Ensure sf object

# Convert fires_grouped to CRS 4326
fires_grouped <- st_transform(fires_grouped, 4326)
secciones <- st_transform(secciones_cat, 4326)

print(fires_grouped)

###### The final object is "fires_grouped" with 9 obs #######

# ================ PLOT ================== #
bbox <- st_bbox(secciones_cat)
bbox_polygon <- st_as_sfc(bbox)


# Plot fires and secciones
ggplot() +
  geom_sf(data = secciones_cat) +
  geom_sf(data = fires_grouped, fill = "red") +
  coord_sf(
  xlim = c(bbox["xmin"], bbox["xmax"]),
  ylim = c(bbox["ymin"], bbox["ymax"]),
  expand = FALSE) +
  theme_minimal()


# ============= Calculate the affected area, create dummies ============= #

# Ensure both sf objects use the same CRS
if (st_crs(secciones_cat) != st_crs(fires_grouped)) {
  fires_grouped <- st_transform(fires_grouped, st_crs(secciones_cat))
}

# Transform to a projected CRS (replace with an appropriate local projection)
projected_crs <- 3857  # Web Mercator
secciones_cat <- st_transform(secciones_cat, projected_crs)
fires_grouped <- st_transform(fires_grouped, projected_crs)

# Extract election periods as character strings
election_periods <- as.character(fires_grouped$election_period)

# Add new columns initialized with NA
secciones_cat <- secciones_cat %>%
  mutate(!!!setNames(rep(list(NA_real_), length(election_periods)), election_periods))

# Function to calculate overlap percentage
calculate_overlap <- function(section_geom, fire_geom) {
  intersection <- tryCatch(st_intersection(section_geom, fire_geom), error = function(e) NULL)
  
  if (is.null(intersection) || any(is.na(intersection))) {
    return(0)  # No overlap or invalid geometry
  } else {
    intersection_area <- sum(st_area(intersection), na.rm = TRUE)
    section_area <- st_area(section_geom)
    
    # Convert units to numeric values before division
    intersection_area <- as.numeric(intersection_area)
    section_area <- as.numeric(section_area)
    
    if (section_area == 0) return(0)  # Avoid division by zero
    
    return((intersection_area / section_area) * 100)  # Percentage
  }
}

# Iterate through each row in secciones_cat
for (i in seq_len(nrow(secciones_cat))) {
  section_geom <- secciones_cat$geometry[i]
  
  # Calculate percentage for each election period
  for (j in seq_len(nrow(fires_grouped))) {
    period <- as.character(fires_grouped$election_period[j])
    fire_geom <- fires_grouped$geometry[j]
    
    secciones_cat[[period]][i] <- calculate_overlap(section_geom, fire_geom)
  }
}

# Create dummy variables (1 if overlap >= 20%, else 0)
secciones_cat <- secciones_cat %>%
  mutate(across(all_of(election_periods), 
                ~ if_else(. >= 20, 1, 0), 
                .names = "{.col}_dummy"))

head(secciones_cat)

# Export the new dataset with percentages and dummies into a shapefile
st_write(secciones_cat, "cat_fires_percent_dummy.shp", driver = "ESRI Shapefile")

# ============ Get number of affected districs ============ #

result <- secciones_cat %>%
  st_drop_geometry() %>%          # Remove spatial data for faster computation
  select(ends_with("_dummy")) %>% # Select dummy columns
  summarise(across(everything(), sum)) %>%  # Sum all dummy columns
  pivot_longer(
    cols = everything(),
    names_to = "election_period",
    values_to = "districts_with_fire",
    names_transform = ~ gsub("_dummy", "", .x)
  )

head(result)

# In the election period 2019-2023, there were 5 secciones censales with significant fire overlap. # nolint: line_length_linter.

# ========= Plot colored districts and fires =========== #

# 1. Plot secciones_cat with dummy-based coloring
dummy_plot <- ggplot() +
  geom_sf(
    data = secciones_cat,
    aes(fill = factor(`2011-2015_dummy`)),  # Use backticks for non-standard column name
    color = "gray30",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c("0" = "white", "1" = "skyblue"),
    labels = c("No Overlap", "Significant Overlap"),
    name = "Fire Overlap >20%"
  ) +
  labs(title = "District Fire Overlap (2011-2015)") +
  theme_void() +
  theme(legend.position = "bottom")

dummy_plot

# 2. Plot fires_grouped separately
fires_plot <- ggplot() +
  geom_sf(
    data = fires_grouped,
    fill = "red",
    alpha = 0.7,
    size = 0.3
  ) +
  labs(title = "Fire Incident Areas") +
  theme_void()

fires_plot

# Combine plots using patchwork (install if needed)

combined_plot <- dummy_plot + fires_plot + 
  plot_layout(ncol = 2) +
  plot_annotation(title = "Spatial Distribution of Fire Overlap and Incidents")

# Display the combined visualization
print(combined_plot)