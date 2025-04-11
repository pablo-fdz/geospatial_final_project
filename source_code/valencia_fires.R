if (!require("nngeo")) remotes::install_github("michaeldorman/nngeo")
if (!require("lwgeom")) install.packages("lwgeom")
install.packages("units", dependencies = TRUE)
install.packages("patchwork")
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


# Load the data
secciones <- st_read("SECC_CE_20230101.shp") 
fires <- st_read("ince_incendios0.shp")

# Rename relevant columns
fires_val <- fires %>%
  rename(
    municipality = nom_mun,  # Municipality name
    fire_date   = f_detec,      # Fire detection date
    year = anyo, 
    cause = g_caus_txt
  )%>%
  select(numparte, year, municipality, Shape_Area, Shape_Leng, fire_date, cause)

fires_val$fire_date <- as.Date(fires_val$fire_date, format = "%d/%m/%Y")
fires_val$month <- month(fires_val$fire_date)

# ======== PROCESS secciones_v DATA ============ #

secciones_v <- secciones %>% filter(NCA == "Comunitat Valenciana") # Keep Comunitat Valenciana only
rm(secciones)
secciones_v <- subset(secciones_v, select = c(CUSEC, NPRO, NCA, NMUN, geometry)) # Drop unnecessary columns

# ======== PROCESS FIRES DATA ============ #

# Dropping NAs
fires_val<- fires_val %>% filter(!is.na(numparte))
  
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

# Check date format of datasets for merging: 
str(fires_val)
str(periods)
fires_val$fire_date <- as.Date(fires_val$fire_date, format = "%d/%m/%Y")

# 3. Assign election periods using fuzzy join
fires_processed <- fires_val %>%
  fuzzyjoin::fuzzy_left_join(
    periods,
    by = c("fire_date" = "start_date", "fire_date" = "end_date"),
    match_fun = list(`>=`, `<=`)
  )

# step 4: 
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
# drop NA's for 1993-2000
fires_repaired <- fires_repaired %>%
  filter(!is.na(election_period))

fires_repaired <- fires_repaired %>%
  select(-municipality, -start_date, -end_date)

fires_grouped <- fires_repaired %>%
  group_by(election_period) %>%
  summarise(
    numparte = list(unique(numparte)),  # Preserve original codes
    month = list(unique(month)),            # Preserve months
    year = list(unique(year)),              # Preserve years
    geometry = st_union(geometry)           # Combine geometries
  ) %>%
  filter(!is.na(election_period)) %>%       # Remove unclassified fires
  st_as_sf()                                # Ensure sf object

fires_grouped <- fires_grouped %>%
  select(-month, -year, -numparte)

fires_grouped <- st_cast(fires_grouped, "MULTIPOLYGON")

# multipolyons:
unique(st_geometry_type(fires_grouped))


# Convert fires_grouped to CRS 4326
fires_grouped <- st_transform(fires_grouped, 4326)
secciones_v <- st_transform(secciones_v, 4326)

###### The final object is "fires_grouped" with 9 obs #######

# ================ PLOT ================== #
bbox <- st_bbox(secciones_v)
bbox_polygon <- st_as_sfc(bbox)


# Plot fires and secciones_v
ggplot() +
  geom_sf(data = secciones_v) +
  geom_sf(data = fires_grouped, fill = "red") +
  coord_sf(
  xlim = c(bbox["xmin"], bbox["xmax"]),
  ylim = c(bbox["ymin"], bbox["ymax"]),
  expand = FALSE) +
  theme_minimal()


# ============= Calculate the affected area, create dummies ============= #

# Ensure both sf objects use the same CRS
if (st_crs(secciones_v) != st_crs(fires_grouped)) {
  fires_grouped <- st_transform(fires_grouped, st_crs(secciones_v))
}

# Transform to a projected CRS (replace with an appropriate local projection)
projected_crs <- 3857  # Web Mercator
secciones_v <- st_transform(secciones_v, projected_crs)
fires_grouped <- st_transform(fires_grouped, projected_crs)

# Extract election periods as character strings
election_periods <- as.character(fires_grouped$election_period)

# Add new columns initialized with NA
secciones_v <- secciones_v %>%
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

# Iterate through each row in secciones_v_v
for (i in seq_len(nrow(secciones_v))) {
  section_geom <- secciones_v$geometry[i]
  
  # Calculate percentage for each election period
  for (j in seq_len(nrow(fires_grouped))) {
    period <- as.character(fires_grouped$election_period[j])
    fire_geom <- fires_grouped$geometry[j]
    
    secciones_v[[period]][i] <- calculate_overlap(section_geom, fire_geom)
  }
}

# Create dummy variables (1 if overlap >= 20%, else 0)
secciones_v <- secciones_v %>%
  mutate(across(all_of(election_periods), 
                ~ if_else(. >= 20, 1, 0), 
                .names = "{.col}_dummy"))

# Export the new dataset with percentages and dummies into a shapefile
st_write(secciones_v, "v_fires_percent_dummy.shp", driver = "ESRI Shapefile")


data <- st_read("v_fires_percent_dummy.shp")

table(data$X2000.2004_)


# ============ Get number of affected districs ============ #

result <- secciones_v %>%
  st_drop_geometry() %>%          # Remove spatial data for faster computation
  select(ends_with("_dummy")) %>% # Select dummy columns
  summarise(across(everything(), sum)) %>%  # Sum all dummy columns
  pivot_longer(
    cols = everything(),
    names_to = "election_period",
    values_to = "districts_with_fire",
    names_transform = ~ gsub("_dummy", "", .x)
  )

result

# ========= Plot colored districts and fires =========== #

# 1. Plot secciones_v_v with dummy-based coloring
dummy_plot <- ggplot() +
  geom_sf(
    data = secciones_v_v,
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

# Combine plots using patchwork (install if needed)

combined_plot <- dummy_plot + fires_plot + 
  plot_layout(ncol = 2) +
  plot_annotation(title = "Spatial Distribution of Fire Overlap and Incidents")

# Display the combined visualization
print(combined_plot)
