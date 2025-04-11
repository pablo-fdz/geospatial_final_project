# Packages for fixed effects estimation
# install.packages("plm")

library(sf) # simple features' library
library(spData) # library of spatial datasets
library(tidyverse) # dplyr, ggplot, ...
library(plm) # panel data models for fixed effects estimation

########################################

# ======================================
# 1. Importing the data
# ======================================

####################

# 1.1. Catalunya fires and election results

####################

# Read the general election results csv: 1st row as character, 
# the rest with the default type
cat_pol_outcomes <- read.csv("data/1_election_outcomes/catalunya_results/general/cat_general_preprocessed.csv", 
                colClasses = c("character", rep(NA, ncol(read.csv("data/1_election_outcomes/catalunya_results/general/cat_general_preprocessed.csv", nrows = 1)) - 1)))

# Rename column for consistency with Comunitat Valenciana data
cat_pol_outcomes <- cat_pol_outcomes %>% 
  rename(nationalist_index_agg = catalanist_index_agg)

head(cat_pol_outcomes)

# Load the shapefile which combines the fire data with the secciones censales
secciones_cat_fires <- st_read("data/3_fires/Catalunya/cat_fires_percent_dummy_20threshold/") # nolint: line_length_linter.

# Remove the leading "X" from column names
names(secciones_cat_fires) <- sub("^X", "", names(secciones_cat_fires)) # nolint: line_length_linter.

# Replace dots with hyphens in column names
names(secciones_cat_fires) <- gsub("\\.", "-", names(secciones_cat_fires)) # nolint: line_length_linter.

# Append 'dummy' to column names ending with '_'
names(secciones_cat_fires) <- gsub("(_)$", "\\1dummy", names(secciones_cat_fires)) # nolint: line_length_linter.

head(secciones_cat_fires)

# Merge the % of the area affected by fires with the election results

# Identify the columns you want to merge (time period columns)
election_periods <- grep("^\\d{4}-\\d{4}$", names(secciones_cat_fires), value = TRUE)

# Select only the CUSEC column, the time period columns and the geometries
select_secciones_cat <- secciones_cat_fires[, c("CUSEC", election_periods)]

# Merge the data frames, keeping all rows from the original 'cat' data frame
cat_merged <- merge(cat_pol_outcomes, select_secciones_cat, by = "CUSEC", all.x = TRUE)

head(cat_merged)

# Now, in the merged data frame we reduce the fire columns area to a single one,
# since each row in that data frame denotes a CUSEC, an election year and an 
# election number

# Pivot to long format
df_long_cat <- cat_merged %>%
  pivot_longer(
    cols = all_of(election_periods),
    names_to = "period",
    values_to = "fires_value"
  ) %>%
  # Extract start and end year from the period column name
  mutate(
    start_year = as.numeric(sub("-.*", "", period)),
    end_year = as.numeric(sub(".*-", "", period)),
    # Mark the type of period: 2 if single year (start equals end), 1 otherwise
    period_type = if_else(start_year == end_year, 2L, 1L)
  ) %>%
  # Keep only the period whose end year matches year_election and type matches election_number
  filter(end_year == year_election, period_type == election_number)

# Now, join back the selected value. Here we assume one match per row.
cat_final <- cat_merged %>%
  left_join(df_long_cat %>% 
              select(CUSEC, year_election, election_number, fires_affected_area = fires_value),
            by = c("CUSEC", "year_election", "election_number"))

# Verification: Check if the fires_affected_area matches the expected value (
# in "FALSE columns", it means that some operations were done incorrectly)
cat_final <- cat_final %>%
  left_join(df_long_cat %>% 
              select(CUSEC, year_election, election_number, expected_fires_affected_area = fires_value),
            by = c("CUSEC", "year_election", "election_number")) %>%
  mutate(verification = fires_affected_area == expected_fires_affected_area)

# Remove period columns
cat_final <- cat_final %>%
  select(-all_of(election_periods))

# View rows where verification failed
cat_final %>% filter(!verification)

# All of the operations have been done correctly (no row with failed operations),
# so we can remove the verification columns
cat_final <- cat_final %>%
  select(-all_of(c("verification", "expected_fires_affected_area")))

# Remove data frames which are not necessary anymore
rm(cat_merged, cat_pol_outcomes, df_long_cat, secciones_cat_fires, select_secciones_cat) # nolint: line_length_linter.

# We create dummy variablles for different thresholds of fires_affected_area
# (20%, 10% and 5%)
cat_final <- cat_final %>%
  mutate(
    fire_20 = ifelse(fires_affected_area >= 20, 1, 0),
    fire_10 = ifelse(fires_affected_area >= 10, 1, 0),
    fire_5  = ifelse(fires_affected_area >= 5, 1, 0)
  )

# Convert data frame again to simple feature
cat_final <- st_as_sf(cat_final)

head(cat_final)

# Create summary of 1s in fire dummy columns, grouped by election year and number
fire_summary_grouped_cat <- cat_final %>%
  group_by(year_election, election_number) %>%
  summarise(
    fire_20_count = sum(fire_20, na.rm = TRUE),
    fire_10_count = sum(fire_10, na.rm = TRUE),
    fire_5_count = sum(fire_5, na.rm = TRUE),
    total_observations = n(),
    fire_20_percentage = round(sum(fire_20, na.rm = TRUE) / n() * 100, 2),
    fire_10_percentage = round(sum(fire_10, na.rm = TRUE) / n() * 100, 2),
    fire_5_percentage = round(sum(fire_5, na.rm = TRUE) / n() * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(year_election, election_number)

# Print the summary
print(fire_summary_grouped_cat)

# Plot geometries of secciones (simple check)

bbox <- st_bbox(cat_final)

ggplot() +
  geom_sf(
    data = cat_final %>% filter(year_election == 2023), 
    aes(fill = factor(fire_20)),
    color = "gray30",
    size = 0.1
  ) +  # Use backticks for non-standard column name) +
  scale_fill_manual(
    values = c("0" = "white", "1" = "blue"),
    labels = c("No Overlap", "Significant Overlap"),
    name = "Fire Overlap >20%") +
  labs(title = "District Fire Overlap (2019-2023)") +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_sf(
  xlim = c(bbox["xmin"], bbox["xmax"]),
  ylim = c(bbox["ymin"], bbox["ymax"]),
  expand = FALSE)

####################

# 1.2. Comunitat Valenciana fires and election results

####################

# Read the general election results csv: 1st row as character, 
# the rest with the default type
cv_pol_outcomes <- read.csv("data/1_election_outcomes/valencia_reordered.csv", 
                colClasses = c("character", rep(NA, ncol(read.csv("data/1_election_outcomes/valencia_reordered.csv", nrows = 1)) - 1)))

head(cv_pol_outcomes)

# Load the shapefile which combines the fire data with the secciones censales
secciones_cv_fires <- st_read("data/3_fires/Comunitat Valenciana/Valencia fires data/v_fires_percent_dummy.shp") # nolint: line_length_linter.

# Remove the leading "X" from column names
names(secciones_cv_fires) <- sub("^X", "", names(secciones_cv_fires)) # nolint: line_length_linter.

# Replace dots with hyphens in column names
names(secciones_cv_fires) <- gsub("\\.", "-", names(secciones_cv_fires)) # nolint: line_length_linter.

# Append 'dummy' to column names ending with '_'
names(secciones_cv_fires) <- gsub("(_)$", "\\1dummy", names(secciones_cv_fires)) # nolint: line_length_linter.

head(secciones_cv_fires)

# Merge the % of the area affected by fires with the election results

# Identify the columns you want to merge (time period columns)
election_periods <- grep("^\\d{4}-\\d{4}$", names(secciones_cv_fires), value = TRUE)

# Select only the CUSEC column, the time period columns and the geometries
select_secciones_cv <- secciones_cv_fires[, c("CUSEC", election_periods)]

# Merge the data frames, keeping all rows from the original political outcomes 
# data frame
cv_merged <- merge(cv_pol_outcomes, select_secciones_cv, by = "CUSEC", all.x = TRUE)

head(cv_merged)

# Now, in the merged data frame we reduce the fire columns area to a single one,
# since each row in that data frame denotes a CUSEC, an election year and an 
# election number

# Pivot to long format
df_long_cv <- cv_merged %>%
  pivot_longer(
    cols = all_of(election_periods),
    names_to = "period",
    values_to = "fires_value"
  ) %>%
  # Extract start and end year from the period column name
  mutate(
    start_year = as.numeric(sub("-.*", "", period)),
    end_year = as.numeric(sub(".*-", "", period)),
    # Mark the type of period: 2 if single year (start equals end), 1 otherwise
    period_type = if_else(start_year == end_year, 2L, 1L)
  ) %>%
  # Keep only the period whose end year matches year_election and type matches election_number
  filter(end_year == year_election, period_type == election_number)

# Now, join back the selected value. Here we assume one match per row.
cv_final <- cv_merged %>%
  left_join(df_long_cv %>% 
              select(CUSEC, year_election, election_number, fires_affected_area = fires_value),
            by = c("CUSEC", "year_election", "election_number"))

# Verification: Check if the fires_affected_area matches the expected value (
# in "FALSE columns", it means that some operations were done incorrectly)
cv_final <- cv_final %>%
  left_join(df_long_cv %>% 
              select(CUSEC, year_election, election_number, expected_fires_affected_area = fires_value),
            by = c("CUSEC", "year_election", "election_number")) %>%
  mutate(verification = fires_affected_area == expected_fires_affected_area)

# Remove period columns
cv_final <- cv_final %>%
  select(-all_of(election_periods))

# View rows where verification failed
cv_final %>% filter(!verification)

# All of the operations have been done correctly (no row with failed operations),
# so we can remove the verification columns
cv_final <- cv_final %>%
  select(-all_of(c("verification", "expected_fires_affected_area")))

# Remove data frames which are not necessary anymore
rm(cv_merged, cv_pol_outcomes, df_long_cv, secciones_cv_fires, select_secciones_cv) # nolint: line_length_linter.

# We create dummy variablles for different thresholds of fires_affected_area
# (20%, 10% and 5%)
cv_final <- cv_final %>%
  mutate(
    fire_20 = ifelse(fires_affected_area >= 20, 1, 0),
    fire_10 = ifelse(fires_affected_area >= 10, 1, 0),
    fire_5  = ifelse(fires_affected_area >= 5, 1, 0)
  )

# Convert data frame again to simple feature
cv_final <- st_as_sf(cv_final)

head(cv_final)

# Create summary of 1s in fire dummy columns, grouped by election year and number
fire_summary_grouped_cv <- cv_final %>%
  group_by(year_election, election_number) %>%
  summarise(
    fire_20_count = sum(fire_20, na.rm = TRUE),
    fire_10_count = sum(fire_10, na.rm = TRUE),
    fire_5_count = sum(fire_5, na.rm = TRUE),
    total_observations = n(),
    fire_20_percentage = round(sum(fire_20, na.rm = TRUE) / n() * 100, 2),
    fire_10_percentage = round(sum(fire_10, na.rm = TRUE) / n() * 100, 2),
    fire_5_percentage = round(sum(fire_5, na.rm = TRUE) / n() * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(year_election, election_number)

# Print the summary
print(fire_summary_grouped_cv)

# Note that there is no data for the 1st election of 2019, nor for the 2023
# election (as the fires data is limited to 2022)

####################

# 1.3. Concatenating Catalunya's and Comunitat Valenciana's data

####################

# Now, we concatenate the simple features for both regions along the
# rows (note that they both have the same columns)

sf_combined <- bind_rows(cat_final, cv_final)

head(sf_combined)

########################################

# ======================================
# 2. Estimating two-way fixed effects model
# ======================================

####################

# 2.1. For Catalunya

####################

# First, we create a unique identifier for each election: 
# the election year and the election number
cat_final <- cat_final %>%
  mutate(election_id = paste0(year_election, "_", election_number))

# Convert the data frame to a panel data frame
# Specify 'CUSEC' as the individual identifier and 'election_id' as the time identifier
cat_panel <- pdata.frame(cat_final, index = c("CUSEC", "election_id"))

# We save the names of the different political outcomes in a vector
dependent_vars <- c("turnout_rate", "blank_ballot_rate",
  "incumbent_vote_proportion", "political_index_agg",
  "nationalist_index_agg")

# Initialize a list to store the models
model_list_cat <- list()

# Loop over each dependent variable
for (dv in dependent_vars) {
  # Define the formula dynamically
  formula <- as.formula(paste(dv, "~ fires_affected_area"))
  
  # Fit the fixed effects model
  model <- plm(formula, data = cat_panel, model = "within", effect = "twoways")
  
  # Store the model in the list
  model_list_cat[[dv]] <- model
}

# Initialize a list to store the robust summaries
robust_summaries_cat <- list()

for (dv in dependent_vars) {
  model <- model_list_cat[[dv]]
  
  # Obtain the robust summary, with heteroskedasticity-robust and clustered 
  # standard errors at the CUSEC level
  robust_summary <- summary(model, vcov = 
    function(x) vcovHC(x, method = "arellano", type = "HC1", cluster = "group"))
  
  # Store the robust summary in the list
  robust_summaries_cat[[dv]] <- robust_summary
}

# Print a sample robust summary for the first dependent variable
for (dv in dependent_vars[1]) {
  cat("\n### Long Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_cat[[dv]])
}

for (dv in dependent_vars) {
  cat("\n### Short Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_cat[[dv]][["coefficients"]])
}

####################

# 2.2. For Comunitat Valenciana

####################

# First, we create a unique identifier for each election: 
# the election year and the election number
cv_final <- cv_final %>%
  mutate(election_id = paste0(year_election, "_", election_number))

# Convert the data frame to a panel data frame
# Specify 'CUSEC' as the individual identifier and 'election_id' as the time identifier
cv_panel <- pdata.frame(cv_final, index = c("CUSEC", "election_id"))

# We save the names of the different political outcomes in a vector
dependent_vars <- c("turnout_rate", "blank_ballot_rate",
  "incumbent_vote_proportion", "political_index_agg",
  "nationalist_index_agg")

# Initialize a list to store the models
model_list_cv <- list()

# Loop over each dependent variable
for (dv in dependent_vars) {
  # Define the formula dynamically
  formula <- as.formula(paste(dv, "~ fires_affected_area"))
  
  # Fit the fixed effects model
  model <- plm(formula, data = cv_panel, model = "within", effect = "twoways")
  
  # Store the model in the list
  model_list_cv[[dv]] <- model
}

# Initialize a list to store the robust summaries
robust_summaries_cv <- list()

for (dv in dependent_vars) {
  model <- model_list_cv[[dv]]
  
  # Obtain the robust summary, with heteroskedasticity-robust and clustered 
  # standard errors at the CUSEC level
  robust_summary <- summary(model, vcov = 
    function(x) vcovHC(x, method = "arellano", type = "HC1", cluster = "group"))
  
  # Store the robust summary in the list
  robust_summaries_cv[[dv]] <- robust_summary
}

# Print the robust summaries
for (dv in dependent_vars[1]) {
  cat("\n### Long Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_cv[[dv]])
}

for (dv in dependent_vars) {
  cat("\n### Short Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_cv[[dv]][["coefficients"]])
}

####################

# 2.3. Combined

####################

# First, we create a unique identifier for each election: 
# the election year and the election number
sf_combined <- sf_combined %>%
  mutate(election_id = paste0(year_election, "_", election_number))

# Convert the data frame to a panel data frame
# Specify 'CUSEC' as the individual identifier and 'election_id' as the time identifier
combined_panel <- pdata.frame(sf_combined, index = c("CUSEC", "election_id"))

# We save the names of the different political outcomes in a vector
dependent_vars <- c("turnout_rate", "blank_ballot_rate",
  "incumbent_vote_proportion", "political_index_agg",
  "nationalist_index_agg")

# Initialize a list to store the models
model_list_combined <- list()

# Loop over each dependent variable
for (dv in dependent_vars) {
  # Define the formula dynamically
  formula <- as.formula(paste(dv, "~ fires_affected_area"))
  
  # Fit the fixed effects model
  model <- plm(formula, data = combined_panel, model = "within", effect = "twoways")
  
  # Store the model in the list
  model_list_combined[[dv]] <- model
}

# Initialize a list to store the robust summaries
robust_summaries_combined <- list()

for (dv in dependent_vars) {
  model <- model_list_combined[[dv]]
  
  # Obtain the robust summary, with heteroskedasticity-robust and clustered 
  # standard errors at the CUSEC level
  robust_summary <- summary(model, vcov = 
    function(x) vcovHC(x, method = "arellano", type = "HC1", cluster = "group"))
  
  # Store the robust summary in the list
  robust_summaries_combined[[dv]] <- robust_summary
}

# Print an example of a long summary
for (dv in dependent_vars[1]) {
  cat("\n### Long Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_combined[[dv]])
}

for (dv in dependent_vars) {
  cat("\n### Short Summary for Dependent Variable:", dv, "\n\n")
  print(robust_summaries_combined[[dv]][["coefficients"]])
}