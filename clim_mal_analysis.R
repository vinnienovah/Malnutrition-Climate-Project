

# Loading the necessary packages ------------------------------------------

library(pacman)
p_load(readr,
       tidyr,
       tidyverse,
       dplyr,
       ggplot2, 
       zoo,
       lubridate,
       kableExtra)

# Reading in the dataset --------------------------------------------------
# malnutrition dataset
mal_df <- read_csv("moh_mal.csv")

#climate one data
clim1_df <- read_csv("climate_data.csv")

# climate two data
clim2_df <- read_csv("temp_precip.csv")


# Create full grid of all possible county-year-month combinations
full_grid <- expand_grid(
  county = unique(clim2_df$county),
  year = 1950:2024,
  month = 1:12
)

# Join your data onto the full grid (this creates NA rows for missing months)
clim2_df <- full_grid %>%
  left_join(clim2_df, by = c("county", "year", "month")) %>%
  arrange(county, year, month)

# lag
clim2_df <- clim2_df %>%
  arrange(county, year, month) %>%
  group_by(county) %>%
  mutate(
    mean_temp = lag(mean_temp),
    total_precip = lag(total_precip)
  ) %>%
  ungroup()

# write.csv(clim2_df, "temp_precip2.csv", row.names = FALSE)

clim2_df <- clim2_df %>%
  filter(year >= 2000 & year <= 2024)

# let's create a function to clean the county names
clean_values <- function(x) {
  x |>
    stringr::str_replace_all("[-/_]", " ") |>  # Remove apostrophes, slashes, and replace hyphens
    stringr::str_replace_all("'", "") |>    # Remove apostrophes
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_to_title()
}



# apply to climate_df and mal if necessary
clim1_df <-clim1_df |>
  mutate(county = clean_values(county),
         month_name = month.abb[month]   # Add month names here
         ) |>
  select(c("county", "year", "month", "month_name", "mean_temp_celsius", "mean_precip", 
           "mean_spei01", "mean_spei02", "mean_spei03", "mean_spei06", 
           "ndvi", "vci"))|>
  filter(year >= 2015 & year <= 2024)
  

clim2_df <- clim2_df |>
  mutate(
    county = clean_values(county),
    month_name = month.abb[month]
  ) |>
  filter(year >= 2015 & year <= 2024)


mal_df <-mal_df |>
  mutate(county = clean_values(county))

# join the two clim datasets
climate_df <- clim1_df |>
  left_join(clim2_df, by = c("year", "month", "month_name", "county"))



# arrange the variables
climate_df <- climate_df |>
  select(c("county", "year", "month", "month_name", "mean_temp", "mean_precip", "total_precip", 
           "ndvi", "vci", "mean_spei01", "mean_spei02", "mean_spei03", "mean_spei06"
  )) |>
  rename("temp" = "mean_temp")


# let's have the data from 2016-2024, to coincide with the mal data
# we define the counties
arid_asal <- c("Mandera", "Wajir", "Isiolo", "Tana River", "Garissa",
               "Turkana", "Marsabit", "Samburu", "Baringo")

semi_arid_asal <- c("Tharaka Nithi", "Embu", "Kajiado", "Kilifi", "Kitui", "Kwale",
                    "Laikipia", "Lamu", "Makueni", "Meru", "Narok", "Nyeri",
                    "Taita Taveta", "West Pokot", "Machakos")

urban <- c("Nairobi", "Mombasa", "Kisumu")

# Combine all selected counties
selected_counties <- c(arid_asal, semi_arid_asal, urban)

# Filter the data frame for those counties and years
climate_16_24_df <- climate_df %>%
  filter(year >= 2015 & year <= 2024,
         county %in% selected_counties) 

mal_df <- mal_df |>
  filter(!county %in% c("Grand Total", "Asal", "Urban", "Non Asal"))|>
  filter(year >= 2016 & year <= 2025)
  
################################################
# Count unique county-year-month combinations  #
#                                              #
nrow(distinct(clim1_df, county, year, month))  #
nrow(distinct(clim2_df, county, year, month))  # 
#                                              #
missing_from_clim2 <- anti_join(               #
  distinct(clim1_df, county, year, month),     #
  distinct(clim2_df, county, year, month),     #
  by = c("county", "year", "month")            #
)                                              #
#                                              #
missing_from_clim2                             #
################################################



# ============================================================================
# CLIMATE-MALNUTRITION INTEGRATION - 
# ============================================================================
# ============================================================================
# STEP 1: VERIFY AND EXPLORE DATA
# ============================================================================

cat("\n=== DATA OVERVIEW ===\n")

# Climate
cat("Climate Data (2015-2024):\n")
cat("  Rows:", nrow(climate_16_24_df), "\n")
cat("  Counties:", n_distinct(climate_16_24_df$county), "\n")
cat("  Counties:", paste(sort(unique(climate_16_24_df$county)), collapse = ", "), "\n")

# Malnutrition
cat("\nMalnutrition Data:\n")
cat("  Rows:", nrow(mal_df), "\n")
cat("  Counties:", n_distinct(mal_df$county), "\n")

# Assessment distribution
cat("\nAssessment Months Distribution:\n")
mal_summary <- mal_df %>%
  count(year, month) %>%
  arrange(year, month)
print(mal_summary)

# ============================================================================
# STEP 2: PREPARE MALNUTRITION DATA
# ============================================================================

cat("\n=== PREPARING MALNUTRITION DATA ===\n")

mal_clean <- mal_df %>%
  mutate(
    # Convert month names to numbers
    month_num = case_when(
      month == "Jan" | month == "January" ~ 1,
      month == "Feb" | month == "February" ~ 2,
      month == "Mar" | month == "March" ~ 3,
      month == "Apr" | month == "April" ~ 4,
      month == "May" ~ 5,
      month == "Jun" | month == "June" ~ 6,
      month == "Jul" | month == "July" ~ 7,
      month == "Aug" | month == "August" ~ 8,
      month == "Sep" | month == "September" ~ 9,
      month == "Oct" | month == "October" ~ 10,
      month == "Nov" | month == "November" ~ 11,
      month == "Dec" | month == "December" ~ 12,
      TRUE ~ month_num  # Use existing month_num if already numeric
    ),
    
    # Classify assessment period
    # Feb/Aug measure OND (previous year's short rains)
    # Jul measures MAM (current year's long rains)
    assessment_period = case_when(
      month_num %in% c(2, 3) ~ "Post-Short-Rains",      # Feb/Mar
      month_num %in% c(7) ~ "Post-Long-Rains",          # July
      month_num %in% c(8) ~ "Post-Long-Rains-Late",     # August (also MAM impact)
      TRUE ~ "Other"
    ),
    
    # Which rainy season does this measure?
    rainy_season_measured = case_when(
      month_num %in% c(2, 3) ~ "OND",           # Feb/Mar measures OND
      month_num %in% c(7, 8) ~ "MAM"           # Jul/Aug measures MAM
    ),
    
    # Standardize assessment month for merging
    assessment_month_std = case_when(
      month_num %in% c(2, 3) ~ 2,        # Standardize to Feb
      month_num %in% c(7, 8) ~ 7        # Standardize to Jul
    ),
    
    # Add county type
    county_type = case_when(
      county %in% arid_asal ~ "Arid ASAL",
      county %in% semi_arid_asal ~ "Semi-Arid ASAL",
      county %in% urban ~ "Urban"
    ),
    
    # Create assessment date
    assessment_date = as.Date(paste(year, month_num, "15", sep = "-"))
  ) 
# %>% filter(assessment_period != "Other")  # Keep only valid assessments

cat("Malnutrition data prepared:\n")
cat("  Total valid assessments:", nrow(mal_clean), "\n")
cat("  Post-Short-Rains (Feb/Mar):", sum(mal_clean$assessment_period == "Post-Short-Rains"), "\n")
cat("  Post-Long-Rains (Jul):", sum(mal_clean$assessment_period == "Post-Long-Rains"), "\n")
cat("  Post-Long-Rains-Late (Aug):", sum(mal_clean$assessment_period == "Post-Long-Rains-Late"), "\n")

# ============================================================================
# STEP 3: CREATE SEASONAL CLIMATE AGGREGATES
# ============================================================================
cat("\n=== CREATING SEASONAL CLIMATE AGGREGATES ===\n")

# Classify months into rainy seasons
climate_with_seasons <- climate_16_24_df %>%
  mutate(
    rainy_season = case_when(
      month %in% c(3, 4, 5) ~ "MAM",        # Long Rains (Mar-Apr-May)
      month %in% c(10, 11, 12) ~ "OND",     # Short Rains (Oct-Nov-Dec)
      month %in% c(6, 7, 8, 9) ~ "JJAS",    # Long Dry (Jun-Jul-Aug-Sep)
      month %in% c(1, 2) ~ "JF"             # Short Dry (Jan-Feb)
    ),
    
    county_type = case_when(
      county %in% arid_asal ~ "Arid ASAL",
      county %in% semi_arid_asal ~ "Semi-Arid ASAL",
      county %in% urban ~ "Urban")
  )

# Aggregate by rainy season (MAM and OND only)
seasonal_climate <- climate_with_seasons %>%
  filter(rainy_season %in% c("MAM", "OND")) %>%
  group_by(county, county_type, year, rainy_season) %>%
  summarise(
    # Rainfall
    total_rainfall = sum(total_precip, na.rm = TRUE),
    mean_rainfall = mean(total_precip, na.rm = TRUE),
    max_rainfall = max(total_precip, na.rm = TRUE),
    min_rainfall = min(total_precip, na.rm = TRUE),
    rainfall_cv = sd(total_precip, na.rm = TRUE) / mean(total_precip, na.rm = TRUE),
    
    # Temperature
    mean_temp = mean(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    temp_range = max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE),
    
    # Vegetation
    mean_ndvi = mean(ndvi, na.rm = TRUE),
    max_ndvi = max(ndvi, na.rm = TRUE),
    min_ndvi = min(ndvi, na.rm = TRUE),
    mean_vci = mean(vci, na.rm = TRUE),
    min_vci = min(vci, na.rm = TRUE),
    
    # Drought indices (SPEI)
    mean_spei01 = mean(mean_spei01, na.rm = TRUE),
    mean_spei02 = mean(mean_spei02, na.rm = TRUE),
    mean_spei03 = mean(mean_spei03, na.rm = TRUE),
    mean_spei06 = mean(mean_spei06, na.rm = TRUE),
    min_spei03 = min(mean_spei03, na.rm = TRUE),
    
    # Count months
    n_months = n(),
    
    .groups = "drop"
  )

cat("Seasonal aggregates created:\n")
cat("  Total seasonal records:", nrow(seasonal_climate), "\n")
cat("  MAM seasons:", sum(seasonal_climate$rainy_season == "MAM"), "\n")
cat("  OND seasons:", sum(seasonal_climate$rainy_season == "OND"), "\n")

# ============================================================================
# STEP 4: CALCULATE CLIMATE ANOMALIES
# ============================================================================

cat("\n=== CALCULATING CLIMATE ANOMALIES ===\n")

seasonal_climate <- seasonal_climate %>%
  group_by(county, rainy_season) %>%
  mutate(
    # Long-term means (LTM)
    rainfall_ltm = mean(total_rainfall, na.rm = TRUE),
    rainfall_sd = sd(total_rainfall, na.rm = TRUE),
    temp_ltm = mean(mean_temp, na.rm = TRUE),
    temp_sd = sd(mean_temp, na.rm = TRUE),
    ndvi_ltm = mean(mean_ndvi, na.rm = TRUE),
    ndvi_sd = sd(mean_ndvi, na.rm = TRUE),
    vci_ltm = mean(mean_vci, na.rm = TRUE),
    
    # Standardized anomalies
    rainfall_anomaly = (total_rainfall - rainfall_ltm) / rainfall_sd,
    temp_anomaly = (mean_temp - temp_ltm) / temp_sd,
    ndvi_anomaly = (mean_ndvi - ndvi_ltm) / ndvi_sd,
    vci_anomaly = mean_vci - vci_ltm,
    
    # Percent of normal
    rainfall_pct_normal = (total_rainfall / rainfall_ltm) * 100,
    ndvi_pct_normal = (mean_ndvi / ndvi_ltm) * 100,
    
    # Drought categories based on rainfall anomaly
    drought_category = case_when(
      rainfall_anomaly < -2.0 ~ "Extreme Drought",
      rainfall_anomaly < -1.5 ~ "Severe Drought",
      rainfall_anomaly < -1.0 ~ "Moderate Drought",
      rainfall_anomaly < -0.5 ~ "Mild Drought",
      rainfall_anomaly > 1.5 ~ "Extreme Wet",
      rainfall_anomaly > 1.0 ~ "Very Wet",
      rainfall_anomaly > 0.5 ~ "Wet",
      TRUE ~ "Normal"
    ),
    
    # Also classify by SPEI
    spei_category = case_when(
      mean_spei03 < -2.0 ~ "Extreme Drought",
      mean_spei03 < -1.5 ~ "Severe Drought",
      mean_spei03 < -1.0 ~ "Moderate Drought",
      mean_spei03 < -0.5 ~ "Mild Drought",
      mean_spei03 > 1.5 ~ "Extreme Wet",
      mean_spei03 > 1.0 ~ "Very Wet",
      mean_spei03 > 0.5 ~ "Wet",
      TRUE ~ "Normal"
    ),
    
    # Binary drought indicators
    drought_occurred = ifelse(rainfall_anomaly < -1.0, 1, 0),
    severe_drought = ifelse(rainfall_anomaly < -1.5, 1, 0),
    extreme_drought = ifelse(rainfall_anomaly < -2.0, 1, 0),
    
    # Vegetation stress
    veg_stress = ifelse(mean_vci < 35, 1, 0)  # VCI < 35 indicates stress
  ) %>%
  ungroup()

cat("Climate anomalies calculated\n")

# Drought frequency summary
drought_summary <- seasonal_climate %>%
  group_by(rainy_season, county_type) %>%
  summarise(
    n = n(),
    n_drought = sum(drought_occurred, na.rm = TRUE),
    n_severe = sum(severe_drought, na.rm = TRUE),
    pct_drought = round(n_drought/n*100, 1),
    .groups = "drop"
  )

cat("\nDrought frequency by season and county type:\n")
print(drought_summary)

# ============================================================================
# STEP 5: CREATE CUMULATIVE CLIMATE INDICATORS
# ============================================================================

cat("\n=== CREATING CUMULATIVE CLIMATE INDICATORS ===\n")

climate_cumulative <- climate_16_24_df %>%
  arrange(county, year, month) %>%
  group_by(county) %>%
  mutate(
    # Rolling rainfall
    rainfall_3m = rollsum(total_precip, k = 3, fill = NA, align = "right"),
    rainfall_6m = rollsum(total_precip, k = 6, fill = NA, align = "right"),
    rainfall_9m = rollsum(total_precip, k = 9, fill = NA, align = "right"),
    rainfall_12m = rollsum(total_precip, k = 12, fill = NA, align = "right"),
    
    # Rolling mean temperature
    temp_3m = rollmean(temp, k = 3, fill = NA, align = "right"),
    temp_6m = rollmean(temp, k = 6, fill = NA, align = "right"),
    
    # Rolling vegetation
    ndvi_3m = rollmean(ndvi, k = 3, fill = NA, align = "right"),
    ndvi_6m = rollmean(ndvi, k = 6, fill = NA, align = "right"),
    vci_3m = rollmean(vci, k = 3, fill = NA, align = "right"),
    vci_6m = rollmean(vci, k = 6, fill = NA, align = "right"),
    
    # Rolling SPEI
    spei03_3m = rollmean(mean_spei03, k = 3, fill = NA, align = "right"),
    spei06_3m = rollmean(mean_spei06, k = 3, fill = NA, align = "right"),
    
    # Count low rainfall months in past 6 months
    rainfall_mean = mean(total_precip, na.rm = TRUE),
    low_rain_month = ifelse(total_precip < 0.25 * rainfall_mean, 1, 0),
    low_rain_months_6m = rollsum(low_rain_month, k = 6, fill = NA, align = "right")
  ) %>%
  ungroup()

# Extract cumulative indicators just before assessments
# For Feb assessments: use January data
climate_at_feb <- climate_cumulative %>%
  filter(month == 1) %>%
  mutate(
    assessment_month = 2,
    assessment_year = year
  ) %>%
  select(county, assessment_year, assessment_month,
         current_rainfall = total_precip, current_temp = temp,
         current_ndvi = ndvi, current_vci = vci,
         rainfall_3m, rainfall_6m, rainfall_9m, rainfall_12m,
         temp_3m, temp_6m,
         ndvi_3m, ndvi_6m, vci_3m, vci_6m,
         spei03_3m, spei06_3m, low_rain_months_6m)

# For Jul/Aug assessments: use June data
climate_at_jul <- climate_cumulative %>%
  filter(month == 6) %>%
  mutate(
    assessment_month = 7,
    assessment_year = year
  ) %>%
  select(county, assessment_year, assessment_month,
         current_rainfall = total_precip, current_temp = temp,
         current_ndvi = ndvi, current_vci = vci,
         rainfall_3m, rainfall_6m, rainfall_9m, rainfall_12m,
         temp_3m, temp_6m,
         ndvi_3m, ndvi_6m, vci_3m, vci_6m,
         spei03_3m, spei06_3m, low_rain_months_6m)

# Combine
climate_cumulative_for_merge <- bind_rows(climate_at_feb, climate_at_jul)

cat("Cumulative indicators created\n")

# ============================================================================
# STEP 6: PREPARE SEASONAL DATA FOR MERGING
# ============================================================================

cat("\n=== PREPARING SEASONAL DATA FOR MERGING ===\n")

seasonal_for_merge <- seasonal_climate %>%
  mutate(
    # Link rainy season to assessment month
    assessment_month = ifelse(rainy_season == "MAM", 7, 2),
    
    # For OND: assessment is in Feb of NEXT year
    # For MAM: assessment is in Jul of SAME year
    assessment_year = ifelse(rainy_season == "OND", year + 1, year)
  ) %>%
  select(
    county, county_type,
    climate_year = year,
    assessment_year,
    assessment_month,
    rainy_season,
    # Rainfall
    total_rainfall, mean_rainfall, rainfall_cv,
    max_rainfall, min_rainfall,
    # Temperature
    mean_temp, max_temp, min_temp, temp_range,
    # Vegetation
    mean_ndvi, max_ndvi, min_ndvi, mean_vci, min_vci,
    # SPEI
    mean_spei01, mean_spei02, mean_spei03, mean_spei06, min_spei03,
    # Anomalies
    rainfall_anomaly, temp_anomaly, ndvi_anomaly, vci_anomaly,
    rainfall_pct_normal, ndvi_pct_normal,
    # Reference values
    rainfall_ltm, rainfall_sd, temp_ltm, ndvi_ltm, vci_ltm,
    # Drought indicators
    drought_category, spei_category,
    drought_occurred, severe_drought, extreme_drought, veg_stress
  )

cat("Seasonal data prepared for merging:\n")
cat("  Records:", nrow(seasonal_for_merge), "\n")
cat("  Assessment years:", paste(range(seasonal_for_merge$assessment_year), collapse = " to "), "\n")

# ============================================================================
# STEP 7: MERGE CLIMATE AND MALNUTRITION DATA
# ============================================================================

cat("\n=== MERGING DATASETS ===\n")

# Step 1: Merge seasonal climate
integrated_step1 <- mal_clean %>%
  left_join(
    seasonal_for_merge,
    by = c("county" = "county",
           "year" = "assessment_year",
           "assessment_month_std" = "assessment_month",
           "county_type" = "county_type")
  )


# Step 2: Add cumulative indicators
integrated_data <- integrated_step1 %>%
  left_join(
    climate_cumulative_for_merge,
    by = c("county" = "county",
           "year" = "assessment_year",
           "assessment_month_std" = "assessment_month")
  )

# Check merge quality
cat("\n=== MERGE QUALITY CHECK ===\n")
cat("Malnutrition assessments:", nrow(mal_clean), "\n")
cat("After merge:", nrow(integrated_data), "\n")
cat("With seasonal climate:", sum(!is.na(integrated_data$total_rainfall)), "\n")
cat("With cumulative data:", sum(!is.na(integrated_data$rainfall_6m)), "\n")
cat("Seasonal match rate:", round(sum(!is.na(integrated_data$total_rainfall))/nrow(mal_clean)*100, 1), "%\n")
cat("Cumulative match rate:", round(sum(!is.na(integrated_data$rainfall_6m))/nrow(mal_clean)*100, 1), "%\n")

# Identify unmatched records
unmatched <- integrated_data %>%
  filter(is.na(total_rainfall)) %>%
  select(county, year, month, assessment_period, rainy_season_measured)

if(nrow(unmatched) > 0) {
  cat("\nUnmatched records:\n")
  print(unmatched %>% count(year, month, rainy_season_measured))
  cat("\nNote: 2025 Feb assessment measures OND 2024, but climate data ends at 2024\n")
}

# ============================================================================
# STEP 8: CREATE FINAL INTEGRATED DATASET
# ============================================================================

cat("\n=== CREATING FINAL INTEGRATED DATASET ===\n")

integrated_final <- integrated_data %>%
  mutate(
    # Unique ID
    record_id = paste(county, year, month, sep = "_"),
    
    # Malnutrition rates
    sam_rate = sam_caseload_6_59 / gam_caseload_6_59,
    mam_rate = mam_caseload_6_59 / gam_caseload_6_59,
    sam_prop = sam_caseload_6_59 / (sam_caseload_6_59 + mam_caseload_6_59),
    
    # Per capita (if needed - would need population data)
    # For now, use raw caseload
    
    # Time variables
    year_season = paste(year, rainy_season, sep = "_"),
    
    # Lag between rainy season and assessment
    months_since_season_end = case_when(
      rainy_season == "MAM" & assessment_month_std == 7 ~ 2,  # May end to July
      rainy_season == "OND" & assessment_month_std == 2 ~ 2,  # Dec end to Feb
      TRUE ~ NA_real_
    ),
    
    # Combined drought indicator (rainfall + SPEI + VCI)
    multi_indicator_drought = case_when(
      drought_occurred == 1 & mean_spei03 < -1 & veg_stress == 1 ~ "Severe Multi-Indicator",
      drought_occurred == 1 & (mean_spei03 < -1 | veg_stress == 1) ~ "Moderate Multi-Indicator",
      drought_occurred == 1 | mean_spei03 < -1 | veg_stress == 1 ~ "Single Indicator",
      TRUE ~ "No Drought"
    )
  ) %>%
  # Reorder columns logically
  select(
    # Identifiers
    record_id, county, county_type, year, month, month_num,
    assessment_month_std, assessment_period, assessment_date,
    
    # Malnutrition
    gam_caseload_6_59, sam_caseload_6_59, mam_caseload_6_59,
    pregnant_breastfeeding_women_caseload,
    sam_rate, mam_rate, sam_prop,
    
    # Season info
    rainy_season_measured, rainy_season, climate_year,
    months_since_season_end, year_season,
    
    # Seasonal climate - Rainfall
    total_rainfall, mean_rainfall, rainfall_cv,
    max_rainfall, min_rainfall,
    rainfall_anomaly, rainfall_pct_normal, rainfall_ltm, rainfall_sd,
    
    # Seasonal climate - Temperature
    mean_temp, max_temp, min_temp, temp_range,
    temp_anomaly, temp_ltm,
    
    # Seasonal climate - Vegetation
    mean_ndvi, max_ndvi, min_ndvi, ndvi_anomaly, ndvi_pct_normal, ndvi_ltm,
    mean_vci, min_vci, vci_anomaly, vci_ltm,
    
    # Seasonal climate - SPEI
    mean_spei01, mean_spei02, mean_spei03, mean_spei06, min_spei03,
    
    # Drought classification
    drought_category, spei_category, multi_indicator_drought,
    drought_occurred, severe_drought, extreme_drought, veg_stress,
    
    # Cumulative indicators
    current_rainfall, current_temp, current_ndvi, current_vci,
    rainfall_3m, rainfall_6m, rainfall_9m, rainfall_12m,
    temp_3m, temp_6m,
    ndvi_3m, ndvi_6m, vci_3m, vci_6m,
    spei03_3m, spei06_3m, low_rain_months_6m
  ) 

# %>% rename(county_type = county_type.x)  # Clean up duplicate column

cat("Final dataset created:\n")
cat("  Total records:", nrow(integrated_final), "\n")
cat("  Complete cases:", sum(complete.cases(integrated_final %>% 
                                              select(gam_caseload_6_59, total_rainfall, rainfall_anomaly))), "\n")

# ============================================================================
# STEP 9: SUMMARY STATISTICS
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

# Filter to complete cases for analysis
integrated_complete <- integrated_final %>%
  filter(!is.na(rainfall_anomaly))

# Overall
summary_overall <- integrated_complete %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(county),
    n_years = n_distinct(year),
    
    # Malnutrition
    mean_gam = round(mean(gam_caseload_6_59, na.rm = TRUE), 0),
    median_gam = round(median(gam_caseload_6_59, na.rm = TRUE), 0),
    max_gam = max(gam_caseload_6_59, na.rm = TRUE),
    mean_sam = round(mean(sam_caseload_6_59, na.rm = TRUE), 0),
    
    # Climate
    mean_rainfall = round(mean(total_rainfall, na.rm = TRUE), 1),
    mean_temp = round(mean(mean_temp, na.rm = TRUE), 1),
    mean_ndvi = round(mean(mean_ndvi, na.rm = TRUE), 3),
    mean_vci = round(mean(mean_vci, na.rm = TRUE), 1),
    
    # Drought
    pct_drought = round(sum(drought_occurred, na.rm = TRUE)/n()*100, 1),
    pct_severe = round(sum(severe_drought, na.rm = TRUE)/n()*100, 1)
  )

cat("\nOverall Summary:\n")
# print(summary_overall)

kableExtra::kable(summary_overall)

# By county type
summary_by_type <- integrated_complete %>%
  group_by(county_type) %>%
  summarise(
    n = n(),
    mean_gam = round(mean(gam_caseload_6_59, na.rm = TRUE), 0),
    mean_sam = round(mean(sam_caseload_6_59, na.rm = TRUE), 0),
    mean_rainfall = round(mean(total_rainfall, na.rm = TRUE), 1),
    pct_drought = round(sum(drought_occurred, na.rm = TRUE)/n()*100, 1),
    cor_rain_gam = round(cor(rainfall_anomaly, gam_caseload_6_59, use = "complete.obs"), 3),
    cor_vci_gam = round(cor(mean_vci, gam_caseload_6_59, use = "complete.obs"), 3)
  )

cat("\nBy County Type:\n")
# print(summary_by_type)
kableExtra::kable(summary_by_type)

# By rainy season
summary_by_season <- integrated_complete %>%
  group_by(rainy_season) %>%
  summarise(
    n = n(),
    mean_gam = round(mean(gam_caseload_6_59, na.rm = TRUE), 0),
    mean_rainfall = round(mean(total_rainfall, na.rm = TRUE), 1),
    pct_drought = round(sum(drought_occurred, na.rm = TRUE)/n()*100, 1),
    cor_rain_gam = round(cor(rainfall_anomaly, gam_caseload_6_59, use = "complete.obs"), 3)
  )

cat("\nBy Rainy Season:\n")
# print(summary_by_season)
kableExtra::kable(summary_by_season)

# ============================================================================
# STEP 10: SAVE ALL DATASETS
# ============================================================================
# cat("\n=== SAVING DATASETS ===\n")

# Main integrated dataset
write.csv(integrated_final, "integrated_climate_malnutrition_final.csv", row.names = FALSE)
# 
# # Complete cases only (for analysis)
# write.csv(integrated_complete, "integrated_complete_cases.csv", row.names = FALSE)
# 
# # Seasonal climate aggregates
# write.csv(seasonal_climate, "seasonal_climate_aggregates.csv", row.names = FALSE)

