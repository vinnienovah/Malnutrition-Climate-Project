# Climate-Malnutrition Analysis in Kenya

Analyzing the impact of climate variability on child and maternal malnutrition in Arid and Semi-Arid Lands (ASALs) of Kenya.

## Overview

This project investigates the relationship between climate variability and malnutrition outcomes in Kenya. The analysis integrates climate data (rainfall, temperature, vegetation indices, drought indicators) with malnutrition surveillance data to understand how climate shocks affect:

- Global Acute Malnutrition (GAM) in children 6-59 months
- Severe Acute Malnutrition (SAM) in children 6-59 months
- Moderate Acute Malnutrition (MAM) in children 6-59 months
- Pregnant and Lactating Women (PLW) caseloads

## Research Questions

1. How do malnutrition responses to climate variability differ between ecological zones (Arid ASAL vs Semi-Arid ASAL vs Urban)?
2. Which climate variables most strongly influence under-5 malnutrition outcomes?
3. At what threshold of climate stress do maternal malnutrition caseloads exhibit non-linear increases?

## Study Area

The analysis covers 27 counties across three ecological zones:

**Arid ASAL:** Mandera, Wajir, Isiolo, Tana River, Garissa, Turkana, Marsabit, Samburu, Baringo

**Semi-Arid ASAL:** Tharaka Nithi, Embu, Kajiado, Kilifi, Kitui, Kwale, Laikipia, Lamu, Makueni, Meru, Narok, Nyeri, Taita Taveta, West Pokot, Machakos

**Urban:** Nairobi, Mombasa, Kisumu

## Data Sources

| File | Description |
|------|-------------|
| `moh_mal.csv` | Malnutrition surveillance data from Ministry of Health (2016-2025) |
| `climate_data.csv` | Climate indicators including temperature, precipitation, SPEI, NDVI, VCI (2015-2024) |
| `temp_precip.csv` | Temperature and precipitation data (1950-2024) |

## Requirements

- R version 4.0 or higher
- RStudio (recommended)

### Required R Packages

```r
install.packages("pacman")

pacman::p_load(
  readr, tidyr, tidyverse, dplyr, lubridate, zoo,
  ggplot2, ggcorrplot, viridis, RColorBrewer, patchwork,
  lme4, lmerTest, glmmTMB, mgcv, gratia,
  performance, DHARMa, car,
  sjPlot, ggeffects, kableExtra, flextable,
  caret
)
