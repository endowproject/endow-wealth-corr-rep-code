require(tidyverse)

path <- file.path(EndowGitHub, "DerivedData")

# Load data
gee <- read.csv(
  file.path(EndowGitHub, "gee_output.csv"),
  stringsAsFactors = FALSE,
  header = TRUE)

# For simplicity now, only retaining columns with the actual values
gee <- gee %>%
  dplyr::select(
    site_code,
    year,
    contains("value")
  )

# Simplify column names
colnames(gee) <- gsub("_value", "", colnames(gee))
colnames(gee) <- gsub("_temporalclosest", "", colnames(gee))
colnames(gee) <- gsub("_temporalmean", "", colnames(gee))
colnames(gee) <- gsub("spatial", "", colnames(gee)) # leaves 'mean' or 'median'
colnames(gee) <- gsub("_years[0-9]*", "", colnames(gee))
colnames(gee) <- gsub("_driver", "", colnames(gee))
colnames(gee) <- gsub("(.+?)(\\hii_.*)", "\\2", colnames(gee))
colnames(gee) <- gsub("characteristics_built_characteristics", "built_characteristics", colnames(gee))
colnames(gee) <- gsub("surface_built_surface", "built_surface", colnames(gee))
colnames(gee) <- gsub("buffer", "", colnames(gee))

# Note: default spatial buffer around the exact coordinates is 250 meters. Partially done to help some coastal sites (KM, FJ, AH) that might otherwise fall outside cells


# Datasets/variables currently included:
# "CSP_gHM": Global Human Modification representing the degree to which terrestrial lands have been modified by humans on a scale of 0.0 to 1.0

# "hii": like all other HII drivers, unitless; it refers to an absolute 0-10 scale but is not normalized to it, so the actual range of values may be smaller than 0-10. Values are multiplied by 100 and converted to integer for efficient exporting to and storage in the Earth Engine

# "hii_infrastucture": the anthropogenic impact of human infrastructure (other than roads and railways) on the terrestrial surface

# "hii_landuse": the impact of anthropogenic land use on the terrestrial surface. "Impact" is a pressure score based on a combination of land use/land cover and population density. Draws mostly on OSM infrastructure types

# "hii_popdens": the impact of population density on the terrestrial surface. "Impact" is a logarithmically scaled pressure score based on population density

# "hii_power" : the impact of power on the terrestrial surface. "Impact" is a pressure score based on the intensity of electricity usage as measured by "Night Time Lights" datasets

# "hii_railway": the anthropogenic impact of railways on the terrestrial surface. Uses OSM railway data

# "hii_road": the anthropogenic impact of roads on the terrestrial surface. Uses OSM road data

# "hii_water" : the impact of navigable waterways on the terrestrial surface. "Impact" is a pressure score based on proximity to a navigable waterway. Coasts, wide rivers and lakes are considered navigable if they meet key criteria related to the distance from a population center (for coastlines including that of the Caspian Sea) or based on width and connectivity (inland waters).

# "GHSL_built_characteristics" : note that for some reason most entries are 0, which isn't actually in standard coding
# GHSL: Global settlement characteristics (10 m) 2018 built_characteristics:
  # 1: open spaces, low vegetation surfaces
  # 2: open spaces, medium vegetation surfaces
  # 3: open spaces, high vegetation surfaces
  # 4: open spaces, water surfaces
  # 5: open spaces, road surfaces
  # 11: built spaces, residential, building height <= 3m
  # 12: built spaces, residential, 3m < building height <= 6m
  # 13: built spaces, residential, 6m < building height <= 15m
  # 14: built spaces, residential, 15m < building height <= 30m
  # 15: built spaces, residential, building height > 30m
  # 21: built spaces, non-residential, building height <= 3m
  # 22: built spaces, non-residential, 3m < building height <= 6m
  # 23: built spaces, non-residential, 6m < building height <= 15m
  # 24: built spaces, non-residential, 15m < building height <= 30m
  # 25: built spaces, non-residential, building height > 30m

# "WorldPop_population" : Estimated number of people residing in each grid cell

# "GPW_population_density" : The estimated number of persons per square kilometer.

# "MAP_accessibility" : land-based travel time to the nearest densely-populated area. Densely-populated areas are defined as contiguous areas with 1,500 or more inhabitants per square kilometer or a majority of built-up land cover types coincident with a population center of at least 50,000 inhabitants.

# "RAI_multiplier_b1" : The final passability index is measured on a scale of 0–1, with 1 being 100% probability that the roads are all-season. Note: many cells deemed not to have roads are masked, i.e. NA values. This is called the 'multiplier' because this is multipled by the rural population values to arrive at the rural population with access to an all-season road.

# "RAI_ruralpop_population" : derived from WorldPop data, exclusively for rural population. Because this excludes urban population, it is potentially inaccurate (e.g., for some of the sites in South Asia, which are potentially close enough to places that would not be deemed 'rural')

# "RAI_ruralpopaccess_population" : rural population with access to an all-season road. Note, this should be derived from multiplying "RAI_multiplier_b1" by "RAI_ruralpop_population". But, this doesn't always seem to align with what we could get by trying to rederive this. Most notably, there are some cases where the RAI_ruralpopaccess_population is greater than the RAI_ruralpop_population, which doesn't make sense. This holds even when putting the spatial buffer at 0, so it should be only for the exact cell. Unclear why this would be the case.

# NOTE: the 'actual' RAI index is meant to be the rural population with access to an all-season road divided by the total rural population. This would seem straightforward to calculate from our two variables, BUT because of the aggregation across the different cells, it isn't as straightforward, I think. For example, we end up with some cases where RAI_ruralpopaccess_population > RAI_ruralpop_population


# "GDP_PPP_b26" : Gridded GDP per capita, derived from a combination of sub-national and national datasets from 2015. Gross Domestic Production per capita (purchasing power parity), in constant 2011 international USD

# "HDI_b26" : Gridded HDI, derived from a combination of sub-national and national datasets for 2015. Human Development Index, based on method introduced 2010 and updated 2011. Dimensionless indicator between 0 and 1.


# "TERRACLIMATE_aet" : Actual evapotranspiration in millimeters, as measured using a one-dimensional soil water balance model
# "TERRACLIMATE_def" : Climate water deficit in millimeters, as measured using a one-dimensional soil water balance model
# "TERRACLIMATE_pdsi" : Palmer Drought Severity Index
# "TERRACLIMATE_pet" : Reference evapotranspiration in millimeters
# "TERRACLIMATE_pr" : Precipitation accumulation in millimeters
# "TERRACLIMATE_ro" : Runoff, as measured using a one-dimensional soil water balance model in millimeters
# "TERRACLIMATE_soil" : Soil moisture in millimeters, as measuring using a one-dimensional soil water balance model
# "TERRACLIMATE_srad" : Downward surface shortwave radiation in W/m^2
# "TERRACLIMATE_swe" : Snow water equivalent in millimeters, as measured using a one-dimensional soil water balance model
# "TERRACLIMATE_tmmn" : Minimum temperature in °C
# "TERRACLIMATE_tmmx" : Maximum temperature in °C
# "TERRACLIMATE_vap" : Vapor pressure in kPa
# "TERRACLIMATE_vpd" : Vapor pressure deficit in kPa
# "TERRACLIMATE_vs" : Wind-speed at 10m in m/s

# "GRACE_lwe_thickness" : Equivalent liquid water thickness in centimeters

# "GRACE_uncertainty" : 1-sigma uncertainty for each 3-degree mascon estimate

# NOTE: the GRACE drought severity index is meant to be calculated from this:
# (GRACE_lwe_thickness_{year, month} - GRACE_lwe_thickness_{month}) / GRACE_uncertainty_{month}
# The GRACE-DSI is a dimensionless quantity that detects both drought and abnormally wet events: less than −2.0 is an exceptional drought, from −1.99 to −1.60 is an extreme drought, from −1.59 to −1.30 is a severe drought, from −1.29 to −0.80 is a moderate drought, from −0.79 to −0.50 is abnormally dry, from −0.49 to 0.49 is near normal, from 0.50 to 0.79 is slightly wet, from 0.80 to 1.29 is moderately wet, from 1.30 to 1.59 is very wet, from 1.60 to 1.99 is extremely wet, and higher than 2.0 is exceptionally wet.
# Since we have already aggregated things with this measure, this straightforward calculation cannot be derived from these data.

# "SPEI_SPEI_01_month" : "SPEI_SPEI_48_month": Standardized Precipitation-Evapotranspiration Index (SPEI) where precipitation and evapotranspiration data was accumulated over the previous month ... to past 48 months. The Standardized Precipitation-Evapotranspiration Index (SPEI) expresses, as a standardized variate (mean zero and unit variance), the deviations of the current climatic balance (precipitation minus evapotranspiration potential) with respect to the long-term balance. The reference period for the calculation in the SPEIbase corresponds to the whole study period. Being a standardized variate means that the SPEI condition can be compared across space and time.
# NOTE: this is currently calculated NOT with the actual month of data collection, but starting in January for all, so should only really use at least the 12-month entry (or revise to use actual month of data collection, which is possible)


# "Hansen_treecover2000" : Tree canopy cover for year 2000 in %, defined as canopy closure for all vegetation taller than 5m in height
# "Hansen_loss" : Binary indicator for forest loss during the study period (0: Not loss; 1: Loss), defined as a stand-replacement disturbance (a change from a forest to non-forest state)
# "Hansen_gain" : Binary indicator of forest gain during the period 2000-2012 (0: No gain; 1: Gain), defined as the inverse of loss (a non-forest to forest change entirely within the study period)
# "Hansen_lossyear" : Year of gross forest cover loss event, encoded as an integer (0 for no loss, or a value in the range 1-24 representing loss detected primarily in the year 2001-2024, respectively)
# "Hansen_first_b30" : Landsat Red cloud-free image composite (corresponding to Landsat 5/7 band 3, 4, 5, and 7 and Landsat 8/9 band 4, 5, 6, and 7). Reference multispectral imagery from the first available year, typically 2000
# "Hansen_first_b40" : Landsat NIR cloud-free image composite (corresponding to Landsat 5/7 band 4 and Landsat 8/9 band 5). Reference multispectral imagery from the first available year, typically 2000
# "Hansen_first_b50" : Landsat SWIR1 cloud-free image composite (corresponding to Landsat 5/7 band 5 and Landsat 8/9 band 6). Reference multispectral imagery from the first available year, typically 2000
# "Hansen_first_b70" : Landsat SWIR2 cloud-free image composite (corresponding to Landsat 5/7 band 7 and Landsat 8/9 band 7). Reference multispectral imagery from the first available year, typically 2000
# "Hansen_last_b30" : Landsat Red cloud-free image composite (corresponding to Landsat 5/7 band 3 and Landsat 8/9 band 4). Reference multispectral imagery from the last available year, typically the last year of the study period
# "Hansen_last_b40" : Landsat NIR cloud-free image composite (corresponding to Landsat 5/7 band 4 and Landsat 8/9 band 5). Reference multispectral imagery from the last available year, typically the last year of the study period
# "Hansen_last_b50" : Landsat SWIR1 cloud-free image composite (corresponding to Landsat 5/7 band 5 and Landsat 8/9 band 6). Reference multispectral imagery from the last available year, typically the last year of the study period
# "Hansen_last_b70" : Landsat SWIR2 cloud-free image composite (corresponding to Landsat 5/7 band 7 and Landsat 8/9 band 7). Reference multispectral imagery from the last available year, typically the last year of the study period
# "Hansen_datamask" : Three values representing areas of no data (0), mapped land surface (1), and permanent water bodies (2)


# "CHIRPS_precipitation" : Precipitation in mm for a 5 day period

# "SRTM_elevation" : Elevation in meters

# "CCNL_b1" : Corrected nighttime light intensity in 2013

# "GHSL_smod_code"
  # -200: No Data
  # 10: Water
  # 11: Very low density rural
  # 12: Low density rural
  # 13: Rural cluster
  # 21: Suburban or peri-urban
  # 22: Semi-dense urban cluster
  # 23: Dense urban cluster
  # 30: Urban centre

# "CISI_b1" : Global Critical Infrastructure Spatial Index. The CISI ranges between 0 (no CI) and 1 (highest CI intensity). The CISI normalized at the global scale gives valuable information on where certain amounts of infrastructure are located.


# Recoding GHSL built characteristics and Degree of Urbanization (smod) variables for easier interpretation
gee <- gee %>%
  mutate(
    # variable mostly 0s, so leaving as is
    # GHSL_built_characteristics_250_median = case_when(
    #   GHSL_built_characteristics_250_median == 1 ~ "open spaces, low vegetation surfaces",
    #   GHSL_built_characteristics_250_median == 2 ~ "open spaces, medium vegetation surfaces",
    #   GHSL_built_characteristics_250_median == 3 ~ "open spaces, high vegetation surfaces",
    #   GHSL_built_characteristics_250_median == 4 ~ "open spaces, water surfaces",
    #   GHSL_built_characteristics_250_median == 5 ~ "open spaces, road surfaces",
    #   GHSL_built_characteristics_250_median == 11 ~ "built spaces, residential, building height <= 3m",
    #   GHSL_built_characteristics_250_median == 12 ~ "built spaces, residential, 3m < building height <= 6m",
    #   GHSL_built_characteristics_250_median == 13 ~ "built spaces, residential, 6m < building height <= 15m",
    #   GHSL_built_characteristics_250_median == 14 ~ "built spaces, residential, 15m < building height <= 30m",
    #   GHSL_built_characteristics_250_median == 15 ~ "built spaces, residential, building height > 30m",
    #   GHSL_built_characteristics_250_median == 21 ~ "built spaces, non-residential, building height <= 3m",
    #   GHSL_built_characteristics_250_median == 22 ~ "built spaces, non-residential, 3m < building height <= 6m",
    #   GHSL_built_characteristics_250_median == 23 ~ "built spaces, non-residential, 6m < building height <= 15m",
    #   GHSL_built_characteristics_250_median == 24 ~ "built spaces, non-residential, 15m < building height <= 30m",
    #   GHSL_built_characteristics_250_median == 25 ~ "built spaces, non-residential, building height > 30m",
    #   TRUE ~ as.character(GHSL_built_characteristics_250_median)  # Keep original value if no match
    # ),
    GHSL_smod_code_250_median = case_when(
      GHSL_smod_code_250_median == -200 ~ "No Data",
      GHSL_smod_code_250_median == 10 ~ "Water",
      GHSL_smod_code_250_median == 11 ~ "Very low density rural",
      GHSL_smod_code_250_median == 12 ~ "Low density rural",
      GHSL_smod_code_250_median == 13 ~ "Rural cluster",
      GHSL_smod_code_250_median == 21 ~ "Suburban or peri-urban",
      GHSL_smod_code_250_median == 22 ~ "Semi-dense urban cluster",
      GHSL_smod_code_250_median == 23 ~ "Dense urban cluster",
      GHSL_smod_code_250_median == 30 ~ "Urban centre",
      TRUE ~ as.character(GHSL_smod_code_250_median)  # Keep original value if no match
    )
  )


# derive RAI rural population with access to an all-season road; note, doesn't quite match provided one
# note that because the accessibility multiplier has NAs for many, these are carried through leading to many NA values
# similarly, some of the rural population values are missing, some because treated as water or treated as non-rural
gee <- gee %>%
  mutate(
    RAI_ruralpopaccess_population_0_derived = RAI_multiplier_b1_0_mean * RAI_ruralpop_population_0_mean,
    RAI_ruralpopaccess_population_250_derived = RAI_multiplier_b1_250_mean * RAI_ruralpop_population_250_mean,
    RAI_ruralpopaccess_population_1000_derived = RAI_multiplier_b1_1000_mean * RAI_ruralpop_population_1000_mean
  )

write.csv(gee, file.path(path, "gee.csv"), row.names = FALSE)