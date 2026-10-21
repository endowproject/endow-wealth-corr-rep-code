
# Pull in all of the site data, merge and clean it so it is ready for
# further analysis.

library(dplyr)
library(tidyr)
library(DescTools) ## for Gini() function
library(igraph)
library(stargazer)
library(plm)
library(ggrepel)
library(glmnet)
library(stringr)
library(ggplot2)
library(randomForest)
library(reshape)
library(xtable)
library(Hmisc)


# Load data
path <- file.path(EndowGitHub, "DerivedData")
EndowDerivedData <- path
load(file.path(path, "su_meta.rdata"))
master_df <- readRDS(file.path(path, "su_master.rds"))
externals <- read.csv(file.path(path, "site_externals_descriptives.csv"))
nets <- read.csv(file.path(path, "site_net_descriptives.csv"))
nets_s <- nets[nets$net == "sum", ]
site_df <- merge(nets_s, externals, by = "site")
gini <- read.csv(file.path(path, "gini_site_data.csv"))
site_df <- merge(site_df, gini, by = "site")
ec <- read.csv(file.path(path, "EC_Revised_Reshape.csv"))
site_df <- merge(site_df, ec, by = "site")
kin <- read.csv(file.path(EndowDerivedData, "sitelevel_kinship.csv"))
geospatial <- read.csv(file.path(path, "all_vars_2026-01-14.csv"))
gee <- read.csv(file.path(path, "gee.csv"))
country_data <- read.csv(file.path(path, "site_country_descriptives.csv"))
cia <- read.csv(file.path(EndowDerivedData, "cia_world_factbook.csv"))
hraf <- read.csv(file.path(path, "HRAF.csv"))
survey <- read.csv(file.path(path, "survey_data.csv"))


## select appropriate geospatial values based on year, where relevant
geo_joined <- geospatial %>%
  right_join(
    country_data %>% dplyr::select(SiteCode, Fieldwork.year),
    by = c("site_name" = "SiteCode")
  )

# Year-varying for nightlight: prefer closest prior/exact, fallback to closest future
geo_night <- geo_joined %>%
  filter(var_name == "nightlight") %>%
  group_by(site_name, var_name) %>%
  group_modify(~ {
    prior <- .x %>% filter(year <= .x$Fieldwork.year)
    if (nrow(prior) > 0) {
      prior %>% slice_max(year, n = 1, with_ties = FALSE)
    } else {
      .x %>% slice_min(year, n = 1, with_ties = FALSE)
    }
  }) %>%
  ungroup()

# Pop density: options are practically 2015 or 2020 (done every five years)
# Always use preceding one (so even 2019 gets 2015, not 2020)
geo_pop <- geo_joined %>%
  filter(var_name == "pop_density") %>%
  mutate(target_year = if_else(Fieldwork.year >= 2020, 2020L, 2015L)) %>%
  filter(year == target_year) %>%
  dplyr::select(-target_year)

# Distance-filtered
#geo_road <- geo_joined %>%
#  filter(var_name == "road_density", dist == 15000) # or 5000

# No year recorded (but some seemingly should?)
geo_other <- geo_joined %>%
  filter(var_name %in% c("bedd", "diurnal", "precip_all", "mean_dist_to_cell"))

geospatial <- bind_rows(geo_night, geo_pop, geo_other) %>%
  dplyr::select(site_name, var_name, value) %>%
  pivot_wider(names_from = var_name, values_from = value)

geospatial$site <- geospatial$site_name

site_df <-
  merge(site_df, geospatial, by = "site", all.x = T)


# ======================================================

# Merge on the earth engine data

gee$site_name <- gee$site_code
site_df <- merge(site_df, gee, by = "site_name", all.x = T)

# ======================================================

# Merge on country level data

# vars_to_drop_elly_data <-
#   c("Name",
#     "Economy",
#     "Fieldwork.year",
#     "Wave1_Start",
#     "Wave1_End",
#     "iso2c",
#     "iso3c")
#
# country_data <-
#   country_data[, ! names(country_data) %in% vars_to_drop_elly_data]
#
# country_data <-
#   country_data[, names(country_data)[!grepl("year", names(country_data))]]

site_df <- site_df %>%
  inner_join(country_data, by = c("site" = "SiteCode"))

site_df$site_name <-
  site_df$site

# ------------------------------------------------------

# Add the CIA world factbook data

site_df$country <- tolower(site_df$Country)

## manually fix country names that differ
cia$country <-
  ifelse(cia$country == "congo_republic_of_the",
         "congo-brazzaville",
         cia$country)

cia$country <-
  ifelse(cia$country == "papua_new_guinea",
         "papua new guinea",
         cia$country)

## note literacy is genuinely missing for fiji and dominica...

site_df <- merge(site_df, cia, by = "country", all.x = T)

## Need to drop country variables (country and Country)

site_df$county <- NULL
site_df$Country <- NULL

# ======================================================

# merge kinship


kin$site_name <- kin$site

site_df <-
  merge(site_df, kin, by = "site_name", all.x = T)

# ======================================================

# Add the ethnographer-reported community-level variables

# Add on relevant variables from the google sheet.

hraf$site_name <- hraf$Site

## Clean the communal land variable.

# unique(hraf$CommunalLand)
hraf$CommunalLandOnly <-
  ifelse(hraf$CommunalLand == "communal land use rights only", 1, 0)
hraf$PrivateLandPredominantly <-
  ifelse(hraf$CommunalLand == "land predominantly private property", 1, 0)

# unique(hraf$LandMarket)
hraf$LandMarketExists <-
  ifelse(hraf$LandMarket != "No market exists", 1, 0)

## Clean the Animal Depedence variables, etc.

## hraf$DepAnimals
### Create a mapping from string ranges to numeric midpoints
depanimals_map <- c(
  "0 - 5% Dependence" = 2.5,
  "6 - 15%" = 10.5,
  "16 - 25%" = 20.5,
  "26 - 35%" = 30.5,
  "36 - 45%" = 40.5,
  "46 - 55%" = 50.5,
  "56 - 65%" = 60.5,
  "66 - 75%" = 70.5,
  "76 - 85%" = 80.5,
  "86 - 100%" = 93
)
hraf$DepAnimals_num <- depanimals_map[as.character(hraf$DepAnimals)]

## hraf$DepAgriculture
### Define mapping for agriculture dependence categories
dep_agriculture_map <- c(
  "0 - 5% Dependence" = 2.5,
  "6 - 15%" = 10.5,
  "16 - 25%" = 20.5,
  "26 - 35%" = 30.5,
  "36 - 45%" = 40.5,
  "46 - 55%" = 50.5,
  "56 - 65%" = 60.5,
  "66 - 75%" = 70.5,
  "76 - 85%" = 80.5,
  "86 - 100%" = 93  # Approximate midpoint of final bin
)
hraf$DepAgriculture_num <- dep_agriculture_map[as.character(hraf$DepAgriculture)]

## hraf$DepTrade

# Mapping from text ranges to numeric midpoints for trade dependence
dep_trade_map <- c(
  "0 - 5% Dependence" = 2.5,
  "6 - 15%" = 10.5,
  "16 - 25%" = 20.5,
  "26 - 35%" = 30.5,
  "36 - 45%" = 40.5,
  "46 - 55%" = 50.5,
  "56 - 65%" = 60.5,
  "66 - 75%" = 70.5,
  "76 - 85%" = 80.5,
  "86 - 100%" = 93
)
# Convert to numeric values
hraf$DepTrade_num <- dep_trade_map[as.character(hraf$DepTrade)]



## unique(hraf$ClassStrat)
# Mapping to shorter labels
class_strat_map <- c(
  "Absence of significant class distinctions among freemen (slavery is treated in EA070), ignoring variations in individual repute achieved through skill, valor, piety, or wisdom" = "No class distinctions",
  "Complex stratification into social classes correlated in large measure with extensive differentiation of occupational statuses" = "Complex stratification",
  "Dual stratification into a hereditary aristocracy and a lower class of ordinary commoners or freemen, where traditionally ascribed noble status is at least as decisive as control over scarce resources" = "Dual stratification",
  "Wealth distinctions, based on the possession or distribution of property, present and socially important but not crystallized into distinct and hereditary social classes" = "Wealth distinctions"
)
# Apply mapping
hraf$ClassStrat_short <- class_strat_map[as.character(hraf$ClassStrat)]
## unique(hraf$ClassStrat_short)
hraf$AnyClassDistinctions <-
  ifelse(hraf$ClassStrat_short != "No class distinctions",
         1,
         0)

hraf$StructuralClassDistinctions <-
  ifelse(hraf$ClassStrat_short != "No class distinctions" &
           hraf$ClassStrat_short != "Wealth distinctions",
         1,
         0)

# hraf_vars_to_keep <-
#   c("site_name",
#     "CommunalLandOnly",
#     "PrivateLandPredominantly",
#     "LandMarketExists",
#     "DepAnimals_num",
#     "DepAgriculture_num",
#     "DepTrade_num",
#     "AnyClassDistinctions",
#     "StructuralClassDistinctions")
#
# hraf_sub <-
#   hraf[, hraf_vars_to_keep]

site_df <- merge(site_df, hraf, by = "site_name", all.x = T)

# ------------------------------------------------------------------------------

## merge on the survey data

survey$site_name <- survey$site
q1_vars <-
  c("prop_prod_subsistence",
    "prop_prod_nonmonetary_within",
    "prop_prod_market_within",
    "prop_prod_communal",
    "prop_prod_nonmonetary_outside",
    "prop_prod_market_outside")
q2_vars <-
  c("prop_time_subsistence",
    "prop_time_nonmonetary_within",
    "prop_time_nonmonetary_outside",
    "prop_time_communal",
    "prop_time_salaried_within",
    "prop_time_salaried_outside",
    "prop_time_sale_within",
    "prop_time_sale_outside")
q345_vars <-
  c("prop_most_common_livelihood",
    "how_common_climate_shock",
    "how_common_disease_shock",
    "how_common_violence_shock",
    "how_common_price_shock",
    "strong_norms_on_production")

survey_sub <-
  survey[, c("site_name", q1_vars, q2_vars, q345_vars)]

site_df <- merge(site_df, survey_sub, by = "site_name", all.x = T)


# ======================================================

# Write the Full CSV
message(paste0("Saving the full merged site-level data set to ", file.path(path, "DataForLASSO.csv")))

write.csv(site_df,
          file.path(path, "DataForLASSO.csv"),
          row.names = F)