library(acid) # weighted.gini
library(data.table)
library(dplyr)
library(tidyr)
library(DescTools) ## for Gini() function
library(ggplot2)
library(stargazer)

## load data
figpath <- file.path(EndowDropbox, "Figures")
tables_path <- file.path(EndowDropbox, "Tables")
EndowDerivedData <- file.path(EndowGitHub, "DerivedData")

su_master <- readRDS(file.path(EndowDerivedData, "su_master.rds"))
su_externals <- read.csv(file.path(EndowDerivedData, "su_externals_df.csv"))


## aggregate to site level characteristics plots

## instead of pre-emptively dropping cases with any missing values
## define custom functions that drop relevant cases for each one

clean_vec <- function(x) {
  x[!is.na(x) & !is.nan(x) & is.finite(x)]
}

safe_mean   <- function(x) { x <- clean_vec(x); if (length(x) == 0) NA_real_ else mean(x) }
safe_median <- function(x) { x <- clean_vec(x); if (length(x) == 0) NA_real_ else median(x) }
safe_max    <- function(x) { x <- clean_vec(x); if (length(x) == 0) NA_real_ else max(x) }
safe_sd     <- function(x) { x <- clean_vec(x); if (length(x) < 2) NA_real_ else sd(x) }
safe_cv     <- function(x) { x <- clean_vec(x); if (length(x) < 2) NA_real_ else sd(x) / mean(x) }

safe_gini <- function(x) {
  x <- clean_vec(x)
  if (length(x) < 2) return(NA_real_)
  Gini(x)
}

safe_quantile_ratio <- function(x, p1, p2) {
  x <- clean_vec(x)
  if (length(x) < 2) return(NA_real_)
  quantile(x, p1, names = FALSE) / quantile(x, p2, names = FALSE)
}

# For ratios (e.g. degree/adult_count) - need num AND denom valid together
clean_ratio <- function(num, denom) {
  ok <- !is.na(num) & !is.nan(num) & is.finite(num) &
        !is.na(denom) & !is.nan(denom) & is.finite(denom) & denom != 0
  num[ok] / denom[ok]
}

safe_ratio_gini <- function(num, denom) {
  r <- clean_ratio(num, denom)
  if (length(r) < 2) return(NA_real_)
  Gini(r)
}

safe_ratio_cv <- function(num, denom) {
  r <- clean_ratio(num, denom)
  if (length(r) < 2) return(NA_real_)
  sd(r) / mean(r)
}

# weighted.gini needs paired cleaning between value and weight
safe_weighted_gini <- function(x, w) {
  ok <- !is.na(x) & !is.nan(x) & is.finite(x) &
        !is.na(w) & !is.nan(w) & is.finite(w)
  if (sum(ok) < 2) return(NA_real_)
  as.numeric(weighted.gini(x[ok], w[ok])$Gini)
}


union_data <- su_master %>%
  filter(network == "sum") %>%
  data.table()

gini_dat <-
  union_data[,
    .(
      mean_wealth_per_capita = safe_mean(wealth_per_capita),
      max_wealth_per_capita = safe_max(wealth_per_capita),
      median_wealth_per_capita = safe_median(wealth_per_capita),
      sd_wealth_per_capita = safe_sd(wealth_per_capita),
      wealth_pc_80_20 = safe_quantile_ratio(wealth_per_capita, 0.8, 0.2),
      wealth_pc_90_10 = safe_quantile_ratio(wealth_per_capita, 0.9, 0.1),
      mean_wealth = safe_mean(wealth),
      max_wealth = safe_max(wealth),
      median_wealth = safe_median(wealth),
      sd_wealth = safe_sd(wealth),
      gini_wealth_per_capita = safe_gini(wealth_per_capita),
      gini_weighted_wealth_per_capita = safe_weighted_gini(wealth_per_capita, su_size),
      gini_wealth_per_adult = safe_gini(wealth_per_adult),
      gini_weighted_wealth_per_adult = safe_weighted_gini(wealth_per_adult, adult_count),
      gini_wealth = safe_gini(wealth),
      #gini_wealth_per_able = safe_gini(wealth_per_able),
      #gini_weighted_wealth_per_able = safe_weightedgini(wealth_per_able, able_count),
      gini_size_adjusted_wealth = safe_gini(size_adjusted_wealth),
      gini_degree_centrality = safe_gini(degree),
      deg_gini = safe_gini(degree), ## repeated from the prior row so as not to break downstream code
      ideg_gini = safe_gini(in_degree),
      odeg_gini = safe_gini(out_degree),
      ideg_pa_gini = safe_ratio_gini(in_degree, adult_count),
      odeg_pa_gini = safe_ratio_gini(out_degree, adult_count),
      ideg_pc_gini = safe_ratio_gini(in_degree, su_size),
      odeg_pc_gini = safe_ratio_gini(out_degree, su_size),
      deg_pa_gini = safe_ratio_gini(degree, adult_count),
      ideg_cv = safe_cv(in_degree),
      odeg_cv = safe_cv(out_degree),
      deg_cv = safe_cv(degree),
      ideg_pa_cv = safe_ratio_cv(in_degree, adult_count),
      odeg_pa_cv = safe_ratio_cv(out_degree, adult_count),
      deg_pa_cv = safe_ratio_cv(degree, adult_count),
      ideg_size_adjusted_gini = safe_gini(size_adjusted_indegree),
      odeg_size_adjusted_gini = safe_gini(size_adjusted_outdegree)
    ),
    by = .(site)
  ] ## already subsetted above to one network layer

externals_dat <- data.table(su_externals)

externals_site_dat <-
  externals_dat[,
    .(
      ext_count_gini = safe_gini(externals_count),
      ext_count_pa_gini = safe_ratio_gini(externals_count, adult_count),
      ext_count_pc_gini = safe_ratio_gini(externals_count,  su_size),
      ext_count_cv = safe_cv(externals_count),
      ext_count_pa_cv = safe_ratio_cv(externals_count, adult_count)
    ),
    by = .(site)
  ]

gini_dat <-
  merge(gini_dat, externals_site_dat, by = "site")

## save gini dat
message(paste0("gini_site_data.csv written to ", EndowDerivedData))

write.csv(gini_dat, file.path(EndowDerivedData, "gini_site_data.csv"))
