# ============================================================
# Compile SU Wealth
# ============================================================

# This script compiles the SU wealth calculations for each site.
# For each site, we align the PossCost and SU data and review completeness.
# Where necessary, we impute missing data with MICE.
# Fully unobserved SUs are left as NAs, and not included in imputation.
# We also "noise up" the wealth calculations by randomly adjusting each
# item count/valuation, and see how this affects the resulting wealth distributions.
# Most scripts are quick to run; EK is the slowest at ~15 minutes (because of imputation).
# There are notes with the calls below and in the site scripts to review.
# A few scripts pull in files from the site repos, so won't run for those without access.

require("dplyr")
require("tidyr")
require('ggplot2')
require('readxl')
require('mice')
require('fastDummies')
require('splitstackshape')

set.seed(5) # for mice imputation -- make sure it replicates

## Specify global parameters
trials_for_noise <- 50
trials_for_imputation <- 50
max_noise <- 0.2
wealth_dir <- file.path(EndowGitHub, "su-wealth-output/")
noised_list <- list() # to store noise trial outputs for all sites
imputation_list <- list() # to store imputed datasets for all sites
su_wealth_list <- list()

# LOCAL FUNCTIONS:

get_item_value <- function(item_name, su_df, max_noise = 0) {
  noise_multiplier <- runif(1, min = 1 - max_noise, max = 1 + max_noise)
  price_qty_multiple <- su_df[, grep(item_name, colnames(su_df))] *
    pc[grep(item_name, pc$item), "cost"] * noise_multiplier
  if (class(price_qty_multiple) == "data.frame") {
    return(rowSums(price_qty_multiple))
  } else {
    return(price_qty_multiple)
  }
}

get_data <- function(site_name) {
  site_dir <<- paste0("../endow-", site_name)
  data_dir <<- paste0(site_dir, "/primary-sources/wave-1/data/")
  metadata_dir <<- paste0(site_dir, "/primary-sources/wave-1/metadata/")
  su <<- read.csv(file.path(EndowDatabase, "su_observations", paste0(site_name, "_su_observations.csv")), as.is = TRUE, header = TRUE)
  pc <<- read.csv(file.path(EndowDatabase, "possession_costs", paste0(site_name, "_possession_costs.csv")), as.is = TRUE, header = TRUE)
  indiv <<- read.csv(file.path(EndowDatabase, "people_observations", paste0(site_name, "_people_observations.csv")), as.is = TRUE, header = TRUE)
  su_sizes <<- indiv %>%
    dplyr::group_by(su_id) %>%
    dplyr::summarise(size_su = sum(is_resident == 1, na.rm = TRUE))
  su <<- merge(su, su_sizes, by = "su_id", all.x = TRUE)
}

# ---- 0. Diagnostic checks (for manual review) ----

vars_to_ignore <- c(
  "su_id", "waveid",
  "smaller_meals", "fewer_meals", "no_food", "sleep_hungry", "without_eating",
  "malehead", "femalehead",
  "malehhfatheredu", "malehhfathernoetic", "malehhfathernoeticother", "malehhfatherothernoetic",
  "malehhmotheredu", "malehhmothernoetic", "malehhmothernoeticother", "malehhmotherothernoetic",
  "femalehhfatheredu", "femalehhfathernoetic", "femalehhfathernoeticother", "femalehhfatherothernoetic",
  "femalehhmotheredu", "femalehhmothernoetic", "femalehhmothernoeticother", "femalehhmotherothernoetic",
  "size_su")

check_site_data <- function(
  su_df,
  pc_df,
  final_item_list) {
  other_hh_vars <- grep("^(male|female)h", colnames(su_df), value = TRUE)
  unpriced <- setdiff(colnames(su_df), pc_df$item)
  unpriced <- setdiff(unpriced, vars_to_ignore)
  unpriced <- setdiff(unpriced, other_hh_vars)
  unenumerated <- setdiff(pc_df$item, colnames(su_df))
  priced_zero <- pc_df$item[which(pc_df$cost == 0)]
  priced_na <- pc_df$item[which(is.na(pc_df$cost))]

  cat("\n\n=======================\n\n")
  cat(site_name, "data checks:\n\n")

  cat("Items in SU but not in PC (unpriced):\n")
  if (length(unpriced)) cat(" ", paste(unpriced, collapse = ", "), "\n\n")
  else cat(" <none>\n\n")

  cat("Items in PC but not in SU (unenumerated):\n")
  if (length(unenumerated)) cat(" ", paste(unenumerated, collapse = ", "), "\n\n")
  else cat(" <none>\n\n")

  cat("Items priced as zero:\n")
  if (length(priced_zero)) cat(" ", paste(priced_zero, collapse = ", "), "\n\n")
  else cat(" <none>\n\n")

  cat("Items priced as NA:\n")
  if (length(priced_na)) cat(" ", paste(priced_na, collapse = ", "), "\n\n")
  else cat(" <none>\n\n")

  cat("Missing data pattern (wealth items only):\n")
  mice::md.pattern(su_df[, final_item_list], plot = FALSE)
}


# ---- 1. Summarise item-level quantities ----

make_item_summary <- function(
  su_df,
  final_item_list,
  pc_df) {
  summary <- su_df %>%
    pivot_longer(all_of(final_item_list), names_to = "item", values_to = "value") %>%
    dplyr::group_by(item) %>%
    dplyr::summarise(
      min      = min(value,  na.rm = TRUE),
      median   = median(value, na.rm = TRUE),
      max      = max(value,  na.rm = TRUE),
      mean     = round(mean(value, na.rm = TRUE), 2),
      sd       = round(sd(value,   na.rm = TRUE), 2),
      prop_na  = round(sum(is.na(value))             / nrow(su_df), 2),
      prop_zero= round(sum(value == 0, na.rm = TRUE) / nrow(su_df), 2),
      .groups  = "drop"
    )
  summary$cost <- pc_df$cost[match(summary$item, pc_df$item)]

  summary
}

# ---- 2. Calculate cash value from item list ----
# baseline_flat: fixed amount added once per SU (default 0)
# baseline_per_capita: amount multiplied by size_su and added (default 0)
# Both can be used together; exchange_rate_to_usd applied after baseline

calculate_cash_value <- function(
  su_df,
  final_item_list,
  baseline_flat = 0,
  baseline_per_capita = 0,
  exchange_rate_to_usd = 1, # default assumes already in USD
  max_noise = 0) {
  su_df$cash_value <- 0
  for (item_name in final_item_list) {
    su_df$cash_value <- su_df$cash_value +
      get_item_value(paste0("^", item_name, "$"), su_df, max_noise = max_noise)
  }
  su_df$cash_value <- su_df$cash_value +
    baseline_flat +
    baseline_per_capita * su_df$size_su
  su_df$cash_value <- su_df$cash_value * exchange_rate_to_usd
  su_df
}

# ---- 3. Plot item-level distributions ----

plot_item_values <- function(
  su_df,
  final_item_list,
  pc_df,
  site_name,
  wealth_dir,
  exchange_rate_to_usd = 1,
  plot_height = 7) {
  su_costed <- su_df
  for (item_name in final_item_list) {
    if (item_name %in% colnames(su_costed)) {
      su_costed[[item_name]] <- su_costed[[item_name]] *
        pc_df$cost[pc_df$item == item_name] * exchange_rate_to_usd
    }
  }
  su_costed <- su_costed %>%
    pivot_longer(
      cols = any_of(final_item_list),
      names_to = "item", values_to = "value"
    )

  ggsave(
    filename = paste0(wealth_dir, "plots/", site_name, "_item_values.pdf"),
    plot = ggplot(su_costed, aes(x = item, y = value)) +
      geom_boxplot() +
      labs(title = site_name, x = "Item", y = "Value") +
      scale_y_continuous(trans = "sqrt", labels = scales::comma) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
    device = "pdf",
    width  = 7,
    height = plot_height
  )
}

# ---- 4. MICE imputation ----

# Returns su_df with final_item_list columns replaced by imputed medians,
# plus the list of m complete datasets for downstream use.
# keep_as_na: vector of su_ids to exclude from imputation and retain as NA rows.

run_imputation <- function(
  su_df,
  final_item_list,
  trials_for_imputation,
  keep_as_na = NULL,
  extra_cols = NULL,
  aggregate = c("median", "mean")) {
    aggregate <- match.arg(aggregate)
    # Columns to impute
    imp_cols <- c(final_item_list, extra_cols)
    su_wealth <- su_df[, imp_cols, drop = FALSE]
    ids <- su_df$su_id

    # Split out the always-NA rows
    if (!is.null(keep_as_na) && length(keep_as_na) > 0) {
      to_add    <- su_wealth[ids %in% keep_as_na, , drop = FALSE]
      su_wealth <- su_wealth[!ids %in% keep_as_na, , drop = FALSE]
      ids_imp   <- ids[!ids %in% keep_as_na]
    } else {
      to_add  <- NULL
      ids_imp <- ids
    }

    cat("Performing imputation with mice for", site_name, ".\n\n")
    imputed_info <- mice(su_wealth, m = trials_for_imputation, printFlag = FALSE)
    imputed_list <- lapply(1:trials_for_imputation, function(i) complete(imputed_info, i))

    stacked <- abind::abind(imputed_list, along = 3)
    agg_vals <- if (aggregate == "median") {
      apply(stacked, c(1, 2), median)
    } else {
      rowMeans(stacked, dims = 2)
    }

    # Warn about any residual NAs
    n_residual_na <- sum(is.na(agg_vals))
    if (n_residual_na > 0) {
      na_items <- colnames(agg_vals)[colSums(is.na(agg_vals)) > 0]
      warning(sprintf(
        "%s: %d residual NA(s) remain after %s-aggregating %d imputed datasets, in column(s): %s. Inspect before proceeding.",
        su_df$su_id[1], # rough site identifier from first row
        n_residual_na,
        aggregate,
        trials_for_imputation,
        paste(na_items, collapse = ", ")
      ))
    }

    # Re-attach the kept-NA rows and restore original row order
    if (!is.null(to_add)) {
      agg_vals <- rbind(agg_vals, to_add)
      imputed_list <- lapply(imputed_list, function(df) {
        df <- rbind(df, to_add)
        df[match(ids, ids_imp), , drop = FALSE]
      })
    }
    agg_vals <- agg_vals[match(ids, ids_imp), , drop = FALSE]

    su_df[, imp_cols] <- agg_vals

    list(su = su_df, imputed_list = imputed_list)
}

# ---- 5. Noise trials ----

run_noise_trials <- function(
  su_df,
  final_item_list,
  trials_for_noise = 50,
  max_noise,
  baseline_flat = 0,
  baseline_per_capita = 0,
  exchange_rate_to_usd = 1) {
    noise_list <- lapply(1:trials_for_noise, function(x) su_df)

    for (i in 1:trials_for_noise) {
      noise_list[[i]] <- calculate_cash_value(
        noise_list[[i]], final_item_list,
        baseline_flat = baseline_flat,
        baseline_per_capita = baseline_per_capita,
        exchange_rate_to_usd = exchange_rate_to_usd,
        max_noise = max_noise
      )
      noise_list[[i]] <- noise_list[[i]] %>%
        dplyr::select(c("su_id", "size_su", "cash_value"))
    }
    noise_list
}

# ---- 6. Report Gini + save outputs ----

report_and_save <- function(
  su_df,
  noise_list,
  site_name,
  wealth_dir,
  item_summary,
  imputed_list = NULL,
  trials_for_noise = 50) {
    # Main wealth output
    su_wealth <<- su_df %>% dplyr::select(c("su_id", "size_su", "cash_value"))
    su_wealth_list[[site_name]] <<- su_wealth
    write.csv(
      su_wealth,
      paste0(wealth_dir, site_name, "_SU_wealth.csv"),
      row.names = FALSE
    )

    # Gini
    cat(site_name, "Gini point estimate:",
        round(DescTools::Gini(su_df$cash_value, na.rm = TRUE), 3), "\n")
    cat(site_name, "Gini with noise range:\n")
    print(summary(unlist(lapply(noise_list,
      function(df) DescTools::Gini(df$cash_value, na.rm = TRUE)))))

    # Noise list
    #saveRDS(noise_list, paste0(wealth_dir, site_name, "_noise_list.rds"))
    noised_list[[site_name]] <<- noise_list

    # Imputed list (optional)
    if (!is.null(imputed_list)) {
      #saveRDS(imputed_list, paste0(wealth_dir, site_name, "_imputed_list.rds"))
      imputation_list[[site_name]] <<- imputed_list
    } else {
      imputation_list[[site_name]] <<- lapply(1:trials_for_noise, function(x) su_df)
    }

    # Item summary
    write.csv(item_summary,
      paste0(wealth_dir, "item_summaries/", site_name, "_item_summary.csv"),
      row.names = FALSE
    )
}


## Now source each site's script

source("su-wealth-calculations/CalculateSUwealth_EG.R")
source("su-wealth-calculations/CalculateSUwealth_EX.R")
source("su-wealth-calculations/CalculateSUwealth_SI.R")
source("su-wealth-calculations/CalculateSUwealth_SM.R")

# 
# source("su-wealth-calculations/CalculateSUwealth_AH.R")
# ## 6 fully unobserved SUs, not imputed; a few items still needing valuations
# 
# source("su-wealth-calculations/CalculateSUwealth_AR.R")
# ## One very wealthy household, so huuuuge Gini!
# 
# source("su-wealth-calculations/CalculateSUwealth_AV.R")
# ## There are individual values for basically all items, which vary between SUs
# ## This results in a total_wealth variable which we could use,
# ##  instead of one derived from the average value of items included in the PossCost file
# ## Has cash, bank_account, mobile_money, debt - not including these for now
# ## Quite a number of SUs with 0 wealth (beyond the baseline)
# 
# source("su-wealth-calculations/CalculateSUwealth_BD.R")
# ## Unusual structure!
# ## Small imputation
# ## noise trials don't include the house structure variables!
# 
# ## BF: currently lacking PossCost file (and collection-entry file with baseline)
# 
# source("su-wealth-calculations/CalculateSUwealth_BM.R")
# ## 1 SU with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_BY.R")
# ## 2 SUs with fully missing info, left as NA
# 
# ## BZ: PossCost file not fully resolved (data not through checks, etc)
# 
# source("su-wealth-calculations/CalculateSUwealth_CP.R")
# 
# source("su-wealth-calculations/CalculateSUwealth_CR.R")
# ## some 20+ SUs with fully missing info, left as NA
# ## some imputation, and site-specific code for some semi-nested units
# 
# ## DA: don't have full data submitted yet (but do have SU and PossCost, so could try)
# 
# source("su-wealth-calculations/CalculateSUwealth_DJ.R")
# ## There are individual values for basically all items, which vary between SUs
# ## This results in a total_wealth variable which we could use,
# ## instead of one derived from the average value of items included in the PossCost file
# ## Has cash, bank_account, mobile_money, debt - not including these for now
# ## Quite a number of SUs with 0 wealth (beyond the baseline)
# 
# source("su-wealth-calculations/CalculateSUwealth_EK.R")
# ## imputation takes a *long time*! Still has some NAs which are then just forced to 0s!
# ## 4 SUs with fully missing info, turned to NA
# ## land not valued, as apparently usufruct and not-monetizable
# 
# source("su-wealth-calculations/CalculateSUwealth_FF.R")
# ## no baseline (all enumerated), one missing sofa imputed to 0
# 
# source("su-wealth-calculations/CalculateSUwealth_FJ.R")
# ## no baseline, has info on income and detailed breakdown and pricing of house construction
# ## small missingness, imputed with mice, takes time to impute
# ## 5 SUs with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_HE.R")
# ## has some information on crop yields and earnings and income, excluding these!
# ## one SU with missing data, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_HI.R")
# ## pulls in file from endow-HI repo, so won't work if don't have access to this!
# ## Includes some imputation of cows, drops goats and rifles
# ## Unclear if baseline meant to include livestock
# 
# source("su-wealth-calculations/CalculateSUwealth_IH.R")
# ## has "money" and "debts" (also monthly income); currently include neither
# 
# source("su-wealth-calculations/CalculateSUwealth_KA.R")
# ## no missingness
# 
# source("su-wealth-calculations/CalculateSUwealth_KM.R")
# ## 26 SUs with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_KO.R")
# ## no missingness
# ## values quite different from anirudh's earlier file!
# ## seems to be because of farmed land not being included earlier?
# 
# source("su-wealth-calculations/CalculateSUwealth_KS.R")
# ## comma versus period issue in Poss_Cost
# ## missing input from ethnog so don't have baseline, not sure on currency
# ## missing valuations for a number of potentially important items,
# ## including land, house, and some livestock
# ## has income, debts, savings - not currently including debts or savings
# 
# source("su-wealth-calculations/CalculateSUwealth_KT.R")
# ## no missingness *of wealth items*
# ## has monthly income
# 
# source("su-wealth-calculations/CalculateSUwealth_LB.R")
# ## currently missing baseline
# 
# source("su-wealth-calculations/CalculateSUwealth_MA.R")
# ## no missingness
# ## has savings and debts, but we do not include these
# 
# source("su-wealth-calculations/CalculateSUwealth_MC.R")
# ## 78 SUs with fully missing information
# ## Beyond that, very exhaustive list (and baseline of 0); Curtis says other cases of NAs can be treated as 0s
# ## A few unpriced items, but small
# 
# source("su-wealth-calculations/CalculateSUwealth_MG.R")
# ## 4 SUs with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_MK.R")
# ##  currently owned land is NOT given a value
# ## Bram says that "almost never is it bought or sold.  There isn’t much of a land market (yet)."
# 
# source("su-wealth-calculations/CalculateSUwealth_MN.R")
# ## 2 SUs with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_MY.R")
# ## currently missing baseline, and resulting values stated by Kramer as NOT reflecting wealth distrib! need to revisit.
# ## 3 SUs with fully missing info, left as NA
# ## has income variables
# 
# source("su-wealth-calculations/CalculateSUwealth_NI.R")
# ## no missingness
# 
# source("su-wealth-calculations/CalculateSUwealth_NP.R")
# ## pulls in file from endow-NP repo, so won't work if don't have access to this!
# ## currently draws from Ivan's own calculations, which are in his endow-NP repo
# ## noise trials are done differently (off of calculated amounts, rather than with item values)
# 
# source("su-wealth-calculations/CalculateSUwealth_PC.R")
# ## 2 SUs with fully missing info, left as NA
# ## has "money" and "debts" (also monthly income); currently include neither
# 
# source("su-wealth-calculations/CalculateSUwealth_PE.R")
# ## Check with Monique/Dan because of supra unit
# ## no missingness
# ## also has average annual amount loaned out, borrowed, and received through remittances
# 
# source("su-wealth-calculations/CalculateSUwealth_PS.R")
# ## no fully missing SUs
# ## reasonable amount of imputation, some educated guesses and other changes
# ## some cases of ranges given -- currently replace with mean of range
# 
# source("su-wealth-calculations/CalculateSUwealth_PT.R")
# ## updates to PossCost hadn't been incorporated in
# ## one SU with almost fully missing info, left as NA, BUT old email suggests they have data and just need to share it!
# ## A few SUs with VERY different wealth values from Anirudh's earlier calculations; unclear why!
# 
# source("su-wealth-calculations/CalculateSUwealth_RA.R")
# ## currently missing baseline!
# ## no missingness
# 
# source("su-wealth-calculations/CalculateSUwealth_SH.R")
# ## 2 SUs with fully missing info, left as NAs; others complete
# 
# source("su-wealth-calculations/CalculateSUwealth_SN.R")
# ## no missingness
# 
# source("su-wealth-calculations/CalculateSUwealth_TI.R")
# ## 4 SUs with fully missing info, left as NAs
# 
# source("su-wealth-calculations/CalculateSUwealth_TM.R")
# ## 3 SUs with fully missing info, left as NAs
# ## not sure on currency
# 
# source("su-wealth-calculations/CalculateSUwealth_TN.R")
# ## 3 SUs with fully missing info, left as NAs
# ## substantially revised/simplified code; results differ slightly; had previously missed some variables but that doesn't fully account
# 
# source("su-wealth-calculations/CalculateSUwealth_TP.R")
# ## no missingness
# ## had previously been using variable calculated by ethnog, with included income and "total city trips"
# 
# source("su-wealth-calculations/CalculateSUwealth_TS.R")
# ## 15 SUs with fully missing info, left as NA
# 
# source("su-wealth-calculations/CalculateSUwealth_TZ.R")
# ## no missingness (save a few unsampled SUs), but one SU with 0s across the board
# ## note no records on house structure itself
# ## says no baseline, fully enumerated
# ## not sure on currency, think Moroccan dirhams!
# 
# source("su-wealth-calculations/CalculateSUwealth_UP.R")
# ## 3 unsampled SUs
# 
# source("su-wealth-calculations/CalculateSUwealth_VT.R")
# ## currently missing baseline
# ## some entries seem like they may be misentries -- 42 umbrellas? 20 toilets?
# 
# source("su-wealth-calculations/CalculateSUwealth_WH.R")
# ## has "money" and "debts" (also monthly income); currently include neither
# 
# source("su-wealth-calculations/CalculateSUwealth_WL.R")
# ## has "money" and "debts" (also monthly income); currently include neither
# 
# source("su-wealth-calculations/CalculateSUwealth_YI.R")
# ## one SU with fully missing data
# 
# source("su-wealth-calculations/CalculateSUwealth_YN.R")
# ## currently missing baseline
# ## one SU missing car and animals

saveRDS(su_wealth_list, paste0(wealth_dir, "su_wealth_list.rds"))
saveRDS(noised_list, paste0(wealth_dir, "noised_list.rds"))
saveRDS(imputation_list, paste0(wealth_dir, "imputation_list.rds"))

rm(trials_for_noise, trials_for_imputation, max_noise)