# ==============================================================================
# ENDOW Wealth Correlations — Full Replication Kit
# ==============================================================================
#
# This script runs the complete analysis pipeline to generate all figures and
# tables referenced in:
#
#   "Social Network Structure, Wealth, and Wealth Inequality Across Cultures"
#      — The ENDOW Team
#
# WHAT THIS SCRIPT DOES
# ---------------------
# It runs all R scripts in order, from raw data through to publication-ready
# figures. There are four stages:
#
#   0. Database Simulation: simulates an ENDOW database with four sites
#   A. Data Processing: cleans raw survey data and builds network objects
#   B. Dataset Generation: constructs analysis-ready site- and SU-level datasets
#   C/D/E. Analysis & Figures: produces all figures and tables in the paper
#
# HOW TO USE IT
# -------------
# 1. Complete the R Setup described in README.md (set up your .Rprofile with
#    EndowGitHub, EndowDatabase, and EndowDropbox path variables).
# 2. Source this file: source("00_run_all.R")
#
# TIP: If you only need to regenerate figures (not re-process raw data), you
# can skip Part A and Part B by setting RUN_DATA_PROCESSING = FALSE and
# RUN_DATASET_GENERATION = FALSE below. Parts B through E read from saved .rds/.csv
# files in DerivedData/, so they do not require Part A to have just been run.
#
# OUTPUT LOCATIONS
# ----------------
# - Figures:  EndowDropbox/Figures/
# - Tables:   EndowDropbox/Tables/
# - Intermediate data files: EndowGitHub/DerivedData/
#
# ==============================================================================


# ==============================================================================
# CONFIGURATION: set these to FALSE to skip stages you don't need to re-run
# ==============================================================================

SIMULATE_DATA          <- TRUE  # Part 0: create a faux ENDOW database with four sites
COMPILE_WEALTH         <- TRUE  # Part A1: compile wealth valuations
RUN_DATA_PROCESSING    <- TRUE  # Part A2: process raw data → network objects
RUN_DATASET_GENERATION <- TRUE  # Part B: build analysis-ready datasets
RUN_MODULARITY         <- TRUE  # Part C: generate wealth modularity (takes ~30 min with faux data)
RUN_MULTILEVEL         <- TRUE  # Part D: run multilevel regression (takes ~7 hours with faux data!)
RUN_FIGURES            <- TRUE  # Part E: generate all figures and tables
SITES_TO_DROP <- c("XX", "ZZ") # no sites should be dropped from analyses; these are dummy codes


# ==============================================================================
# PREREQUISITES CHECK
# ==============================================================================

# These three variables must be defined in your .Rprofile (see README.md).
# EndowGitHub  — path to this repository (ENDOW-wealth-corr)
# EndowDatabase — path to the endow-database repository;
#                 should here be the "endow-database-sim" folder in this repo
# EndowDropbox — path to your local folder for storing outputs

if (!exists("EndowGitHub") || !is.character(EndowGitHub)) {
  stop(
    "EndowGitHub is not defined. ",
    "Follow the R Setup section of README.md to configure your .Rprofile."
  )
}

if (!exists("EndowDatabase") || !is.character(EndowDatabase)) {
  stop(
    "EndowDatabase is not defined. ",
    "Follow the R Setup section of README.md to configure your .Rprofile."
  )
}

if (!exists("EndowDropbox") || !is.character(EndowDropbox)) {
  stop(
    "EndowDropbox is not defined. ",
    "Follow the R Setup section of README.md to configure your .Rprofile."
  )
}

# Derived path used throughout — all intermediate data files live here
EndowDerivedData <- file.path(EndowGitHub, "DerivedData")

message("=== ENDOW Replication Kit ===")
message("EndowGitHub:    ", EndowGitHub)
message("EndowDatabase:  ", EndowDatabase)
message("EndowDropbox:   ", EndowDropbox)
message("DerivedData at: ", EndowDerivedData)
message("")


# ==============================================================================
# HELPER: run a script with timing and error reporting
# ==============================================================================

run_script <- function(script_name, description = NULL) {
  path <- file.path(EndowGitHub, script_name)
  if (!is.null(description)) {
    message("  -> ", description)
  }
  message("     Running: ", script_name, " ...")
  t_start <- proc.time()["elapsed"]
  tryCatch(
    {
      suppressPackageStartupMessages(
        source(path, local = FALSE)
      )
      elapsed <- round(proc.time()["elapsed"] - t_start)
      message("     Done (", elapsed, "s)")
    },
    error = function(e) {
      message("     ERROR in ", script_name, ":")
      message("     ", conditionMessage(e))
      message("     Continuing with next script ...")
    }
  )
  invisible(NULL)
}

# ==============================================================================
# PART 0: SIMULATE ENDOW DATABASE
# ==============================================================================
# This script produces faux database files that we have for each ENDOW site.
# No effort has been made here to make these 'realistic' -- they simply allow
# the code to run!
#
# Key outputs:
#   endow-database-sim/    — faux database folder. Set "EndowDatabase" to this path
#     networks/            - edge lists with recording support network nominations
#     partnerships/        - marital partnerships between residents
#     people/              - time-invariant info on people (birth year, parents)
#     people_observations/ - time-varying info on people
#     possession_costs/    - monetary value of enumerated property and assets
#     residents/           - associates residents to sharing units
#     su_distances/        - distances between sharing units in meters (not used here)
#     su_observations/     - info on sharing units
# ==============================================================================

if (SIMULATE_DATA) {

  message("")
  message("============================================================")
  message("PART 0: SIMULATE ENDOW DATABASE")
  message("============================================================")

  run_script(
    "simulate_endow_database.R",
    "Generate faux data for four ENDOW sites."
  )
} else {
  message("Skipping simulate_endow_database.R (SIMULATE_DATA = FALSE)")
}


# ==============================================================================
# PART A: DATA PROCESSING
# ==============================================================================
# These scripts take raw data from the endow-database repo as input and produce
# cleaned, standardized network and metadata objects saved to DerivedData/.
#
# Key outputs:
#   su_nets.rdata        — per-prompt SU-level network adjacency matrices
#   su_meta.rdata        — SU-level metadata aligned to network order
#   su_alters.rdata      — SU-level alter information (incl. external alters)
#   su_nets_expanded.rdata — combined network layers (loan, behav, info, etc.)
#   igraphs_for_each_layer_list.rds — igraph objects for each network layer
#   site_net_descriptives.csv — site-level network summary statistics
#
# NOTE: process_data.R sources standardize_data.R and process-PE.R internally.
# If you have trouble with the FamAgg package, install it via Bioconductor:
#   if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
#   BiocManager::install("FamAgg")
# ==============================================================================

if (COMPILE_WEALTH) {

  message("")
  message("============================================================")
  message("PART A1: COMPILE WEALTH VALUATIONS")
  message("============================================================")

  run_script(
    "su_wealth_compile.R",
    "Calculate wealth valuations for each site; impute and run noise trails"
  )
} else {
  message("Skipping su_wealth_compile.R (COMPILE_WEALTH = FALSE)")
}

if (RUN_DATA_PROCESSING) {
  message("")
  message("============================================================")
  message("PART A2: DATA PROCESSING")
  message("============================================================")

  run_script(
    "process_data.R",
    "Clean and standardize raw survey data; build SU-level network objects"
  )

  run_script(
    "analyse_data_and_create_networks.R",
    "Combine network layers; compute site-level network descriptives"
  )
} else {
  message("Skipping Part A (RUN_DATA_PROCESSING = FALSE)")
}


# ==============================================================================
# PART B: DATASET GENERATION
# ==============================================================================
# These scripts take the network objects from Part A and build the analysis-
# ready datasets that are used by all the figure-generation scripts in Part D.
# Note: The final script call to merge_and_clean_data_for_lasso.R is run
# separately after Part C, as it merges in the modularity data generated in Part C.
# Note also: two datasets are not included here, because they require the exact
# coordinates for each site, which are identifying. One is based on queries to
# Google Earth Engine (done using gee_query.ipynb and then cleaned with
# clean_gee_output.R to generate the gee.csv called in here). The other is based
# on queries to downloaded or online geospatial datasets, gathered using functions
# in this public repo: https://github.com/barguzin/endow and a further private
# repo (https://github.com/barguzin/conawan23), resulting in the file named
# all_vars_2026-01-14.csv.
#
# Key outputs:
#   su-wealth-output/*_SU_wealth.csv      — per-site SU wealth tables
#   su-wealth-output/noised_list.rds      — 50 wealth datasets with ±20% item noise
#   su-wealth-output/imputation_list.rds  — 50 MICE-imputed datasets (sites with missingness)
#   su_master.rds                         — node-level dataset: centrality measures + wealth
#   su_master.csv                         — CSV version of above (written to Dropbox)
#   su_externals_df.csv                   — SU-level external alter counts
#   gini_site_data.csv                    — site-level Gini coefficients and wealth stats
#   IEC_Revised.csv                       — individual-level economic connectedness
#   EC_Revised.csv                        — site-level economic connectedness (long format)
#   EC_Revised_Reshape.csv                — site-level EC (wide format)
#   site_country_descriptives.csv         — country-level variables (GDP, V-Dem, etc.)
#   DataForLASSO.csv                      — merged site-level dataset for cross-site analysis
#                                           (done with "B2" following Part C modularity)
# ==============================================================================

if (RUN_DATASET_GENERATION) {

  message("")
  message("============================================================")
  message("PART B: DATASET GENERATION")
  message("============================================================")

  output_dirs <- c(
    "Figures/DescriptiveStats/DensityPlots",
    "Figures/Kinship/sum/primary",
    "Figures/Kinship/sum/secondary",
    "Figures/Modularity",
    "Figures/Networks",
    "Figures/WealthBoxPlots/PerAdult",
    "Figures/WealthBoxPlots/PerCapita",
    "Figures/WealthBoxPlots/Raw",
    "Figures/WealthDistributions/BySite",
    "Figures/WealthDistributions/ByVariable/Wealth_Absolute",
    "Figures/WealthDistributions/ByVariable/Wealth_Relative",
    "Figures/WealthDistributions/ByVariable/WealthPerAdult_Absolute",
    "Figures/WealthDistributions/ByVariable/WealthPerCapita_Absolute",
    "Figures/paper-1st-draft/multilevel",
    "Figures/paper/wealth_homophily",
    "Tables/DescriptiveStats"
  )
  for (d in output_dirs) {
    dir.create(file.path(EndowDropbox, d), showWarnings = FALSE, recursive = TRUE)
  }
  message("Created main directory structure")

  run_script(
    "create_su_master.R",
    "Build master node-level dataset: centrality + wealth for each SU"
  )

  run_script(
    "wealth_size_adjustment.R",
    "Compute size-adjusted wealth residuals and merge into su_master"
  )

  run_script(
    "degree_size_adjustment.R",
    "Compute size-adjusted degree residuals and merge into su_master"
  )

  run_script(
    "country_vars.R",
    "Compile country-level variables (GDP, population, etc.)"
  )

  run_script(
    "Economic_Connectedness_Revised.R",
    "Compute Economic Connectedness (EC) and individual-level IEC"
  )

  run_script(
    "site_level.R",
    "Compute site-level Gini coefficients and wealth summary statistics"
  )

  run_script(
    "kinship.R",
    "Compute kinship variables (proportion kin etc)."
  )

} else {
  message("Skipping Part B (RUN_DATASET_GENERATION = FALSE)")
}


# ==============================================================================
# PART C: MODULARITY ANALYSIS
# ==============================================================================
# Single script that generates maximum wealth modualarity scores for each
# community. Additionally, calculates the null distribution for wealth modularity,
# which is used to compute the normed wealth modularity score. Normed wealth
# modularity is then regressed against Gini to create Figure 5d (generated in
# Part D below).
# This script takes a long time to run (several hours).
#
# Key outputs:
#  modularity_data_set.rds — dataset with modularity scores and null distributions
# ==============================================================================

if (RUN_MODULARITY) {

  message("")
  message("============================================================")
  message("PART C: MODULARITY ANALYSIS AND FIGURE GENERATION")
  message("============================================================")

  run_script("Modularity.R")

  message("Modularity figure written to:         ", file.path(EndowDropbox, "Figures", "Modularity"))

} else {
  message("Skipping Part C (RUN_MODULARITY = FALSE)")
}


if (RUN_DATA_PROCESSING) {
  message("")
  message("============================================================")
  message("PART B2: DATASET GENERATION (following modularity)")
  message("============================================================")

  run_script(
    "merge_and_clean_data_for_lasso.R",
    "Merge all site-level datasets into single cross-site analysis file"
  )
} else {
  message("Skipping merge_and_clean_data_for_lasso.R (RUN_DATASET_GENERATION = FALSE)")
}

if (RUN_MULTILEVEL) {

  message("")
  message("============================================================")
  message("PART D: MULTILEVEL MODEL AND FIGURE GENERATION")
  message("============================================================")
  message("")
  message("This will take a few days!")

  run_script("multilevel_model.R")

  message("Multilevel figures written to:         ", file.path(EndowDropbox, "Figures", "paper-1st-draft", "multilevel"))

} else {
  message("Skipping Part D (RUN_MULTILEVEL = FALSE)")
}


# ==============================================================================
# PART E: ANALYSIS AND FIGURE GENERATION
# ==============================================================================
# Each script below uses the datasets from Part B to produce one or more
# figures or tables. The paper figure references are noted for each script.
# Figures are written to subfolders of EndowDropbox/Figures/ and tables to
# EndowDropbox/Tables/.
# ==============================================================================

if (RUN_FIGURES) {

  message("")
  message("============================================================")
  message("PART D: ANALYSIS AND FIGURE GENERATION")
  message("============================================================")

  # Create all paper-1st-draft subdirectories before running figure scripts
  paper1_dirs <- c(
    "paper-1st-draft",
    "paper-1st-draft/within-site-multipanel",
    "paper-1st-draft/within-site-multipanel-frank",
    "paper-1st-draft/within-site-iec",
    "paper-1st-draft/within-site-other",
    "paper-1st-draft/sitewise-size",
    "paper-1st-draft/size-controls",
    "paper-1st-draft/cross-site",
    "paper-1st-draft/fixed_band_null",
    "paper-1st-draft/kinship",
    "paper-1st-draft/kinship/rank-rank",
    "paper-1st-draft/kinship/aaw_kin",
    "paper-1st-draft/lorenz",
    "paper-1st-draft/figures_wealth_mismeasurement",
    "paper-1st-draft/wealth-boxplots",
    "paper-1st-draft/FRank-Quartiles",
    "paper-1st-draft/RC-Quartiles",
    "paper-1st-draft/EC-Quartiles",
    "paper-1st-draft/ninety-ten",
    "paper-1st-draft/LASSO-And-Related",
    "paper-1st-draft/wealth modularity"
  )
  for (d in paper1_dirs) {
    dir.create(file.path(EndowDropbox, "Figures", d), showWarnings = FALSE, recursive = TRUE)
  }
  message("Created paper-1st-draft directory structure in Figures/")

  # ----------------------------------------------------------------------------
  # D1. Descriptive Statistics
  # Generates summary statistics and community overview figures.
  #
  # Paper figures:
  #   Figures/paper-1st-draft/sites_map.png           - Fig. 1 in the paper
  #   Figures/paper-1st-draft/site_descriptives.pdf   — Fig. 2 in the paper
  #   Figures/paper-1st-draft/site_su_descrip.pdf     — Appendix; community size, SU size, mean degree,
  #                                                     and wealth distributions across sites
  #   Figures/paper-1st-draft/subsistence.png         - Appendix figure showing subsistence types of sites
  #   Tables/DescriptiveStats/su_summary.tex          — per-site SU summary statistics
  #   Tables/DescriptiveStats/summary_table.csv       — pooled SU summary stats
  # ----------------------------------------------------------------------------

  run_script(
    "descriptivestats.R",
    "Community descriptives → site_descriptives.pdf, site_su_descrip.pdf"
  )

  run_script(
    "net_descriptives.R",
    "Network descriptives → Networks/*, DescriptiveStats/*"
  )

  # ----------------------------------------------------------------------------
  # D2. Wealth Distributions
  # Generates box/violin plots of absolute wealth across sites.
  #
  # Paper figures (Appendix):
  #   wealth-boxplots/AbsoluteWealthViolinPlot.pdf
  #   wealth-boxplots/AbsoluteWealthPerCapitaBoxPlot.pdf
  #   wealth-boxplots/AbsoluteWealthPerAdultBoxPlot.pdf
  # Also produces per-site histogram PDFs in WealthDistributions/
  # ----------------------------------------------------------------------------

  run_script(
    "WealthDistribution.R",
    "Wealth distribution plots → wealth-boxplots/*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D3. Gini Measure Correlations
  # Produces a heatmap of correlations between different Gini coefficient
  # specifications (unweighted/weighted, per-capita/per-adult/per-able).
  #
  # Paper figures (Appendix):
  #   gini_measure_correlations_heatmap.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "Correlate_Various_Gini_Measures.R",
    "Gini measure correlation heatmap → gini_measure_correlations_heatmap.pdf"
  )

  # ----------------------------------------------------------------------------
  # D4. Lorenz Curves
  # Plots Lorenz curves for wealth distribution (raw, per-capita, per-adult)
  # for each site individually, and as multi-panel PDFs grouped by 9 sites.
  #
  # Paper figures (Appendix):
  #   lorenz/00_Lorenz_Curves_Part_1.pdf through Part_5.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "lorenz_curves.R",
    "Lorenz curves for each site → lorenz/00_Lorenz_Curves_Part_*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D5. EC vs Wealth (Measurement Validation)
  # Cross-site scatter plots of economic connectedness and Gini against
  # median wealth — used as a check that wealth mismeasurement is not driving
  # the EC–Gini relationship.
  #
  # Paper figures (Appendix — figures_wealth_mismeasurement/):
  #   NICE_Median_Wealth_Gini_Wealth.pdf
  #   NICE_Median_WPC_Gini_WPC.pdf
  #   NICE_Median_WPC_RAAW.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "ec_vs_wealth.R",
    "Measurement validation plots → figures_wealth_mismeasurement/*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D6. Within-Site Sitewise Regression Plots (Degree vs Wealth)
  # For each site, regresses rank wealth on rank in/out-degree (both absolute
  # and per-capita), producing forest-plot-style figures of the site-level
  # correlation coefficients. Also produces kinship variants.
  #
  # Paper figures (main text):
  #   within-site-multipanel/Sitewise_sum_rank_out_degree_weighted_rank_wealth.png
  #   within-site-multipanel/Sitewise_sum_rank_in_degree_weighted_rank_wealth.png
  #   within-site-multipanel/Sitewise_sum_rank_out_degree_percap_weighted_rank_wpc.png
  #   within-site-multipanel/Sitewise_sum_rank_in_degree_percap_weighted_rank_wpc.png
  # Paper figures (Appendix):
  #   within-site-multipanel/Sitewise_collapse_sum_* — proportional network
  #   within-site-multipanel/Sitewise_main_supp_* — raw composite network
  #   within-site-other/Sitewise_sum_rank_out/in_degree_peradult_* — per adult
  #   sitewise-size/Sitewise_sum_rank_wealth/in/out_degree_su_size — size
  #   kinship/rank-rank/Sitewise_primary/non_primary_connectedkin_* — kinship
  # ----------------------------------------------------------------------------

  run_script(
    "sitewise_regplots_clean.R",
    "Within-site degree vs wealth plots → within-site-multipanel/*.png"
  )

  # ----------------------------------------------------------------------------
  # D6b. Within-Site Sitewise Regression Plots — Size Controls
  # Regresses wealth on degree after partialling out sharing-unit size.
  # Produces size-residualised forest plots.
  #
  # Paper figures (Appendix):
  #   size-controls/Sitewise_sum_residuals_* — size-residualised plots
  # ----------------------------------------------------------------------------

  run_script(
    "sitewise_regplots_control_size.R",
    "Size-controlled within-site plots → size-controls/*.png"
  )

  # ----------------------------------------------------------------------------
  # D6c. Within-Site Fixed Effects Coefficients
  # Manual within-estimator: demeans log wealth and log degree by site × size,
  # standardises within site, then runs site-interaction models to extract
  # site-specific coefficients. Plots as forest plots.
  #
  # Paper figures (Appendix):
  #   size-controls/Fixed_Effects_Site_Coefficients_Outdegree.png
  #   size-controls/Fixed_Effects_Site_Coefficients_Indegree.png
  # ----------------------------------------------------------------------------

  run_script(
    "fixed_effects.R",
    "Fixed effects (demeaning) plots → size-controls/Fixed_Effects_*.png"
  )

  # ----------------------------------------------------------------------------
  # D7. Within-Site Multilevel Model Tables
  # Fits random-slope-only multilevel models corresponding to Figure
  # fig:within-site-multipanel panels A-D and writes paste-ready LaTeX tables.
  #
  # Tables:
  #   Tables/within_site_multilevel_tables.tex
  # ----------------------------------------------------------------------------

  run_script(
    "within_site_multilevel_models.R",
    "Within-site multilevel model tables → Tables/within_site_multilevel_tables.tex"
  )

  # ----------------------------------------------------------------------------
  # D8. Within-Site Sitewise Plots — FRank (Average Alter Wealth) & Individual EC (IEC)
  # Forest plots of within-site correlation between AAW or IEC and wealth, plus kinship.
  #
  # Paper figures (main text):
  #   within-site-multipanel-frank/Sitewise_sum_rank_avg_frank_absolute_rank_wealth.png
  #   within-site-multipanel-frank/Sitewise_rev_sum_rank_avg_frank_absolute_rank_wealth.png
  #   within-site-multipanel-frank/Sitewise_sum_rank_avg_frank_capita_rank_wpc.png
  #   within-site-multipanel-frank/Sitewise_rev_sum_rank_avg_frank_capita_rank_wpc.png
  # Paper figures (Appendix):
  #   within-site-iec/Sitewise_*_rank_iec_*.png — various IEC vs wealth specs
  # ----------------------------------------------------------------------------

  run_script(
    "sitewise_regplots_iec.R",
    "Within-site IEC vs wealth plots → within-site-iec/*.png"
  )

  # ----------------------------------------------------------------------------
  # D9. Relative FRank vs Wealth Gini — Multi-panel Scatter Plots
  # Cross-site scatter plots showing the correlation between the
  # Friend Rank Ratio (average alter wealth of the poor relative to the rich)
  # and the wealth Gini. Includes panels for different wealth specifications
  # (per-capita, absolute, per-adult, size-adjusted). Also produces equivalent
  # plots for in-degree Gini, out-degree Gini, EC, and Relative Connectedness.
  # And tables showing the site-level network-derived social capital variables.
  #
  # Appendix figures:
  #   cross-site/GiniWealthFriendRankScatterAll.pdf
  #   cross-site/GiniWealthInDegreeScatterAll.pdf
  #   cross-site/GiniWealthOutDegreeScatterAll.pdf
  #   cross-site/GiniWealthECScatterAll.pdf
  #   cross-site/GiniWealthRCScatterAll.pdf
  #
  # Appendix tables (tables/DescriptiveStats):
  #   deg_gini_site_vars_table.tex
  #   ec_site_vars_table.tex
  #   raaw_site_vars_table.tex
  #   rc_site_vars_table.tex
  # ----------------------------------------------------------------------------

  run_script(
    "Relative_Frank_Vs_Wealth_GINI_Multipanel.R",
    "Cross-site scatter plots → cross-site/GiniWealth*ScatterAll.pdf"
  )

  # ----------------------------------------------------------------------------
  # D9b. Fixed-Band Null: Relative Average Alter Wealth vs Gini
  # Tests whether a wealth-band tie-formation null can mechanically generate
  # the observed cross-site relationship between RAAW and wealth inequality.
  #
  # Paper figures and tables:
  #   paper-1st-draft/fixed_band_null/simulation_summary.csv
  #   paper-1st-draft/fixed_band_null/cross_site_summary.csv
  #   paper-1st-draft/fixed_band_null/fig2_by_bandwidth/fig2_*.png/.pdf
  #   paper-1st-draft/fixed_band_null/fig3_excess_raaw_vs_gini.png/.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "fixed_band_null_raaw.R",
    "Fixed-band RAAW null model → paper-1st-draft/fixed_band_null/"
  )

  # ----------------------------------------------------------------------------
  # D9c. Noise Sensitivity: Relative FRank Ratio vs Gini
  # For each of the 50 "noised up" wealth datasets (item valuations randomly
  # perturbed ±20%), recomputes per-capita Gini and the Relative Friend Rank
  # Ratio (AAWP/AAWR) for every site, then correlates them across sites.
  # Produces a histogram of the 50 resulting correlation coefficients with a
  # dashed line marking the true (unnoised) correlation.
  #
  # Paper figures:
  #   paper-1st-draft/cross-site/wealth_sensitivity_frank_gini.pdf
  #   paper-1st-draft/cross-site/wealth_sensitivity_frank_gini.png
  # ----------------------------------------------------------------------------

  run_script(
    "wealth_sensitivity_frank_gini.R",
    "Wealth sensitivity histogram → paper-1st-draft/cross-site/wealth_sensitivity_frank_gini.pdf"
  )

  # ----------------------------------------------------------------------------
  # D9d. Wealth Sensitivity: Within-Site Forest Plots
  # Replicates the four main within-site avg-alter-wealth forest plots but
  # replaces regression-based CIs with an empirical interval from the 50 noised
  # wealth datasets. For each site the point is the median within-site
  # correlation across 50 trials; the bar spans the [2nd, 49th] sorted values
  # (dropping one extreme on each side ≈ 96% empirical interval).
  #
  # Four specs: rank(avg_frank_capita/absolute) vs rank(wpc/wealth),
  # for both sum and rev_sum network layers.
  #
  # Paper figures:
  #   paper-1st-draft/within-site-multipanel-frank/noise_Sitewise_sum_avg_frank_capita_wpc.png
  #   paper-1st-draft/within-site-multipanel-frank/noise_Sitewise_sum_avg_frank_absolute_wealth.png
  #   paper-1st-draft/within-site-multipanel-frank/noise_Sitewise_rev_sum_avg_frank_capita_wpc.png
  #   paper-1st-draft/within-site-multipanel-frank/noise_Sitewise_rev_sum_avg_frank_absolute_wealth.png
  # ----------------------------------------------------------------------------

  run_script(
    "wealth_sensitivity_forest_plots.R",
    "Wealth sensitivity forest plots → paper-1st-draft/within-site-multipanel-frank/noise_Sitewise_*.png"
  )

  # ----------------------------------------------------------------------------
  # D10. Kinship Analysis
  # Examines whether the within-site wealth–degree relationship differs for
  # kin vs non-kin network ties, and whether kinship mediates the relationship
  # between wealth and EC.
  #
  # Paper figures (Appendix):
  #   kinship/rank-rank/Sitewise_primary_connectedkin_*.png
  #   kinship/rank-rank/Sitewise_non_primary_connectedkin_*.png
  #   kinship/aaw_kin/AAW_sitewise_dotplot_avg_frank_all_absolute_primary.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "kinship_analysis.R",
    "Kinship analysis → kinship/rank-rank/*.png, kinship/aaw_kin/*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D11. LASSO and Related Cross-Site Analysis
  # The main cross-site analysis script. Produces:
  #   (a) Key scatter plots: Gini vs In-Degree Gini, Out-Degree Gini,
  #       Relative FRank (AAW), and Modularity
  #   (b) Wage labor and property rights vs Gini plots
  #   (c) EC vs Size scatter plot
  #   (d) Relative Connectedness vs Gini
  #   (e) Correlation plots (CorrelationsWithgini_wealth_per_capita.pdf,
  #       CorrelationsWithrel_frank_capita.pdf)
  #   (f) LASSO variable selection figure
  #   (g) Hierarchical model alpha plot (within-site wealth–degree effect sizes)
  #   (h) Tables: Bivariate and stepwise regression results
  #   (i) Tables: Descriptive tables of site-level variables
  #
  #
  # Paper figures (main text):
  #   cross-site/IDeg_Gini_vs_Gini.pdf
  #   cross-site/ODeg_Gini_vs_Gini.pdf
  #   cross-site/Rel_Frank_vs_Gini.pdf
  #   cross-site/Modularity_vs_Gini.pdf
  #   CorrelationsWithgini_wealth_per_capita.pdf
  # Paper figures (Appendix):
  #   LASSO-And-Related/LASSO_gini_wealth_per_capita_with_rel_frank_capita*.pdf
  #   wealth modularity/mod_facet_all.pdf
  #   WageLabor_vs_Gini.pdf
  #   Property_Rights_vs_Gini.pdf
  #   CorrelationsWithrel_frank_capita.pdf
  #   Relative_Connectedness_vs_Gini.pdf
  # Tables (Appendix):
  #   tables/CorrelationsWithgini_wealth_per_capita.tex
  #   tables/CorrelationsWithrel_frank_capita.tex
  #   tables/Stepwise_Regression_from_Lasso.tex
  #   tables/other_site_vars_table.tex
  #   tables/wealth_site_vars_table.tex
  #   tables/wealth_mod_site_vars_table.tex
  # ----------------------------------------------------------------------------

  run_script(
    "LASSO_and_Related_Analysis.R",
    "LASSO, cross-site scatter plots, correlations → cross-site/*.pdf + more"
  )

  # ----------------------------------------------------------------------------
  # D12. Economic Connectedness Quartile Correlations
  # Produces heatmaps showing how EC between each pair of wealth quartiles
  # correlates with site-level wealth Gini. Also produces Friend Rank (AAW)
  # quartile correlation plots.
  #
  # Paper figures:
  #   FRank-Quartiles/Frank_Quartiles_vs_GINI_WPC_sum.pdf
  #   FRank-Quartiles/Frank_Quartiles_vs_GINI_WPC_rev_sum.pdf
  #   RC-Quartiles/RC_Quartile_Corrs_Heatmap_sum_revsum_with_GINIPC.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "Economic_Connectedness_Quartiles_Correlations.R",
    "EC/RC quartile heatmaps → FRank-Quartiles/*.pdf, RC-Quartiles/*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D13. Wealth Ratios vs Gini
  # Cross-site scatter plots of 90/10 and 80/20 percentile wealth ratios
  # against the Gini coefficient and against EC / Friend Rank measures.
  # These serve as robustness checks that results are not sensitive to the
  # choice of inequality measure.
  #
  # Paper figures (Appendix):
  #   ninety-ten/WealthPC_90_10_vs_Gini.pdf
  #   ninety-ten/WealthPC_80_20_vs_Gini.pdf
  #   ninety-ten/WealthPC_90_10_vs_Rel_Sum_Frank.pdf
  #   ninety-ten/WealthPC_80_20_vs_Rel_Sum_Frank.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "wealth_ratios_vs_gini.R",
    "Wealth ratio robustness checks → ninety-ten/*.pdf"
  )

  # ----------------------------------------------------------------------------
  # D14. Wealth Homophily
  # Computes neighbour wealth statistics (mean, SD, range) for each sharing
  # unit and regresses site-level summaries against the wealth Gini.
  # Produces scatter plots with and without outlier site NI.
  #
  # Paper figures (Appendix):
  #   Normed SD of SU Wealth vs Average Normed SD of Neighbor Wealth.png
  #   Normed SD of SU Wealth (minus top 5 perc) vs Average Normed SD of Neighbor Wealth (minus top 5 perc).png
  #   RAAW_vs_AssortWPC.pdf
  #   RAAW_vs_AssortWPCRank.pdf
  # ----------------------------------------------------------------------------

  run_script(
    "wealth_homophily.R",
    "Wealth homophily → Normed SD of SU Wealth vs Average Normed SD of Neighbor Wealth.png"
  )

  run_script(
    "assortativity.R",
    "Compare RAAW to assortativity → cross-site/RAAW_vs_Assort*.pdf"
  )

  message("")
  message("============================================================")
  message("PART E COMPLETE")
  message("Paper figures written to:         ", file.path(EndowDropbox, "Figures", "paper-1st-draft"))
  message("All figures written to:           ", file.path(EndowDropbox, "Figures"))
  message("All tables written to:            ", file.path(EndowDropbox, "Tables"))
  message("============================================================")

} else {
  message("Skipping Part E (RUN_FIGURES = FALSE)")
}

message("")
message("=== Replication kit complete ===")
