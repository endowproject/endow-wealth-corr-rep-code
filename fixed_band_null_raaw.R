## fixed_band_null_raaw.R
##
## Robustness check: Can the observed negative cross-site correlation between
## RAAW (Relative Average Alter Wealth) and wealth inequality (Gini) be
## generated mechanically by a wealth-band tie-formation rule?
##
## Bandwidths used in the current supplement section:
##   fixed     -- absolute dollar band (+/- $1000, $2000, $5000)
##   relative  -- % of site mean WPC (+/- 20%, 40%, 60% of mean WPC)
##
## Additional exploratory bandwidths (own-wealth and log-point bands) are left
## commented/inactive below because they are not included in the current text.
##
## The concern: if sharing units simply befriend others within a fixed wealth
## band, poor SUs in high-inequality sites will automatically have poor alters,
## driving RAAW down as a mechanical artefact rather than a social process.
##
## Null model design:
##   - For each ego i, eligible alters = {j : |wpc_i - wpc_j| <= h_i, j != i}
##     where h_i is the ego-specific bandwidth (scalar for fixed/relative types,
##     per-ego vector for own_wealth type)
##   - Each ego's observed out-degree is preserved.
##   - Within the eligible set, alters are chosen uniformly at random.
##   - Observed edge weights are randomly permuted across sampled alters
##     (Option A -- matching the weighted-edge specification in
##     Economic_Connectedness_Revised.R).
##   - If the eligible set is smaller than observed out-degree, all eligible
##     alters are used and the shortfall is filled from nearest-wealth alters
##     outside the band. Constrained egos and forced edges are reported.
##
## Matching the paper's preferred specification
## (see Economic_Connectedness_Revised.R and LASSO_and_Related_Analysis.R):
##   - Layer:           "sum" (directed, outgoing nominations)
##   - Wealth measure:  wealth per capita (su_wealth / su_size)
##   - Alter rank:      weighted_capita_rank = weighted.rank(wpc, su_size) / n
##   - Below median:    weighted_capita_rank <= 0.5
##   - Above median:    weighted_capita_rank >  0.5
##   - RAAW:            avg_frank_low_capita / avg_frank_high_capita
##                      where averages use w = su_size
##   - Site inequality: gini_wealth_per_capita from gini_site_data.csv
##
## Interpretation:
##   If the observed RAAW-Gini correlation lies in the tail of the placebo
##   distribution (small p-value), or if excess RAAW (observed - placebo) is
##   still strongly related to Gini, then fixed-dollar-band befriending alone
##   cannot explain the paper's result.
##
## Outputs saved to EndowDropbox/Figures/paper-1st-draft/fixed_band_null/:
##   simulation_raw.rds                          -- raw B x sites x h data frame
##   simulation_summary.csv                      -- one row per site x bandwidth
##   cross_site_summary.csv                      -- one row per bandwidth (Table for paper)
##   fig2_by_bandwidth/fig2_<h_label>.png/.pdf   -- one file per included bandwidth
##   fig3_excess_raaw_vs_gini.png/.pdf           -- excess RAAW vs Gini (4 panels)

library(cNORM)       # weighted.rank (matches Economic_Connectedness_Revised.R)
library(data.table)  # fast aggregation
library(igraph)      # network objects
library(dplyr)
library(ggplot2)
library(ggrepel)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# ==============================================================================
# Parameters -- adjust here for final run
# ==============================================================================

set.seed(42)

B     <- 500    # simulations per site x bandwidth; increase to 1000 for publication
LAYER <- "sum"  # composite directed network layer

# Fixed absolute bandwidths used in the current supplement section
H_FIXED <- c(1000, 2000, 5000)
# H_FIXED <- c(500, 1000, 2000, 5000)  # includes $500 exploratory panel

# Relative bandwidths as share of mean wealth per capita in each site
H_REL <- c(0.20, 0.40, 0.60)

# Own-wealth bandwidths as share of ego's own wealth per capita.
# Commented out because these panels are not included in the current section.
# H_OWN <- c(0.20, 0.40, 0.60)

# Log-point bandwidths (absolute): |log(wpc_i) - log(wpc_j)| <= h.
# Commented out because these panels are not included in the current section.
# H_LOG_ABS <- c(0.5, 1.0, 2.0)

# Log-point bandwidths (relative): h = mult * mean(log(wpc)) per site.
# Commented out because these panels are not included in the current section.
# H_LOG_REL <- c(0.20, 0.40, 0.60)

# ==============================================================================
# Paths
# ==============================================================================

path    <- file.path(EndowGitHub, "DerivedData")
figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "fixed_band_null")
dir.create(figpath, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Load data
# ==============================================================================

# igraphs_for_each_layer_list.rds: structure [[layer]][[site]] = igraph
# (same source used in Economic_Connectedness_Revised.R)
edges    <- readRDS(file.path(path, "igraphs_for_each_layer_list.rds"))
node_dat <- readRDS(file.path(path, "su_master.rds"))
node_dat <- node_dat[node_dat$network == "sum", ]

gini_dat <- read.csv(file.path(path, "gini_site_data.csv"),
                     stringsAsFactors = FALSE) %>%
  dplyr::select(site, gini_wealth_per_capita)

sites <- intersect(names(edges[[LAYER]]), unique(node_dat$site))

safe_weighted_rank <- function(x, weights) {
  valid <- !is.na(x) & !is.na(weights) & weights > 0
  result <- rep(NA_real_, length(x))
  result[valid] <- weighted.rank(x[valid], weights = weights[valid]) / sum(valid)
  result
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

safe_quantile <- function(x, probs) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  quantile(x, probs, na.rm = FALSE)
}

# ==============================================================================
# Prepare per-site node wealth data
# Matches Economic_Connectedness_Revised.R lines 67-111
# ==============================================================================

node_data_by_site <- list()

for (s in sites) {

  nd <- node_dat[node_dat$site == s,
                 c("su_id", "site", "wealth_per_capita", "su_size")]

  if (nrow(nd) < 5) next

  nd$weighted_capita_rank <-
  safe_weighted_rank(nd$wealth_per_capita, nd$su_size)

  node_data_by_site[[s]] <- nd
}

# ==============================================================================
# Helper: compute RAAW from an edge-list data frame
#
# edge_df  : data.frame with columns (su_id, alter_id, weight)
# nd       : node data with (su_id, weighted_capita_rank, su_size)
#
# Returns named numeric: c(raaw, avg_frank_low, avg_frank_high)
# Returns all-NA if either wealth group is empty
# ==============================================================================

compute_raaw <- function(edge_df, nd) {

  if (nrow(edge_df) == 0)
    return(c(raaw = NA_real_, avg_frank_low = NA_real_, avg_frank_high = NA_real_))

  alter_ranks <- nd[, c("su_id", "weighted_capita_rank")]
  names(alter_ranks) <- c("alter_id", "alter_capita_rank")

  edt <- merge(edge_df, alter_ranks, by = "alter_id", all.x = FALSE)

  if (nrow(edt) == 0)
    return(c(raaw = NA_real_, avg_frank_low = NA_real_, avg_frank_high = NA_real_))

  edt$weight <- as.numeric(edt$weight)
  edt <- as.data.table(edt)

  # Per-ego weighted average alter rank
  ego_avg <- edt[, .(avg_frank_capita = wmean(alter_capita_rank, weight)),
                 by = .(su_id)]

  ego_ranks <- nd[, c("su_id", "weighted_capita_rank", "su_size")]
  ego_avg   <- merge(ego_avg, ego_ranks, by = "su_id", all.x = TRUE)

  low  <- ego_avg[ego_avg$weighted_capita_rank <= 0.5, ]
  high <- ego_avg[ego_avg$weighted_capita_rank >  0.5, ]

  if (nrow(low) == 0 || nrow(high) == 0)
    return(c(raaw = NA_real_, avg_frank_low = NA_real_, avg_frank_high = NA_real_))

  afl <- wmean(low$avg_frank_capita, low$su_size)
  afh <- wmean(high$avg_frank_capita, high$su_size)

  if (is.na(afh) || afh == 0)
    return(c(raaw = NA_real_, avg_frank_low = afl, avg_frank_high = afh))

  c(raaw = afl / afh, avg_frank_low = afl, avg_frank_high = afh)
}

# ==============================================================================
# Helper: pre-compute per-ego info for a (site, h) combination
#
# obs_el   : observed edge list (data.frame: su_id, alter_id, weight)
# nd       : node data (su_id, wealth_per_capita)
# h        : bandwidth
#
# Returns a list, one element per unique ego:
#   $ego_id        : character
#   $k             : observed out-degree
#   $obs_weights   : numeric vector of observed weights (length k)
#   $eligible      : character vector of alters within band (may be empty)
#   $fill_pool     : character vector of nearest alters outside band,
#                    sorted ascending by |wpc_ego - wpc_alter|
#                    (used if eligible set is too small)
# ==============================================================================

precompute_ego_info <- function(obs_el, nd, h, metric = "absolute") {
  # h:      scalar bandwidth (all egos) or named numeric vector (per-ego, for
  #         own_wealth bands). Names must match su_id.
  # metric: "absolute" -- distance = |wpc_i - wpc_j|
  #         "log"      -- distance = |log(wpc_i) - log(wpc_j)|  (log-point band)
  #         fill_pool is sorted by the same metric used for eligibility.

  wpc       <- setNames(nd$wealth_per_capita, nd$su_id)
  log_wpc   <- log(wpc)   # safe: wpc > 0 is enforced upstream
  all_ids   <- nd$su_id
  egos      <- unique(obs_el$su_id)
  scalar_h  <- length(h) == 1

  lapply(egos, function(ego) {

    h_ego    <- if (scalar_h) h else h[[ego]]
    ego_rows <- obs_el[obs_el$su_id == ego, ]
    k        <- nrow(ego_rows)
    obs_wts  <- ego_rows$weight

    others <- all_ids[all_ids != ego]
    dists  <- if (metric == "log") abs(log_wpc[others] - log_wpc[ego])
              else                 abs(wpc[others]     - wpc[ego])

    eligible  <- others[dists <= h_ego]
    outside   <- others[dists >  h_ego]
    fill_pool <- outside[order(dists[dists > h_ego])]

    list(ego_id      = ego,
         k           = k,
         obs_weights = obs_wts,
         eligible    = eligible,
         fill_pool   = fill_pool)
  })
}

# ==============================================================================
# Helper: run ONE simulation draw given pre-computed ego info
#
# ego_info_list : output of precompute_ego_info
#
# Returns list:
#   $edge_df        : data.frame (su_id, alter_id, weight)
#   $n_constrained  : number of egos with eligible set < out-degree
#   $n_forced       : total edges placed outside the band
# ==============================================================================

one_simulation <- function(ego_info_list) {

  n_constrained <- 0L
  n_forced      <- 0L
  pieces        <- vector("list", length(ego_info_list))

  for (i in seq_along(ego_info_list)) {

    ei      <- ego_info_list[[i]]
    k       <- ei$k
    n_elig  <- length(ei$eligible)

    if (k == 0) {
      pieces[[i]] <- data.frame(su_id    = character(0),
                                alter_id = character(0),
                                weight   = numeric(0),
                                stringsAsFactors = FALSE)
      next
    }

    if (n_elig >= k) {
      # Normal case: sample k alters uniformly from eligible set
      sampled       <- sample(ei$eligible, k, replace = FALSE)
      n_forced_here <- 0L

    } else {
      # Constrained: use all eligible, fill shortfall from nearest outside band
      n_constrained <- n_constrained + 1L
      shortfall     <- k - n_elig
      n_fill        <- min(shortfall, length(ei$fill_pool))
      fill          <- if (n_fill > 0) ei$fill_pool[seq_len(n_fill)] else character(0)
      sampled       <- c(ei$eligible, fill)
      n_forced_here <- n_fill
      n_forced      <- n_forced + n_forced_here
    }

    k_actual <- length(sampled)

    # Randomly permute observed weights and assign to sampled alters.
    # If k_actual < k (fill_pool ran out), keep only the k_actual largest
    # weights so total weight is not artificially deflated.
    if (k_actual == k) {
      assigned_weights <- sample(ei$obs_weights)
    } else {
      ow <- ei$obs_weights[!is.na(ei$obs_weights)]
      assigned_weights <- sort(ow, decreasing = TRUE)[seq_len(k_actual)]
    }

    pieces[[i]] <- data.frame(
      su_id    = ei$ego_id,
      alter_id = sampled,
      weight   = assigned_weights,
      stringsAsFactors = FALSE
    )
  }

  list(edge_df       = do.call(rbind, pieces),
       n_constrained = n_constrained,
       n_forced      = n_forced)
}

# ==============================================================================
# Main simulation loop
# ==============================================================================

message("\n=== Fixed-Band Null Model for RAAW ===")
message("Layer: ", LAYER, " | B = ", B, " simulations per (site, bandwidth)")
message("Sites: ", paste(names(node_data_by_site), collapse = ", "), "\n")

all_rows    <- list()  # will become the flat results data frame
obs_records <- list()  # observed site-level stats

gini_vec <- setNames(gini_dat$gini_wealth_per_capita, gini_dat$site)

for (s in names(node_data_by_site)) {

  nd <- node_data_by_site[[s]]
  g  <- edges[[LAYER]][[s]]
  if (is.null(g)) next

  # Observed edge list, restricted to nodes with wealth data
  el_mat <- as_edgelist(g, names = TRUE)
  if (nrow(el_mat) == 0) next

  obs_el <- data.frame(
    su_id    = el_mat[, 1],
    alter_id = el_mat[, 2],
    weight   = E(g)$weight,
    stringsAsFactors = FALSE
  )
  obs_el <- obs_el[obs_el$su_id %in% nd$su_id & obs_el$alter_id %in% nd$su_id, ]
  if (nrow(obs_el) == 0) next

  obs_raaw_vec <- compute_raaw(obs_el, nd)
  obs_raaw     <- obs_raaw_vec["raaw"]

  wpc_vec <- setNames(nd$wealth_per_capita, nd$su_id)
  obs_el$wpc_ego   <- wpc_vec[obs_el$su_id]
  obs_el$wpc_alter <- wpc_vec[obs_el$alter_id]
  obs_mean_abd <- mean(abs(obs_el$wpc_ego - obs_el$wpc_alter), na.rm = TRUE)
  obs_med_abd  <- median(abs(obs_el$wpc_ego - obs_el$wpc_alter), na.rm = TRUE)

  mean_wpc <- mean(nd$wealth_per_capita)
  h_grid   <- unique(c(H_FIXED, H_REL * mean_wpc))

  # Label helper
  make_label <- function(h) {
    if (h %in% H_FIXED) return(paste0("h_", h, "_fixed"))
    mult <- round(h / mean_wpc, 2)
    paste0("h_", round(mult * 100), "pct_mean_wpc")
  }

  obs_records[[s]] <- list(
    site         = s,
    obs_raaw     = obs_raaw,
    gini         = gini_vec[s],
    mean_wpc     = mean_wpc,
    obs_mean_abd = obs_mean_abd,
    obs_med_abd  = obs_med_abd,
    n_egos       = length(unique(obs_el$su_id)),
    n_edges      = nrow(obs_el)
  )

  message("Site ", s,
          ": obs RAAW = ", round(obs_raaw, 4),
          " | Gini = ",    round(gini_vec[s], 3),
          " | egos = ",    obs_records[[s]]$n_egos)

  for (h in h_grid) {

    h_lbl <- make_label(h)

    # Pre-compute eligible sets (done once per site x h)
    ego_info <- precompute_ego_info(
      obs_el[, c("su_id", "alter_id", "weight")], nd, h
    )

    # Diagnostic: expected share of constrained egos
    egos_k     <- sapply(ego_info, function(x) x$k)
    egos_elig  <- sapply(ego_info, function(x) length(x$eligible))
    frac_const_exp <- mean(egos_elig < egos_k)

    # B simulation draws
    pl_raaws     <- numeric(B)
    pl_mean_abd  <- numeric(B)
    pl_med_abd   <- numeric(B)
    pl_n_const   <- integer(B)
    pl_n_forced  <- integer(B)

    for (b in seq_len(B)) {
      res <- one_simulation(ego_info)
      r   <- compute_raaw(res$edge_df, nd)

      pl_raaws[b]    <- r["raaw"]
      pl_n_const[b]  <- res$n_constrained
      pl_n_forced[b] <- res$n_forced

      if (nrow(res$edge_df) > 0) {
        res$edge_df$wpc_ego   <- wpc_vec[res$edge_df$su_id]
        res$edge_df$wpc_alter <- wpc_vec[res$edge_df$alter_id]
        pl_mean_abd[b] <- mean(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
                               na.rm = TRUE)
        pl_med_abd[b]  <- median(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
                                 na.rm = TRUE)
      } else {
        pl_mean_abd[b] <- NA_real_
        pl_med_abd[b]  <- NA_real_
      }
    }

    n_egos <- length(ego_info)

    all_rows[[paste0(s, "__", h_lbl)]] <- data.frame(
      site              = s,
      h                 = h,
      h_label           = h_lbl,
      h_type            = if (h %in% H_FIXED) "fixed" else "relative",
      b                 = seq_len(B),
      obs_raaw          = obs_raaw,
      placebo_raaw      = pl_raaws,
      excess_raaw       = obs_raaw - pl_raaws,
      gini              = gini_vec[s],
      obs_mean_abd      = obs_mean_abd,
      obs_med_abd       = obs_med_abd,
      placebo_mean_abd  = pl_mean_abd,
      placebo_med_abd   = pl_med_abd,
      n_constrained     = pl_n_const,
      n_forced          = pl_n_forced,
      n_egos            = n_egos,
      share_constrained = pl_n_const / n_egos,
      stringsAsFactors  = FALSE
    )

    #message("  h = ", sprintf("%-30s", h_lbl),
    #        " | mean placebo RAAW = ", round(mean(pl_raaws, na.rm = TRUE), 4),
    #        " | frac constrained = ", round(frac_const_exp, 3))
  }

  # Own-wealth and log-band sensitivity loops are not part of the current
  # supplement section. They are kept inactive here for easy restoration.
  # if (FALSE) {

  # # Own-wealth bandwidth loop
  # for (mult in H_OWN) {

  #   h_lbl <- paste0("h_", round(mult * 100), "pct_own_wpc")

  #   # Per-ego bandwidth: h_i = mult * wpc_i
  #   h_ego <- setNames(mult * wpc_vec[nd$su_id], nd$su_id)

  #   ego_info <- precompute_ego_info(
  #     obs_el[, c("su_id", "alter_id", "weight")], nd, h_ego
  #   )

  #   egos_k     <- sapply(ego_info, function(x) x$k)
  #   egos_elig  <- sapply(ego_info, function(x) length(x$eligible))
  #   frac_const_exp <- mean(egos_elig < egos_k)

  #   pl_raaws     <- numeric(B)
  #   pl_mean_abd  <- numeric(B)
  #   pl_med_abd   <- numeric(B)
  #   pl_n_const   <- integer(B)
  #   pl_n_forced  <- integer(B)

  #   for (b in seq_len(B)) {
  #     res <- one_simulation(ego_info)
  #     r   <- compute_raaw(res$edge_df, nd)

  #     pl_raaws[b]    <- r["raaw"]
  #     pl_n_const[b]  <- res$n_constrained
  #     pl_n_forced[b] <- res$n_forced

  #     if (nrow(res$edge_df) > 0) {
  #       res$edge_df$wpc_ego   <- wpc_vec[res$edge_df$su_id]
  #       res$edge_df$wpc_alter <- wpc_vec[res$edge_df$alter_id]
  #       pl_mean_abd[b] <- mean(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
  #                              na.rm = TRUE)
  #       pl_med_abd[b]  <- median(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
  #                                na.rm = TRUE)
  #     } else {
  #       pl_mean_abd[b] <- NA_real_
  #       pl_med_abd[b]  <- NA_real_
  #     }
  #   }

  #   n_egos <- length(ego_info)

  #   all_rows[[paste0(s, "__", h_lbl)]] <- data.frame(
  #     site              = s,
  #     h                 = mult,   # store multiplier as numeric h
  #     h_label           = h_lbl,
  #     h_type            = "own_wealth",
  #     b                 = seq_len(B),
  #     obs_raaw          = obs_raaw,
  #     placebo_raaw      = pl_raaws,
  #     excess_raaw       = obs_raaw - pl_raaws,
  #     gini              = gini_vec[s],
  #     obs_mean_abd      = obs_mean_abd,
  #     obs_med_abd       = obs_med_abd,
  #     placebo_mean_abd  = pl_mean_abd,
  #     placebo_med_abd   = pl_med_abd,
  #     n_constrained     = pl_n_const,
  #     n_forced          = pl_n_forced,
  #     n_egos            = n_egos,
  #     share_constrained = pl_n_const / n_egos,
  #     stringsAsFactors  = FALSE
  #   )

  #   message("  h = ", sprintf("%-30s", h_lbl),
  #           " | mean placebo RAAW = ", round(mean(pl_raaws, na.rm = TRUE), 4),
  #           " | frac constrained = ", round(frac_const_exp, 3))
  # }

  # # Log-band loop (absolute log-points and % of mean log WPC)
  # log_wpc_vec  <- log(wpc_vec)
  # mean_log_wpc <- mean(log_wpc_vec[nd$su_id], na.rm = TRUE)
  # h_log_grid   <- unique(c(H_LOG_ABS, H_LOG_REL * mean_log_wpc))

  # make_log_label <- function(h_log) {
  #   if (h_log %in% H_LOG_ABS) {
  #     paste0("h_", gsub("\\.", "", formatC(h_log, format = "f", digits = 1)), "log")
  #   } else {
  #     mult <- round(h_log / mean_log_wpc, 2)
  #     paste0("h_", round(mult * 100), "pct_mean_logwpc")
  #   }
  # }

  # for (h_log in h_log_grid) {

  #   h_lbl     <- make_log_label(h_log)
  #   h_type_log <- if (h_log %in% H_LOG_ABS) "log_fixed" else "log_relative"

  #   ego_info <- precompute_ego_info(
  #     obs_el[, c("su_id", "alter_id", "weight")], nd, h_log, metric = "log"
  #   )

  #   egos_k         <- sapply(ego_info, function(x) x$k)
  #   egos_elig      <- sapply(ego_info, function(x) length(x$eligible))
  #   frac_const_exp <- mean(egos_elig < egos_k)

  #   pl_raaws    <- numeric(B)
  #   pl_mean_abd <- numeric(B)
  #   pl_med_abd  <- numeric(B)
  #   pl_n_const  <- integer(B)
  #   pl_n_forced <- integer(B)

  #   for (b in seq_len(B)) {
  #     res <- one_simulation(ego_info)
  #     r   <- compute_raaw(res$edge_df, nd)

  #     pl_raaws[b]    <- r["raaw"]
  #     pl_n_const[b]  <- res$n_constrained
  #     pl_n_forced[b] <- res$n_forced

  #     if (nrow(res$edge_df) > 0) {
  #       res$edge_df$wpc_ego   <- wpc_vec[res$edge_df$su_id]
  #       res$edge_df$wpc_alter <- wpc_vec[res$edge_df$alter_id]
  #       pl_mean_abd[b] <- mean(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
  #                              na.rm = TRUE)
  #       pl_med_abd[b]  <- median(abs(res$edge_df$wpc_ego - res$edge_df$wpc_alter),
  #                                na.rm = TRUE)
  #     } else {
  #       pl_mean_abd[b] <- NA_real_
  #       pl_med_abd[b]  <- NA_real_
  #     }
  #   }

  #   n_egos <- length(ego_info)

  #   all_rows[[paste0(s, "__", h_lbl)]] <- data.frame(
  #     site              = s,
  #     h                 = h_log,
  #     h_label           = h_lbl,
  #     h_type            = h_type_log,
  #     b                 = seq_len(B),
  #     obs_raaw          = obs_raaw,
  #     placebo_raaw      = pl_raaws,
  #     excess_raaw       = obs_raaw - pl_raaws,
  #     gini              = gini_vec[s],
  #     obs_mean_abd      = obs_mean_abd,
  #     obs_med_abd       = obs_med_abd,
  #     placebo_mean_abd  = pl_mean_abd,
  #     placebo_med_abd   = pl_med_abd,
  #     n_constrained     = pl_n_const,
  #     n_forced          = pl_n_forced,
  #     n_egos            = n_egos,
  #     share_constrained = pl_n_const / n_egos,
  #     stringsAsFactors  = FALSE
  #   )

  #   message("  h = ", sprintf("%-30s", h_lbl),
  #           " | mean placebo RAAW = ", round(mean(pl_raaws, na.rm = TRUE), 4),
  #           " | frac constrained = ", round(frac_const_exp, 3))
  # }

  # }
}

# ==============================================================================
# Combine and save raw results
# ==============================================================================

sim_df <- do.call(rbind, all_rows)
rownames(sim_df) <- NULL
saveRDS(sim_df, file.path(figpath, "simulation_raw.rds"))
message("\nRaw simulation output saved.")

# ==============================================================================
# Site x bandwidth summary table
# ==============================================================================

sim_summary <- sim_df %>%
  group_by(site, h, h_label, h_type) %>%
  dplyr::summarise(
    gini                  = first(gini),
    obs_raaw              = first(obs_raaw),
    obs_mean_abd          = first(obs_mean_abd),
    obs_med_abd           = first(obs_med_abd),
    placebo_mean_raaw     = mean(placebo_raaw,    na.rm = TRUE),
    placebo_median_raaw   = median(placebo_raaw,  na.rm = TRUE),
    placebo_q025_raaw     = safe_quantile(placebo_raaw, 0.025),
    placebo_q975_raaw     = safe_quantile(placebo_raaw, 0.975),
    mean_excess_raaw      = mean(excess_raaw,      na.rm = TRUE),
    placebo_mean_abd      = mean(placebo_mean_abd, na.rm = TRUE),
    placebo_med_abd       = mean(placebo_med_abd,  na.rm = TRUE),
    mean_share_constrained = mean(share_constrained, na.rm = TRUE),
    mean_n_forced         = mean(n_forced,         na.rm = TRUE),
    .groups = "drop"
  )

write.csv(sim_summary, file.path(figpath, "simulation_summary.csv"),
          row.names = FALSE)

# ==============================================================================
# Cross-site summary: for each bandwidth, distribution of
# cor(placebo RAAW, Gini) across B simulation draws
# ==============================================================================

# Observed cross-site correlation
obs_df <- do.call(rbind, lapply(obs_records, function(x) {
  data.frame(site = x$site, obs_raaw = x$obs_raaw, gini = x$gini,
             stringsAsFactors = FALSE)
}))
obs_df <- obs_df[!is.na(obs_df$obs_raaw) & !is.na(obs_df$gini), ]
obs_corr <- cor(obs_df$obs_raaw, obs_df$gini, use = "complete.obs")

# Per-draw cross-site placebo correlations
sim_corr_draws <- sim_df %>%
  filter(!is.na(placebo_raaw), !is.na(gini)) %>%
  group_by(h_label, h_type, b) %>%
  dplyr::summarise(
    placebo_corr = {
      ok <- !is.na(placebo_raaw) & !is.na(gini)
      if (sum(ok) < 3) NA_real_ else cor(placebo_raaw[ok], gini[ok])
    },
    n_sites = sum(!is.na(placebo_raaw) & !is.na(gini)),
    .groups = "drop"
  )

# Excess RAAW correlation with Gini (site-level mean excess)
excess_gini_corrs <- sim_df %>%
  filter(!is.na(excess_raaw), !is.na(gini)) %>%
  group_by(h_label) %>%
  dplyr::summarise(
    corr_excess_gini = {
      site_excess <- tapply(excess_raaw, site, mean, na.rm = TRUE)
      site_gini   <- gini_vec[names(site_excess)]
      ok          <- !is.na(site_excess) & !is.na(site_gini)
      if (sum(ok) < 3) NA_real_ else cor(site_excess[ok], site_gini[ok])
    },
    .groups = "drop"
  )

cross_site_summary <- sim_corr_draws %>%
  filter(!is.na(placebo_corr)) %>%
  group_by(h_label, h_type) %>%
  dplyr::summarise(
    obs_corr              = obs_corr,
    placebo_mean_corr     = mean(placebo_corr),
    placebo_q025_corr     = quantile(placebo_corr, 0.025),
    placebo_q50_corr      = quantile(placebo_corr, 0.500),
    placebo_q975_corr     = quantile(placebo_corr, 0.975),
    # p-value: share of placebo correlations <= observed (i.e., at least as negative)
    p_value               = mean(placebo_corr <= obs_corr),
    n_included_sites      = round(mean(n_sites)),
    avg_share_constrained = {
      sim_df %>%
        filter(h_label == first(h_label)) %>%
        pull(share_constrained) %>%
        mean(na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  left_join(excess_gini_corrs, by = "h_label")

write.csv(cross_site_summary, file.path(figpath, "cross_site_summary.csv"),
          row.names = FALSE)

# ==============================================================================
# Figures
# ==============================================================================

format_pval <- function(p) {
  vapply(p, function(pv) {
    if (is.na(pv)) "p = NA"
    else if (pv < 0.001) "p < 0.001"
    else paste0("p = ", round(pv, 3))
  }, character(1))
}

# --- Figure 2: Placebo correlation distributions per bandwidth ----------------

# Panel labels used in the current supplement section:
# fixed $, % of mean WPC.
fig2_panel_labels <- c(
  h_1000_fixed          = "$1,000 Fixed",
  h_2000_fixed          = "$2,000 Fixed",
  h_5000_fixed          = "$5,000 Fixed",
  h_20pct_mean_wpc      = "20% Mean WPC",
  h_40pct_mean_wpc      = "40% Mean WPC",
  h_60pct_mean_wpc      = "60% Mean WPC"
)

# Additional exploratory Fig 2 panels are not included in the current section.
# h_500_fixed           = "$500 Fixed"
# h_20pct_own_wpc       = "20% Own WPC"
# h_40pct_own_wpc       = "40% Own WPC"
# h_60pct_own_wpc       = "60% Own WPC"
# h_05log               = "0.5 Log Pts"
# h_10log               = "1 Log Pt"
# h_20log               = "2 Log Pts"
# h_20pct_mean_logwpc   = "20% Mean Log WPC"
# h_40pct_mean_logwpc   = "40% Mean Log WPC"
# h_60pct_mean_logwpc   = "60% Mean Log WPC"

# Observed correlation (same for all panels)
fig2_obs_corr <- cross_site_summary %>%
  slice(1) %>%
  pull(obs_corr)

fig2_draws <- sim_corr_draws %>%
  filter(h_label %in% names(fig2_panel_labels),
         !is.na(placebo_corr)) %>%
  mutate(panel_label = factor(fig2_panel_labels[h_label],
                              levels = fig2_panel_labels))

make_fig2 <- function(data, ncol = 5) {
  ggplot(data, aes(x = placebo_corr)) +
    geom_histogram(bins = 40, colour = "white", alpha = 0.75) +
    geom_vline(xintercept = fig2_obs_corr, colour = "red",
               linewidth = 0.9, linetype = "solid") +
    facet_wrap(~ panel_label, ncol = ncol, scales = "free_y") +
    labs(
      x = "Cross-Site Correlation (Placebo RAAW vs Gini)",
      y = "Count"
    ) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text       = element_text(size = 10))
}

# Combined all-panel Figure 2 is not included in the current section.
# fig2_all  <- make_fig2(fig2_draws, ncol = 3)
# n_panels  <- length(unique(na.omit(as.character(fig2_draws$panel_label))))
# fig2_ncol <- 3
# fig2_nrow <- ceiling(n_panels / fig2_ncol)
# ggsave(file.path(figpath, "fig2_placebo_correlation_distributions.png"),
#        fig2_all, width = 12, height = 4 * fig2_nrow, dpi = 300)
# ggsave(file.path(figpath, "fig2_placebo_correlation_distributions.pdf"),
#        fig2_all, width = 12, height = 4 * fig2_nrow)

# Individual panel files
indiv_dir <- file.path(figpath, "fig2_by_bandwidth")
dir.create(indiv_dir, showWarnings = FALSE)

for (lbl in names(fig2_panel_labels)) {
  panel_dat <- fig2_draws %>% filter(h_label == lbl)
  if (nrow(panel_dat) == 0) next
  p <- make_fig2(panel_dat, ncol = 1)
  safe_name <- gsub("[^A-Za-z0-9_]", "", fig2_panel_labels[lbl])
  ggsave(file.path(indiv_dir, paste0("fig2_", lbl, ".png")),
         p, width = 5, height = 4, dpi = 300)
  ggsave(file.path(indiv_dir, paste0("fig2_", lbl, ".pdf")),
         p, width = 5, height = 4)
}

# --- Figure 3: Excess RAAW vs Gini -------------------------------------------
# Representative panels: $1,000, $2,000 fixed; 20% mean; 40% mean

fig3_panel_labels <- c(
  h_1000_fixed     = "$1,000 Fixed",
  h_2000_fixed     = "$2,000 Fixed",
  h_20pct_mean_wpc = "20% Mean WPC",
  h_40pct_mean_wpc = "40% Mean WPC"
)

fig3_dat <- sim_summary %>%
  filter(h_label %in% names(fig3_panel_labels), !is.na(gini)) %>%
  mutate(panel_label = factor(fig3_panel_labels[h_label],
                              levels = fig3_panel_labels))

fig3 <- ggplot(fig3_dat, aes(x = gini, y = mean_excess_raaw)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  #geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = "blue",
    linewidth = 0.7
  ) +
  #geom_text_repel(aes(label = site), size = 3, max.overlaps = 20) +
  geom_label(
    aes(label = site),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  facet_wrap(~panel_label, ncol = 2) +
  labs(
    x = "Wealth per capita Gini",
    y = "Mean excess RAAW"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10)
  )

ggsave(file.path(figpath, "fig3_excess_raaw_vs_gini.png"),
       fig3, width = 10, height = 10, dpi = 300)
ggsave(file.path(figpath, "fig3_excess_raaw_vs_gini.pdf"),
       fig3, width = 10, height = 10)

# ==============================================================================
# Print cross-site summary table and interpretation
# ==============================================================================

message("\n=== Cross-Site Summary Table ===\n")

print_cols <- c("h_label", "obs_corr", "placebo_mean_corr",
                "placebo_q025_corr", "placebo_q975_corr",
                "p_value", "corr_excess_gini",
                "n_included_sites", "avg_share_constrained")
print_cols <- print_cols[print_cols %in% names(cross_site_summary)]

print(
  format(as.data.frame(cross_site_summary[, print_cols]), digits = 3),
  row.names = FALSE
)

# message("\n=== Interpretation ===")
# message("Observed RAAW-Gini correlation: r = ", round(obs_corr, 4))
# message("")
# message("A small p-value means the observed correlation is more negative")
# message("than expected under a rule where SUs connect only within a fixed")
# message("absolute wealth-per-capita band (the fixed-band null).")
# message("")
# message("If corr_excess_gini is also negative and significant, then")
# message("the paper's result is not explained by fixed-dollar befriending:")
# message("even after accounting for what the null predicts, sites with more")
# message("inequality show systematically lower RAAW than the null expects.")
# message("")
message("Figures saved to: ", figpath)
