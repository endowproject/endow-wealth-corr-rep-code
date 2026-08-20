# ==============================================================================
# Noise Sensitivity: Relative Friend Rank vs Gini (Wealth Per Capita)
# ==============================================================================
#
# For each of the 50 "noised up" wealth datasets, this script:
#   1. Recomputes wealth-per-capita Gini for every site
#   2. Recomputes AAWP (avg alter wealth of the poor) and AAWR (avg alter
#      wealth of the rich) using the noised wealth ranks — per-capita version,
#      exactly following Economic_Connectedness_Revised.R — for the "sum" layer
#   3. Derives rel_frank_capita = AAWP / AAWR for every site
#   4. Correlates rel_frank_capita with gini_wealth_per_capita across sites
#
# Produces: histogram of the 50 correlations, with a dashed line for the
# "true" correlation (computed from the unnoised DataForLASSO.csv).
# ==============================================================================

library(cNORM)       # weighted.rank
library(data.table)
library(dplyr)
library(DescTools)   # Gini
library(igraph)
library(ggplot2)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

path        <- file.path(EndowGitHub, "DerivedData")
wealth_dir  <- file.path(EndowGitHub, "su-wealth-output")
fig_path    <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site")

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

noised_list <- readRDS(file.path(wealth_dir, "noised_list.rds"))
edges       <- readRDS(file.path(path, "igraphs_for_each_layer_list.rds"))
sum_edges   <- edges[["sum"]]

# True correlation from the unnoised DataForLASSO.csv
dat <- read.csv(file.path(path, "DataForLASSO.csv"))
dat$rel_frank_capita <- dat$avg_frank_low_capita.sum / dat$avg_frank_high_capita.sum
true_cor <- cor(dat$rel_frank_capita, dat$gini_wealth_per_capita, use = "complete.obs")
cat("Observed correlation (unnoised):", round(true_cor, 4), "\n\n")

# Sites present in both noised_list and the sum network layer
common_sites <- intersect(names(noised_list), names(sum_edges))
cat("Sites used:", length(common_sites), "\n", paste(common_sites, collapse = ", "), "\n\n")

# ------------------------------------------------------------------------------
# Helper: compute site-level stats for one trial
# ------------------------------------------------------------------------------

compute_site_stats <- function(trial_idx) {

  results <- lapply(common_sites, function(s) {

    df <- noised_list[[s]][[trial_idx]]

    # Wealth per capita
    df$wealth_per_capita <- df$cash_value / df$size_su

    # Drop unobserved / degenerate rows
    df <- df[is.finite(df$wealth_per_capita) & !is.na(df$wealth_per_capita), ]
    if (nrow(df) < 3) return(NULL)

    # Gini
    gini <- DescTools::Gini(df$wealth_per_capita)
    if (!is.finite(gini)) return(NULL)

    # Weighted capita rank (same method as Economic_Connectedness_Revised.R)
    df$weighted_capita_rank <-
      cNORM::weighted.rank(df$wealth_per_capita, weights = df$size_su) / nrow(df)

    # Build weighted edge list for the sum layer at this site
    eds <- sum_edges[[s]]
    w_edgelist <- data.table(cbind(igraph::as_edgelist(eds), igraph::edge_attr(eds, "weight")))
    names(w_edgelist) <- c("su_id", "alter_id", "weight")
    w_edgelist[, weight := as.numeric(weight)]

    # Merge ego wealth/rank onto edges
    ego_cols <- df[, c("su_id", "size_su", "weighted_capita_rank")]
    edge_merge <- merge(w_edgelist, ego_cols, by = "su_id")

    # Merge alter capita rank onto edges
    alter_cols <- df[, c("su_id", "weighted_capita_rank")]
    names(alter_cols) <- c("alter_id", "alter_capita_rank")
    edge_merge <- merge(edge_merge, alter_cols, by = "alter_id")

    if (nrow(edge_merge) == 0) return(NULL)

    edge_merge <- data.table(edge_merge)

    # Average alter capita rank per SU (= avg_frank_capita)
    iec_data <- edge_merge[,
      .(weighted_capita_rank = first(weighted_capita_rank),
        su_size              = first(size_su),
        avg_frank_capita     = wmean(alter_capita_rank, weight)),
      by = .(su_id)]

    poor <- iec_data[weighted_capita_rank <= 0.5]
    rich <- iec_data[weighted_capita_rank >  0.5]

    if (nrow(poor) == 0 || nrow(rich) == 0) return(NULL)

    aawp <- wmean(poor$avg_frank_capita, poor$su_size)
    aawr <- wmean(rich$avg_frank_capita, rich$su_size)
    if (!is.finite(aawp) || !is.finite(aawr) || aawr == 0) return(NULL)

    data.frame(site = s, gini = gini, rel_frank = aawp / aawr)
  })

  do.call(rbind, Filter(Negate(is.null), results))
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# ------------------------------------------------------------------------------
# Run all 50 trials
# ------------------------------------------------------------------------------

n_trials     <- length(noised_list[[1]])   # 50
correlations <- numeric(n_trials)

cat("Running", n_trials, "noise trials...\n")

for (i in seq_len(n_trials)) {
  stats_df <- compute_site_stats(i)
  if (is.null(stats_df) || nrow(stats_df) < 3) {
    correlations[i] <- NA
  } else {
    correlations[i] <- cor(stats_df$rel_frank, stats_df$gini, use = "complete.obs")
  }
  if (i %% 10 == 0) cat("  Trial", i, "done. Correlation:", round(correlations[i], 4), "\n")
}

cat("\nCorrelation summary across", n_trials, "trials:\n")
print(summary(correlations))

# ------------------------------------------------------------------------------
# Histogram
# ------------------------------------------------------------------------------

hist_df <- data.frame(correlation = correlations[!is.na(correlations)])

p <- ggplot(hist_df, aes(x = correlation)) +
  geom_histogram(bins = 15) +
  geom_vline(xintercept = true_cor,
             linetype = "dashed", colour = "firebrick", linewidth = 0.9) +
  #annotate("text",
  #         x     = true_cor + 0.005,
  #         y     = Inf,
  #         label = paste0("Observed Correlation = ", round(true_cor, 2)),
  #         hjust = 0, vjust = 1.5,
  #         colour = "firebrick", size = 3.5) +
  #scale_x_continuous(breaks = seq(-0.52, -0.40, by = 0.04)) +
  #scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  labs(
    x = "Cross-Site Correlation",
    y = "Count"
  ) +
  theme_classic()

ggsave(file.path(fig_path, "noise_sensitivity_frank_gini.pdf"),
       plot = p, width = 7, height = 5)

cat("\nFigures saved to", fig_path, "\n")
