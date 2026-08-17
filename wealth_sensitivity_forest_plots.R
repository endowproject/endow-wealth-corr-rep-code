# ==============================================================================
# Noise Sensitivity Forest Plots: Within-Site Avg Alter Wealth vs Own Wealth
# ==============================================================================
#
# Produces four forest plots matching the main within-site paper figures
# (from sitewise_regplots_iec.R), but replacing the regression-based 95% CI
# with an empirical interval derived from the 50 noised wealth datasets.
#
# For each site × each of the 50 noise trials:
#   - Recomputes wealth ranks from the noised cash_value
#   - Recomputes avg_frank (weighted mean alter wealth rank) from the network
#   - Computes the within-site Pearson correlation of rank(avg_frank) vs rank(y)
#
# The point shown is the MEDIAN correlation across 50 trials.
# The error bar spans the [2nd, 49th] sorted values (dropping one extreme on
# each side), giving an empirical ~96% interval across the noise distribution.
#
# Four specifications (matching the four paper figures):
#   1. rank(avg_frank_capita) vs rank(wealth_per_capita) — sum network
#   2. rank(avg_frank_absolute) vs rank(wealth)          — sum network
#   3. rank(avg_frank_capita) vs rank(wealth_per_capita) — rev_sum network
#   4. rank(avg_frank_absolute) vs rank(wealth)          — rev_sum network
#
# Further produces two plots, showing the distribution of Ginis across the 50
# trials (one, using the noise trials, one using the imputed wealth data)
#
# Output: paper-1st-draft/within-site-multipanel-frank/
#     noise_Sitewise_sum_avg_frank_capita_wpc.png
#     noise_Sitewise_sum_avg_frank_absolute_wealth.png
#     noise_Sitewise_rev_sum_avg_frank_capita_wpc.png
#     noise_Sitewise_rev_sum_avg_frank_absolute_wealth.png
#   paper-1st-draft/ginis_with_noise.png
#   paper-1st-draft/ginis_with_imputations.png
#
# ==============================================================================

library(data.table)
library(dplyr)
library(igraph)
library(ggplot2)

showtext::showtext_auto(FALSE)
showtext::showtext_opts(dpi = 96) ## return to default

# ------------------------------------------------------------------------------
# Paths and data
# ------------------------------------------------------------------------------

path       <- file.path(EndowGitHub, "DerivedData")
wealth_dir <- file.path(EndowGitHub, "su-wealth-output")
fig_path   <- file.path(EndowDropbox, "Figures", "paper-1st-draft",
                        "within-site-multipanel-frank")

master_df <- readRDS(file.path(path, "su_master.rds"))
su_wealth_list <- readRDS(file.path(wealth_dir, "su_wealth_list.rds"))
noised_list <- readRDS(file.path(wealth_dir, "noised_list.rds"))
imputation_list <- readRDS(file.path(wealth_dir, "imputation_list.rds"))
edges       <- readRDS(file.path(path, "igraphs_for_each_layer_list.rds"))
sum_edges   <- edges[["sum"]]

format_pval <- function(p, digits = 4) {
      stopifnot(is.numeric(p), p >= 0, p <= 1)
      if (p < 0.0001) {
        "p < 0.0001"
      } else {
        paste0("p = ", formatC(p, digits = digits, format = "f"))
      }
    }

# Some sites are still joined here: TN instead of TE and AZ; MC instead of MC and AP

# Deal with double sites: TE and AZ, MC and AP (note: AP is excluded because of coverage)
#su_wealth_list$TE <- subset(su_wealth_list$TN, su_id %in% master_df$su_id[master_df$site == "TE"])
#su_wealth_list$AZ <- subset(su_wealth_list$TN, su_id %in% master_df$su_id[master_df$site == "AZ"])
#su_wealth_list$MC <- subset(su_wealth_list$MC, su_id %in% master_df$su_id[master_df$site == "MC"])
#if ("AP" %in% names(su_wealth_list)) {
#  su_wealth_list$AP <- subset(su_wealth_list$AP, su_id %in% master_df$su_id[master_df$site == "AP"])
#}
#su_wealth_list$TN <- NULL

#for (list_name in c("noised_list", "imputation_list")) {
#  lst <- get(list_name)
#  lst$TE <- lapply(lst$TN, function(df) subset(df, su_id %in% master_df$su_id[master_df$site == "TE"]))
#  lst$AZ <- lapply(lst$TN, function(df) subset(df, su_id %in% master_df$su_id[master_df$site == "AZ"]))
#  lst$MC <- lapply(lst$MC, function(df) subset(df, su_id %in% master_df$su_id[master_df$site == "MC"]))
#  if ("AP" %in% names(lst)) {
#    lst$AP <- lapply(lst$TN, function(df) subset(df, su_id %in% master_df$su_id[master_df$site == "AP"]))
#  }
#  lst$TN <- NULL
#  assign(list_name, lst)
#}

common_sites <- intersect(names(noised_list), names(sum_edges))

# Build rev_sum by reversing directed edges (swap from/to, keep weights)
rev_edges <- lapply(sum_edges, reverse_edges)

layers <- list(sum = sum_edges, rev_sum = rev_edges)

specs <- list(
  list(layer = "sum",     frank = "capita",   y = "capita",
       file  = "noise_Sitewise_sum_avg_frank_capita_wpc.png",
       ylab  = "Corr: rank(avg frank per capita) vs rank(wealth per capita)"),
  list(layer = "sum",     frank = "absolute", y = "absolute",
       file  = "noise_Sitewise_sum_avg_frank_absolute_wealth.png",
       ylab  = "Corr: rank(avg frank absolute) vs rank(wealth)"),
  list(layer = "rev_sum", frank = "capita",   y = "capita",
       file  = "noise_Sitewise_rev_sum_avg_frank_capita_wpc.png",
       ylab  = "Corr: rank(avg frank per capita) vs rank(wealth per capita) [rev-sum]"),
  list(layer = "rev_sum", frank = "absolute", y = "absolute",
       file  = "noise_Sitewise_rev_sum_avg_frank_absolute_wealth.png",
       ylab  = "Corr: rank(avg frank absolute) vs rank(wealth) [rev-sum]")
)

# ------------------------------------------------------------------------------
# Compute within-site correlation for one site × one wealth dataset
# Returns NA if the site has too few observations or degenerate data
# ------------------------------------------------------------------------------

site_correlation <- function(s, wealth_df, ed_list, frank_type, y_type) {

  df <- wealth_df[[s]]
  df <- df[is.finite(df$cash_value) & !is.na(df$cash_value) & df$size_su > 0, ]
  if (nrow(df) < 3) return(NA_real_)

  df$wealth_per_capita <- df$cash_value / df$size_su
  df$rank_wpc          <- percent_rank(df$wealth_per_capita)
  df$rank_wealth       <- percent_rank(df$cash_value)

  g <- ed_list[[s]]
  if (is.null(g) || ecount(g) == 0) return(NA_real_)

  el  <- as_edgelist(g)
  edg <- data.frame(su_id    = el[, 1],
                    alter_id = el[, 2],
                    weight   = as.numeric(E(g)$weight),
                    stringsAsFactors = FALSE)

  edg <- merge(edg, df[, c("su_id", "rank_wpc", "rank_wealth")], by = "su_id")

  alter_ranks <- df[, c("su_id", "rank_wpc", "rank_wealth")]
  names(alter_ranks) <- c("alter_id", "alter_rank_wpc", "alter_rank_wealth")
  edg <- merge(edg, alter_ranks, by = "alter_id")

  if (nrow(edg) == 0) return(NA_real_)

  # avg_frank per ego: weighted mean of alter's wealth rank
  alter_col <- if (frank_type == "capita") "alter_rank_wpc" else "alter_rank_wealth"
  y_col     <- if (y_type    == "capita") "rank_wpc"       else "rank_wealth"

  iec <- tapply(seq_len(nrow(edg)), edg$su_id, function(idx) {
    wmean(edg[[alter_col]][idx], edg$weight[idx])
  })

  iec_df   <- data.frame(su_id = names(iec), avg_frank = as.numeric(iec),
                          stringsAsFactors = FALSE)
  combined <- merge(df[, c("su_id", "rank_wpc", "rank_wealth")], iec_df, by = "su_id")
  if (nrow(combined) < 3) return(NA_real_)

  combined$rank_avg_frank <- percent_rank(combined$avg_frank)

  suppressWarnings(
    cor(combined$rank_avg_frank, combined[[y_col]], use = "complete.obs")
  )
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# ------------------------------------------------------------------------------
# Run all 50 trials for all sites and all specs
# coef_arrays[[spec_idx]] is a matrix: sites × trials
# ------------------------------------------------------------------------------

n_trials <- length(noised_list[[1]])
cat("Computing", n_trials, "noise trials ×", length(specs), "specs ×",
    length(common_sites), "sites...\n")

coef_arrays <- lapply(specs, function(spec) {
  matrix(NA_real_, nrow = length(common_sites), ncol = n_trials,
         dimnames = list(common_sites, NULL))
})

for (i in seq_len(n_trials)) {
  trial_data <- lapply(common_sites, function(s) noised_list[[s]][[i]])
  names(trial_data) <- common_sites

  for (j in seq_along(specs)) {
    spec     <- specs[[j]]
    ed_list  <- layers[[spec$layer]]
    coef_arrays[[j]][, i] <- sapply(common_sites, function(s) {
      site_correlation(s, trial_data, ed_list, spec$frank, spec$y)
    })
  }
  if (i %% 10 == 0) cat("  Trial", i, "done\n")
}

# ------------------------------------------------------------------------------
# Summarise: median, lo (2nd smallest), hi (2nd largest) per site × spec
# ------------------------------------------------------------------------------

summarise_site <- function(x) {
  # x is a vector of 50 coefficients for one site
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 4) return(c(median = NA, lo = NA, hi = NA))
  c(median = median(x),
    lo      = x[2],        # drop one extreme on each side
    hi      = x[n - 1])
}

# ------------------------------------------------------------------------------
# Make and save one forest plot per spec
# ------------------------------------------------------------------------------

make_noise_forest_plot <- function(spec_idx) {

  spec      <- specs[[spec_idx]]
  coef_mat  <- coef_arrays[[spec_idx]]

  summary_df <- as.data.frame(t(apply(coef_mat, 1, summarise_site)))
  summary_df$site <- rownames(summary_df)
  summary_df <- summary_df[!is.na(summary_df$median), ]

  # Order by median coefficient
  summary_df <- summary_df[order(summary_df$median), ]
  summary_df$site_f <- factor(summary_df$site, levels = summary_df$site)

  # Binomial sign test on median coefficients
  n_pos  <- sum(summary_df$median > 0)
  n_tot  <- nrow(summary_df)
  binom_p <- binom.test(n_pos, n_tot, p = 0.5, alternative = "two.sided")$p.value

  cat(spec$file, "— n_pos:", n_pos, "/", n_tot,
      "— binom p:", signif(binom_p, 2), "\n")

  ggplot(summary_df, aes(x = site_f, y = median)) +
    scale_colour_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
    geom_errorbar(aes(ymin = pmax(lo, -1), ymax = pmin(hi, 1)), width = 0.2) +
    geom_point(aes(colour = median > 0), size = 2) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    ylim(-1, 1) +
    labs(x        = "Site",
         y        = "Correlation Coefficient (median ± noise interval)",
         #subtitle = format_pval(binom_p)
         ) +
    theme_classic() +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}

for (j in seq_along(specs)) {
  p <- make_noise_forest_plot(j)
  print(p)
  ggsave(file.path(fig_path, specs[[j]]$file),
         plot = p, width = 10, height = 5)
  cat("  Saved:", specs[[j]]$file, "\n")
}

cat("\nAll figures saved to", fig_path, "\n")



# ------------------------------------------------------------------------------
# Make Gini distribution plots
# ------------------------------------------------------------------------------

# Compute Gini point estimates
point_estimates <- sapply(names(su_wealth_list), function(site) {
  DescTools::Gini(su_wealth_list[[site]]$cash_value, na.rm = TRUE)
})
point_df <- data.frame(site = names(point_estimates), gini = point_estimates)

site_order <- point_df %>%
  arrange(gini) %>%
  pull(site)

gini_noise <- lapply(names(noised_list), function(site) {
  ginis <- sapply(noised_list[[site]],
    function(df) DescTools::Gini(df$cash_value, na.rm = TRUE))
  data.frame(site = site, gini = ginis)
})

gini_noise_df <- bind_rows(gini_noise)

gini_noise_df$site <- factor(gini_noise_df$site, levels = site_order)

gini_noise_summary <- gini_noise_df %>%
  group_by(site) %>%
  dplyr::summarise(
    lo = { x <- sort(gini); if (length(x) < 2) NA_real_ else x[2] },
    hi = { x <- sort(gini); n <- length(x); if (n < 2) NA_real_ else x[n - 1] },
    .groups = "drop"
  )

ggsave(
  filename = file.path(EndowDropbox, "Figures", "paper-1st-draft", "ginis_with_noise.pdf"),
  plot = ggplot(gini_noise_summary, aes(x = site)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, colour = "grey30") +
    geom_point(data = point_df %>% mutate(site = factor(site, levels = site_order)),
             aes(x = site, y = gini), colour = "firebrick", size = 1.5, shape = 18) +
    labs(x = NULL, y = "Gini") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
  device = "pdf",
  width  = 10,
  height = 5
)

  # --- Compute Gini distribution from imputations ---
gini_impute <- lapply(names(imputation_list), function(site) {
  ginis <- sapply(imputation_list[[site]],
    function(df) DescTools::Gini(df$cash_value, na.rm = TRUE))
  data.frame(site = site, gini = ginis)
})

gini_impute_df <- bind_rows(gini_impute)

imputation_flag <- gini_impute_df %>%
  group_by(site) %>%
  dplyr::summarise(imputed = n_distinct(gini, na.rm = TRUE) > 1, .groups = "drop")

point_df <- point_df %>%
  left_join(imputation_flag, by = "site") %>%
  mutate(
    site = factor(site, levels = site_order),
    imputed = ifelse(is.na(imputed), FALSE, imputed)
  )

gini_impute_df$site <- factor(gini_impute_df$site, levels = site_order)

gini_impute_summary <- gini_impute_df %>%
  group_by(site) %>%
  dplyr::summarise(
    lo = { x <- sort(gini); if (length(x) < 2) NA_real_ else x[2] },
    hi = { x <- sort(gini); n <- length(x); if (n < 2) NA_real_ else x[n - 1] },
    .groups = "drop"
  ) %>%
  mutate(site = factor(site, levels = site_order))

ggsave(
  filename = file.path(EndowDropbox, "Figures", "paper-1st-draft", "ginis_with_imputations.pdf"),
  plot = ggplot(gini_impute_summary, aes(x = site)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, colour = "grey30") +
    geom_point(data = point_df,
      aes(x = site, y = gini, colour = imputed),
      size = 1.5, shape = 18) +
    scale_colour_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey50"),
      labels = c("TRUE" = "Yes", "FALSE" = "No"),
      name = "Imputation: ") +
    labs(x = NULL, y = "Gini") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
  device = "pdf",
  width  = 10,
  height = 5
)
