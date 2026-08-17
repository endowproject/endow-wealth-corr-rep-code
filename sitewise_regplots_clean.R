## Within-Site Sitewise Regression Plots — Degree vs Wealth
##
## For each site, runs a standardised regression of rank wealth on rank degree
## (in and out, absolute and per-capita). Produces forest-plot-style figures
## where each dot is one site's correlation coefficient (±95% CI), coloured
## blue if positive and red if negative. Also shows a binomial-test p-value
## indicating whether more sites have positive correlations than expected by
## chance under a null of no correlation.
##
## The core plotting function make_sitewise_plots() can be called with any
## x/y variable names to produce a new sitewise plot off-the-shelf.
##
## Main paper figures (within-site-multipanel/):
##   Sitewise_sum_rank_out_degree_weighted_rank_wealth.png
##   Sitewise_sum_rank_in_degree_weighted_rank_wealth.png
##   Sitewise_sum_rank_out_degree_percap_weighted_rank_wpc.png
##   Sitewise_sum_rank_in_degree_percap_weighted_rank_wpc.png
## Appendix figures: collapse_sum, main_supp, per-adult, and kinship variants,
##   plus size versus wealth and degree.

library(dplyr)
library(tidyr)
library(DescTools)
library(igraph)
library(stargazer)
library(plm)
library(stringr)
library(ggplot2)

# ==============================================================================
# Setup
# ==============================================================================

path <-
  file.path(EndowGitHub,
            "DerivedData")

master_df <- readRDS(file.path(path, "su_master.rds"))

# --------------

format_pval <- function(p, digits = 4) {
      stopifnot(is.numeric(p), p >= 0, p <= 1)
      if (p < 0.0001) {
        "p < 0.0001"
      } else {
        paste0("p = ", formatC(p, digits = digits, format = "f"))
      }
    }

unique_sites <- unique(master_df$site)

num_loops <- 10

make_sitewise_plots <- function(
  x,
  y,
  network_type,
  ordered = TRUE,
  show_pooled = TRUE
) {
  results_loops <- list()
  for (loop_num in 1:num_loops) {
    results_loops[[loop_num]] <-
      vector(mode = "numeric", length = length(unique_sites))
  }

  results <-
    data.frame(
      site = character(),
      coefficient = numeric(),
      std_error = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

  network_df <- master_df %>% filter(network == network_type)

  s_index <- 0

  for (site in unique_sites) {
    s_index <- s_index + 1

    data_site <- filter(network_df, site == !!site)

    xn <- as.numeric(data_site[, x])
    yn <- as.numeric(data_site[, y])

    if (all(xn[!is.na(xn)] - mean(xn[!is.na(xn)]) == 0)) {
      next
    }

    std_x <- (xn - mean(xn, na.rm = TRUE)) / sd(xn, na.rm = TRUE)
    std_y <- (yn - mean(yn, na.rm = TRUE)) / sd(yn, na.rm = TRUE)

    formula <- as.formula(paste("std_y ~", "std_x"))
    model <- lm(formula, data = data_site)

    coef_summary <- summary(model)$coefficients
    coef <- coef_summary["std_x", "Estimate"]
    std_err <- coef_summary["std_x", "Std. Error"]
    p_value <- coef_summary["std_x", "Pr(>|t|)"]

    results <- rbind(
      results,
      data.frame(
        site = site,
        coefficient = coef,
        std_error = std_err,
        p_value = p_value
      )
    )

    shuffle_formula <- as.formula(paste("std_y ~", "std_x_reshuffle"))

    for (loop_num in 1:num_loops) {
      std_x_reshuffle <- std_x[sample(
        1:length(std_x),
        length(std_x),
        replace = FALSE
      )]
      model_shuffle <- lm(shuffle_formula)
      coef_summary <- summary(model_shuffle)$coefficients
      coef <- coef_summary["std_x_reshuffle", "Estimate"]
      results_loops[[loop_num]][s_index] <- coef
    }
  }

  if (ordered) {
    results <- results[order(results$coefficient), ]
  }

  # ── Binomial test ─────────────────────────────────────────────────────────────
  num_positive_coefs <- sum(results$coefficient > 0)
  num_coefs <- nrow(results)
  binom_test <- binom.test(
    x = num_positive_coefs,
    n = num_coefs,
    p = 0.5,
    alternative = "two.sided"
  )
  #print(paste0(
  #  "P so many positive coefs given null of 0.5::::   ",
  #  binom_test$p.value
  #))

  subtitle <- paste0(
    "Binomial sign test: ",
    format_pval(binom_test$p.value)
  )

  # ── Optional pooled RE estimate ───────────────────────────────────────────────
  if (show_pooled) {
    meta_fit <- metafor::rma(
      yi = coefficient,
      sei = std_error,
      data = results,
      method = "REML"
    )

    pooled_row <- data.frame(
      site = "Pooled",
      coefficient = as.numeric(meta_fit$b),
      std_error = as.numeric(meta_fit$se),
      p_value = as.numeric(meta_fit$pval)
    )

    subtitle <- paste0(
      "Binomial sign test: ",
      format_pval(binom_test$p.value),
      "     |     ",
      "Pooled coefficient = ",
      sprintf("%.2f", pooled_row$coefficient),
      ", ",
      format_pval(pooled_row$p_value)#,
      #", I\u00b2 = ",
      #sprintf("%.0f%%", meta_fit$I2)
    )

    plot_df <- rbind(results, pooled_row) |>
      dplyr::mutate(
        site_order = factor(site, levels = c(unique(results$site), "Pooled")),
        conf_low = pmax(coefficient - 1.96 * std_error, -1),
        conf_high = pmin(coefficient + 1.96 * std_error, 1),
        is_pooled = site == "Pooled"
      )
  } else {
    plot_df <- results |>
      dplyr::mutate(
        site_order = factor(site, levels = unique(results$site)),
        conf_low = pmax(coefficient - 1.96 * std_error, -1),
        conf_high = pmin(coefficient + 1.96 * std_error, 1),
        is_pooled = FALSE
      )
  }

  # ── Plot ──────────────────────────────────────────────────────────────────────
  n_sites <- sum(!plot_df$is_pooled)
  sep_x <- n_sites + 0.5

  plot <-
    ggplot(plot_df, aes(x = site_order, y = coefficient)) +

    {
      if (show_pooled) {
        geom_vline(
          xintercept = sep_x,
          colour = "grey60",
          linewidth = 0.4,
          linetype = "dashed"
        )
      }
    } +

    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high, linewidth = is_pooled),
      width = 0.2
    ) +
    scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 0.9)) +

    geom_point(
      data = ~ dplyr::filter(., !is_pooled),
      aes(colour = coefficient > 0),
      size = 2
    ) +
    scale_colour_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +

    {
      if (show_pooled) {
        geom_point(
          data = ~ dplyr::filter(., is_pooled),
          shape = 23,
          size = 4,
          fill = "#2c3e50",
          colour = "#2c3e50"
        )
      }
    } +

    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +

    theme_classic() +
    scale_y_continuous(limits = c(-1, 1)) +
    xlab("Site") +
    ylab("Correlation Coefficient and 95% CI") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(subtitle = subtitle)

  ggsave(
    file.path(
      figpath,
      paste0("Sitewise_", network_type, "_", x, "_", y, ".png")
    ),
    plot = plot,
    units = "in",
    width = 10,
    height = 5,
    dpi = 200
  )
}

# Main Paper Figures

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "within-site-multipanel")

## Four-Panel Figure

make_sitewise_plots("rank_out_degree_percap_weighted", "rank_wpc", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_percap_weighted", "rank_wpc", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_weighted", "rank_wealth", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "rank_wealth", "sum", ordered = TRUE, show_pooled = TRUE)

# Appendix Figures

## Size vs Wealth, In-Degree, Out-Degree

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "sitewise-size")

make_sitewise_plots("rank_wealth", "su_size", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_weighted", "su_size", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "su_size", "sum", ordered = TRUE, show_pooled = TRUE)

## Proportional Composite Network

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "within-site-multipanel")

make_sitewise_plots("rank_out_degree_percap_weighted", "rank_wpc", "collapse_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_percap_weighted", "rank_wpc", "collapse_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_weighted", "rank_wealth", "collapse_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "rank_wealth", "collapse_sum", ordered = TRUE, show_pooled = TRUE)

## Raw Composite Network

make_sitewise_plots("rank_out_degree_percap_weighted", "rank_wpc", "main_supp", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_percap_weighted", "rank_wpc", "main_supp", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_weighted", "rank_wealth", "main_supp", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "rank_wealth", "main_supp", ordered = TRUE, show_pooled = TRUE)

## (Per Adult)

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "within-site-other")

make_sitewise_plots("rank_out_degree_peradult_weighted", "rank_wpa", "sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_peradult_weighted", "rank_wpa", "sum", ordered = TRUE, show_pooled = TRUE)

## Kinship

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "kinship", "rank-rank")

make_sitewise_plots("rank_out_degree_weighted", "rank_wealth", "primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_weighted", "rank_wealth", "non_primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_percap_weighted", "rank_wpc", "primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_out_degree_percap_weighted", "rank_wpc", "non_primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "rank_wealth", "primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_weighted", "rank_wealth", "non_primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_percap_weighted", "rank_wpc", "primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)
make_sitewise_plots("rank_in_degree_percap_weighted", "rank_wpc", "non_primary_connectedkin_sum", ordered = TRUE, show_pooled = TRUE)