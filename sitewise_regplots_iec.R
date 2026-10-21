## Within-Site Sitewise Plots — Individual Economic Connectedness (IEC)
##
## Produces forest-plot-style figures for the within-site correlation between
## rank wealth and rank IEC (Individual Economic Connectedness — the average
## wealth rank of a sharing unit's network partners).
##
## The core plotting function make_sitewise_plots() can be called with any
## x/y variable names to produce a new sitewise plot off-the-shelf.
##
## Paper figures (main text):
##  within-site-multipanel-frank/Sitewise_sum_rank_avg_frank_absolute_rank_wealth.png
##  within-site-multipanel-frank/Sitewise_rev_sum_rank_avg_frank_absolute_rank_wealth.png
##  within-site-multipanel-frank/Sitewise_sum_rank_avg_frank_capita_rank_wpc.png
##  within-site-multipanel-frank/Sitewise_rev_sum_rank_avg_frank_capita_rank_wpc.png
## Appendix figures (within-site-iec/):
##   Sitewise_*_rank_iec_*.png — various IEC vs wealth specifications

library(data.table)
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

master_df <- master_df[master_df$network == "sum", ]
master_df <- master_df[, names(master_df)[! names(master_df) %in% c("layer", "network")]]
iec <- read.csv(file.path(path, "IEC_Revised.csv"))

# master_df <- merge(master_df, iec, by = c("su_id", "network"))
master_df <- merge(master_df, iec, by = c("su_id"))
master_df$site <- master_df$site.x
master_df$rank_wealth <- master_df$rank_wealth.x

# Save paths
tables_path <- file.path(EndowDropbox, "Tables", "sitewise", "iec")
figpath <- file.path(EndowDropbox, "Figures", "sitewise", "paper")

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
  # ── Shuffle test setup ────────────────────────────────────────────────────────
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

  if (nrow(network_df) == 0) {
    message(
      "Warning: network '",
      network_type,
      "' not found in data. Skipping."
    )
    return(NULL)
  }

  s_index <- 0

  for (site in unique_sites) {
    s_index <- s_index + 1

    data_site <- filter(network_df, site == !!site)
    data_site <- as.data.frame(data_site)

    if (nrow(data_site) <= 1) {
      next
    }

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

    # ── Shuffle test ────────────────────────────────────────────────────────────
    results_vec <- vector(mode = "numeric", length = num_loops)
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
      results_vec[loop_num] <- coef
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

  subtitle <- paste0("Binomial sign test: ", format_pval(binom_test$p.value))

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

  return(list(
    results = results,
    binomial_p = binom_test$p.value
  ))
}

master_df <- data.table(master_df)

master_df$high_outdegree_pa <-
  master_df$IEC * master_df$out_degree_peradult_weighted

master_df$high_outdegree <-
  master_df$IEC * master_df$out_degree_weighted

master_df[, rank_iec_capita := percent_rank(IEC_capita), by = .(site, network)]
master_df[, rank_iec_adult := percent_rank(IEC), by = .(site, network)]
master_df[, rank_iec_absolute := percent_rank(IEC_absolute), by = .(site, network)]
master_df[, rank_iec_size_adjusted := percent_rank(IEC_size_adjusted), by = .(site, network)]
# master_df[, rank_high_outdegree_pa := percent_rank(high_outdegree_pa), by = .(site, network)]
# master_df[, rank_high_outdegree := percent_rank(high_outdegree), by = .(site, network)]
master_df[, rank_avg_frank_capita := percent_rank(avg_frank_capita), by = .(site, network)]
master_df[, rank_avg_frank_absolute := percent_rank(avg_frank_absolute), by = .(site, network)]
master_df[, rank_avg_frank_adult := percent_rank(avg_frank), by = .(site, network)]
master_df[, rank_avg_frank_size_adjusted := percent_rank(avg_frank_size_adjusted), by = .(site, network)]
master_df[, rank_median_frank := percent_rank(median_frank), by = .(site, network)]
master_df[, rank_sum_frank := percent_rank(sum_frank), by = .(site, network)]
master_df[, rank_sum_fwealth := percent_rank(sum_fwealth), by = .(site, network)]
master_df[, rank_median_fwealth := percent_rank(median_fwealth), by = .(site, network)]

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "within-site-multipanel-frank")

make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "rev_sum", ordered = TRUE)

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "within-site-iec")

make_sitewise_plots("rank_iec_size_adjusted", "rank_size_adjusted_wealth", "sum", ordered = TRUE)

make_sitewise_plots("rank_iec_capita", "rank_wpc", "sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_capita", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "sum", ordered = TRUE)

make_sitewise_plots("rank_median_frank", "rank_wpc", "sum", ordered = TRUE)
make_sitewise_plots("rank_sum_frank", "rank_wpc", "sum", ordered = TRUE)
make_sitewise_plots("rank_sum_frank", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_sum_fwealth", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_sum_fwealth", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("sum_fwealth", "wealth", "sum", ordered = TRUE)
make_sitewise_plots("sum_fwealth", "wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_median_fwealth", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_median_fwealth", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("median_fwealth", "wealth", "sum", ordered = TRUE)
make_sitewise_plots("median_fwealth", "wealth", "rev_sum", ordered = TRUE)

make_sitewise_plots("rank_iec_adult", "rank_wpa", "sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_adult", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_adult", "rank_wpa", "sum", ordered = TRUE)

make_sitewise_plots("rank_iec_capita", "rank_wpc", "rev_sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_capita", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_adult", "rank_wpa", "rev_sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_adult", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_adult", "rank_wpa", "rev_sum", ordered = TRUE)

make_sitewise_plots("rank_iec_adult", "rank_wpa", "sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_adult", "rank_wealth", "sum", ordered = TRUE)

make_sitewise_plots("rank_iec_capita", "rank_wpc", "rev_sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_capita", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_adult", "rank_wpa", "rev_sum", ordered = TRUE)
#make_sitewise_plots("rank_iec_adult", "rank_wealth", "rev_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "rev_sum", ordered = TRUE)

make_sitewise_plots("rank_iec_capita", "rank_wpc", "collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "main_supp", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "main_supp", ordered = TRUE)

make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "main_supp", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "main_supp", ordered = TRUE)

make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "rev_collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "rev_collapse_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "rev_main_supp", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "rev_main_supp", ordered = TRUE)


figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "kinship", "rank-rank")
make_sitewise_plots("rank_iec_capita", "rank_wpc", "primary_kin", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "primary_kin", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "primary_kin", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "primary_kin", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "non_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "non_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "rev_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "rev_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_capita", "rank_wpc", "rev_non_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_avg_frank_absolute", "rank_wealth", "rev_non_primary_connectedkin_sum", ordered = TRUE)


figpath <- file.path(EndowDropbox, "Figures/Kinship/sum")
unique_sites <- unique(master_df$site)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "sum", ordered = TRUE)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "sum", ordered = TRUE)

figpath <- file.path(EndowDropbox, "Figures/Kinship/sum/primary")
unique_sites <- unique(master_df$site)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "non_primary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "non_primary_connectedkin_sum", ordered = TRUE)


figpath <- file.path(EndowDropbox, "Figures/Kinship/sum/secondary")
unique_sites <- unique(master_df$site)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "secondary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_capita", "rank_wpc", "non_secondary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "secondary_connectedkin_sum", ordered = TRUE)
make_sitewise_plots("rank_iec_absolute", "rank_wealth", "non_secondary_connectedkin_sum", ordered = TRUE)

human_var_names <- list(
  rank_iec_capita = "Rank IEC per Capita",
  rank_iec_absolute = "Rank Absolute IEC",
  rank_wealth = "Wealth Rank",
  rank_wpc = "Wealth per Capita"
)

human_net_names <- list(
  secondary_connectedkin_sum = "Secondary Kin",
  non_secondary_connectedkin_sum = "Non-Secondary Kin",
  primary_connectedkin_sum = "Primary Kin",
  non_primary_connectedkin_sum = "Non-Primary Kin"
)

make_combined_sitewise_plot <- function(x_var, y_var, net1, net2, save_name = NULL, order_by = "coef-diff") {
  res_net1 <- make_sitewise_plots(x_var, y_var, net1, ordered = FALSE)
  res_net2 <- make_sitewise_plots(x_var, y_var, net2, ordered = FALSE)

  df_net1 <- res_net1$results
  df_net2 <- res_net2$results

  df_net1$network <- human_net_names[[net1]]
  df_net2$network <- human_net_names[[net2]]

  combined_res <- rbind(df_net1, df_net2)

  if (order_by == "coef-diff") {
    coef_diff <- abs(df_net1$coefficient - df_net2$coefficient)
    site_order <- df_net1$site[order(coef_diff)]
  } else if (order_by == "net1") {
    site_order <- df_net1$site[order(df_net1$coefficient)]
  } else if (order_by == "net2") {
    site_order <- df_net2$site[order(df_net2$coefficient)]
  } else {
    stop("Invalid 'order_by' value. Use 'coef-diff', 'net1', or 'net2'.")
  }

  combined_res$site <- factor(combined_res$site, levels = site_order)

  pval_net1 <- res_net1$binomial_p
  pval_net2 <- res_net2$binomial_p

  plot <- ggplot(combined_res, aes(x = site, y = coefficient, color = network)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(
      aes(
        ymin = pmax(coefficient - 1.96 * std_error, -1),
        ymax = pmin(coefficient + 1.96 * std_error, 1)
      ),
      width = 0.2,
      position = position_dodge(width = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    annotate("text", x = 1, y = 1.02,
             label = paste(human_net_names[[net1]], format_pval(pval_net1)),
             color = "blue", hjust = 0, size = 4) +
    annotate("text", x = 1, y = 0.92,
             label = paste(human_net_names[[net2]], format_pval(pval_net2)),
             color = "red", hjust = 0, size = 4) +
    theme_classic() +
    xlab("Site") +
    ylab(paste0("Correlation: ", human_var_names[[x_var]], " vs. ", human_var_names[[y_var]])) +
    ylim(-1, 1.1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(color = "Network Type")

  if (is.null(save_name)) {
    save_name <- paste0("Sitewise_Combined_", net1, "_vs_", net2, "_", x_var, "_", y_var, ".png")
  }

  ggsave(file.path(figpath, save_name),
         plot = plot,
         width = 10, height = 5, dpi = 300)
}

figpath <- file.path(EndowDropbox, "Figures", "Kinship", "sum")
make_combined_sitewise_plot("rank_iec_capita", "rank_wpc", "primary_connectedkin_sum", "non_primary_connectedkin_sum", order_by = "net1")
make_combined_sitewise_plot("rank_iec_absolute", "rank_wealth", "primary_connectedkin_sum", "non_primary_connectedkin_sum",  order_by = "net1")






#------------------------------------------------------------------------------

# make_sitewise_plots("rank_iec_capita", "rank_wpa", "sum", ordered = TRUE)
# make_sitewise_plots("rank_high_outdegree_pa", "rank_wpa", "sum", ordered = TRUE)
#
# make_sitewise_plots("rank_iec_capita", "rank_wpa", "collapse_sum", ordered = TRUE)
# make_sitewise_plots("rank_high_outdegree_pa", "rank_wpa", "collapse_sum", ordered = TRUE)
#
# make_sitewise_plots("rank_iec_capita", "rank_wpa", "main_supp", ordered = TRUE)
# make_sitewise_plots("rank_high_outdegree_pa", "rank_wpa", "main_supp", ordered = TRUE)