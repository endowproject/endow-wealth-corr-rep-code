## Economic Connectedness Quartile Correlations
##
## Examines how the cross-site correlation between Gini and EC varies depending
## on which wealth quartile the ego and alter belong to. Produces heatmaps
## where cell (i, j) shows the correlation between wealth Gini and the EC from
## quartile i to quartile j. Does this for both the EC and RC (Relative
## Connectedness = EC_own / EC_all) measures, and for the "sum" and "rev_sum"
## network layers.
##
## Also produces Friend Rank (Average Alter Wealth, AAW) quartile plots
## showing how correlations with Gini differ across the Q1–Q4 wealth quartiles.
##
## Main paper figures:
##   FRank-Quartiles/Frank_Quartiles_vs_GINI_WPC_sum.pdf
##   FRank-Quartiles/Frank_Quartiles_vs_GINI_WPC_rev_sum.pdf
##   RC-Quartiles/RC_Quartile_Corrs_Heatmap_sum_revsum_with_GINIPC.pdf

library(colorRamps)
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")

EC_long <- read.csv(file.path(path, "EC_Revised.csv"))

gini <- read.csv(file.path(path, "gini_site_data.csv"))

merge <- merge(EC_long, gini, by = "site")

# =========================================

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "EC-Quartiles")

# loop through layers

# layers <- unique(merge$layer)
layers <- c("sum", "rev_sum")

corr_matrices <- list()

for (l in layers) {

  dat <- merge[merge$layer == l, ]
  corr_matrices[[l]] <-
    matrix(NA, nrow = 4, ncol = 4)

  for (own in 1:4) {

    for (alter in 1:4) {

      var_name <- paste0("EC_q", own, "_to_q", alter, "_capita")

      corr_matrices[[l]][own, alter] <-
        cor(dat[, "gini_wealth_per_capita"], dat[, var_name])

    }

  }

  # plot

  varLabels <-
    c("Q1",
      "Q2",
      "Q3",
      "Q4")

  dat_to_plot <- corr_matrices[[l]] %>% reshape2::melt()
  names(dat_to_plot) <- c("OwnQuartile", "AlterQuartile", "Correlation")

  dat_to_plot$OwnQuartile <- as.factor(dat_to_plot$OwnQuartile)
  dat_to_plot$AlterQuartile <- as.factor(dat_to_plot$AlterQuartile)

  plot <-
    ggplot(dat_to_plot,
         aes(x = AlterQuartile, y = OwnQuartile, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2("Correlation",
                         low = "red",
                         mid = "white",
                         high = "cyan",
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5)) +
    geom_text(aes(label = Correlation %>% round(2) %>% format(digits = 2)),
              color = "black") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    # theme(axis.text.x = element_text(size = 10),
    #       axis.text.y = element_text(size = 10)) +
    labs(title = " ", x = "Alter Quartile", y = "Ego Quartile") +
    scale_y_discrete(labels = varLabels) +
    # scale_y_discrete(limits = rev, labels = rev(varLabels)) +
    scale_x_discrete(position = "bottom", labels = varLabels)

  # ggsave(file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.png")),
  #        # file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
  #        plot,
  #        width = 10,
  #        height = 10,
  #        units = "in")

  ggsave(file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
         # file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
         plot,
         width = 5,
         height = 5,
         units = "in")

}


# relative connectedness =========================================

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "RC-Quartiles")

# loop through layers

# layers <- unique(merge$layer)
layers <- c("sum", "rev_sum")

corr_matrices <- list()
rc_plots      <- list()   # stored for combined figure

for (l in layers) {

  dat <- merge[merge$layer == l, ]
  corr_matrices[[l]] <-
    matrix(NA, nrow = 4, ncol = 4)

  for (own in 1:4) {

    for (alter in 1:4) {

      var_name <- paste0("EC_q", own, "_to_q", alter, "_capita")
      div_name <- paste0("EC_all_to_q", alter, "_capita")

      corr_matrices[[l]][own, alter] <-
        cor(dat[, "gini_wealth_per_capita"], (dat[, var_name]/dat[,div_name]))

    }

  }

  # plot

  varLabels <-
    c("Q1",
      "Q2",
      "Q3",
      "Q4")

  dat_to_plot <- corr_matrices[[l]] %>% reshape2::melt()
  names(dat_to_plot) <- c("OwnQuartile", "AlterQuartile", "Correlation")

  dat_to_plot$OwnQuartile <- as.factor(dat_to_plot$OwnQuartile)
  dat_to_plot$AlterQuartile <- as.factor(dat_to_plot$AlterQuartile)

  plot <-
    ggplot(dat_to_plot,
           aes(x = AlterQuartile, y = OwnQuartile, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2("Correlation",
                         low = "red",
                         mid = "white",
                         high = "cyan",
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5)) +
    geom_text(aes(label = Correlation %>% round(2) %>% format(digits = 2)),
              color = "black") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    # theme(axis.text.x = element_text(size = 10),
    #       axis.text.y = element_text(size = 10)) +
    labs(title = " ", x = "Alter Quartile", y = "Ego Quartile") +
    scale_y_discrete(labels = varLabels) +
    # scale_y_discrete(limits = rev, labels = rev(varLabels)) +
    scale_x_discrete(position = "bottom", labels = varLabels)

  # ggsave(file.path(figpath, paste0("RC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.png")),
  #        # file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
  #        plot,
  #        width = 10,
  #        height = 10,
  #        units = "in")

  rc_plots[[l]] <- plot   # stored for combined side-by-side figure below

  ggsave(file.path(figpath, paste0("RC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
         plot,
         width = 5,
         height = 5,
         units = "in")

}

# Combined side-by-side figure with shared horizontal legend below
g <- ggplotGrob(
      rc_plots[["sum"]] +
        theme(legend.position = "bottom", legend.direction = "horizontal")
    )
    shared_legend <- gtable::gtable_filter(g, "guide-box")

    # Strip legend from both plots
    p1 <- rc_plots[["sum"]]     + theme(legend.position = "none")
    p2 <- rc_plots[["rev_sum"]] + theme(legend.position = "none")

    # Combine
    combined_rc <- cowplot::plot_grid(
      cowplot::plot_grid(p1, p2, nrow = 1),
      shared_legend,
      ncol = 1,
      rel_heights = c(1, 0.2)
    )

ggsave(file.path(figpath, "RC_Quartile_Corrs_Heatmap_sum_revsum_with_GINIPC.pdf"),
       combined_rc,
       width = 11, height = 6,
       units = "in")

# average friend rank ratios =========================================

weightedCorrelation <- function(x, y, w) {
  x_comp <- x[!is.na(x) & !is.na(y) & !is.na(w)]
  y_comp <- y[!is.na(x) & !is.na(y) & !is.na(w)]
  w_comp <- w[!is.na(x) & !is.na(y) & !is.na(w)]

  x_wm <- weighted.mean(x_comp, w_comp)
  y_wm <- weighted.mean(y_comp, w_comp)

  x_wsd <- sqrt(sum( (w_comp / sum(w_comp)) * (x_comp - x_wm)^2 ))
  y_wsd <- sqrt(sum( (w_comp / sum(w_comp)) * (y_comp - y_wm)^2 ))

  x_std <- (x_comp - x_wm) / x_wsd
  y_std <- (y_comp - y_wm) / y_wsd

  corr <-
    summary(lm(y_std ~ x_std, w = w_comp))$coefficients[2, 1]

  se <-
    summary(lm(y_std ~ x_std, w = w_comp))$coefficients[2, 2]

  return(c(corr, se))
}

# layers <- unique(ec$layer)
layers <- c("sum", "rev_sum")

vars_to_include <-
  c("avg_frank_q4_capita",
    "avg_frank_q3_capita",
    "avg_frank_q2_capita",
    "avg_frank_q1_capita")

# vars_to_include <-
#   paste0(vars_to_include, "_capita")

Correlations_list <-
  list()

corrs_list <-
  list()

se_list <-
  list()

df_list <-
  list()

for (l in layers) {

  ec_sub <-
    merge[merge$layer == l, ]

  Correlations_list[[l]] <-
    lapply(X = ec_sub[, vars_to_include],
           FUN = weightedCorrelation,
           y = ec_sub$gini_wealth_per_capita,
           w = rep(1, nrow(ec_sub)))

  corrs_list[[l]] <-
    sapply(Correlations_list[[l]], "[[", 1)

  se_list[[l]] <-
    sapply(Correlations_list[[l]], "[[", 2)

  df_list[[l]] <-
    data.frame(var = names(corrs_list[[l]]),
               corr = corrs_list[[l]],
               se = se_list[[l]])

  df_list[[l]]$lb <-
    ifelse(df_list[[l]]$corr - 1.96 * df_list[[l]]$se < -1,
           -1,
           df_list[[l]]$corr - 1.96 * df_list[[l]]$se)

  df_list[[l]]$ub <-
    ifelse(df_list[[l]]$corr + 1.96 * df_list[[l]]$se > 1,
           1,
           df_list[[l]]$corr + 1.96 * df_list[[l]]$se)

  df_list[[l]] <- df_list[[l]][df_list[[l]]$var != "gini_wealth_per_capita", ]

  df_list[[l]]$order <- nrow(df_list[[l]]):1
  ggplot(df_list[[l]], aes(corr, reorder(var, order))) +
    geom_pointrange(aes(xmin = lb, xmax = ub),
                    position = position_dodge(width = 0.5),
                    colour = "dodgerblue2") +
    theme_classic() +
    scale_x_continuous("Correlation Coefficient with Gini Wealth per Capita", limits = c(-1, 1)) +
    scale_y_discrete("",
                     labels = c(
                       "avg_frank_q1_capita" = "RAAW: Q1",
                       "avg_frank_q2_capita" = "RAAW: Q2",
                       "avg_frank_q3_capita" = "RAAW: Q3",
                       "avg_frank_q4_capita" = "RAAW: Q4")) +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 0, linetype = "longdash")

  ggsave(file.path(EndowDropbox,
                   "Figures",
                   "paper-1st-draft",
                   "FRank-Quartiles",
                   paste0("Frank_Quartiles_vs_GINI_WPC_", l, ".pdf")),
         units = "in",
         width = 5, height = 3)

}

# ec_sub_no_ar <-
#   ec_sub[ec_sub$site != "AR", ]

Correlations_list <-
  list()

corrs_list <-
  list()

se_list <-
  list()

df_list <-
  list()

for (l in layers) {

  ec_sub_no_ar <-
    merge[merge$layer == l, ]

  ec_sub_no_ar <-
    ec_sub_no_ar[ec_sub_no_ar$site != "AR", ]

  Correlations_list[[l]] <-
    lapply(X = ec_sub_no_ar[, vars_to_include],
           FUN = weightedCorrelation,
           y = ec_sub_no_ar$gini_wealth_per_capita,
           w = rep(1, nrow(ec_sub_no_ar)))

  corrs_list[[l]] <-
    sapply(Correlations_list[[l]], "[[", 1)

  se_list[[l]] <-
    sapply(Correlations_list[[l]], "[[", 2)

  df_list[[l]] <-
    data.frame(var = names(corrs_list[[l]]),
               corr = corrs_list[[l]],
               se = se_list[[l]])

  df_list[[l]]$lb <-
    ifelse(df_list[[l]]$corr - 1.96 * df_list[[l]]$se < -1,
           -1,
           df_list[[l]]$corr - 1.96 * df_list[[l]]$se)

  df_list[[l]]$ub <-
    ifelse(df_list[[l]]$corr + 1.96 * df_list[[l]]$se > 1,
           1,
           df_list[[l]]$corr + 1.96 * df_list[[l]]$se)

  df_list[[l]] <- df_list[[l]][df_list[[l]]$var != "gini_wealth_per_capita", ]

  df_list[[l]]$order <- nrow(df_list[[l]]):1
  ggplot(df_list[[l]], aes(corr, reorder(var, order))) +
    geom_pointrange(aes(xmin = lb, xmax = ub),
                    position = position_dodge(width = 0.5),
                    colour = "dodgerblue2") +
    theme_classic() +
    scale_x_continuous("Correlation Coefficient with Gini Wealth per Capita", limits = c(-1, 1)) +
    scale_y_discrete("",
                     labels = c(
                       "avg_frank_q1_capita" = "RAAW: Q1",
                       "avg_frank_q2_capita" = "RAAW: Q2",
                       "avg_frank_q3_capita" = "RAAW: Q3",
                       "avg_frank_q4_capita" = "RAAW: Q4")) +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 0, linetype = "longdash")

  ggsave(file.path(EndowDropbox,
                   "Figures",
                   "paper-1st-draft",
                   "FRank-Quartiles",
                   paste0("No_AR_Frank_Quartiles_vs_GINI_WPC_", l, ".pdf")),
         units = "in",
         width = 5, height = 5)

}

corr_matrices <- list()

for (l in layers) {

  dat <- merge[merge$layer == l, ]
  dat <- dat[dat$site != "AR", ]
  corr_matrices[[l]] <-
    matrix(NA, nrow = 4, ncol = 4)

  for (own in 1:4) {

    for (alter in 1:4) {

      var_name <- paste0("EC_q", own, "_to_q", alter, "_capita")

      corr_matrices[[l]][own, alter] <-
        cor(dat[, "gini_wealth_per_capita"], dat[, var_name])

    }

  }

  # plot

  varLabels <-
    c("Q1",
      "Q2",
      "Q3",
      "Q4")

  dat_to_plot <- corr_matrices[[l]] %>% reshape2::melt()
  names(dat_to_plot) <- c("OwnQuartile", "AlterQuartile", "Correlation")

  dat_to_plot$OwnQuartile <- as.factor(dat_to_plot$OwnQuartile)
  dat_to_plot$AlterQuartile <- as.factor(dat_to_plot$AlterQuartile)

  plot <-
    ggplot(dat_to_plot,
           aes(x = AlterQuartile, y = OwnQuartile, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2("Correlation",
                         low = "red",
                         mid = "white",
                         high = "cyan",
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5)) +
    geom_text(aes(label = Correlation %>% round(2) %>% format(digits = 2)),
              color = "black") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    # theme(axis.text.x = element_text(size = 10),
    #       axis.text.y = element_text(size = 10)) +
    labs(title = " ", x = "Alter Quartile", y = "Ego Quartile") +
    scale_y_discrete(labels = varLabels) +
    # scale_y_discrete(limits = rev, labels = rev(varLabels)) +
    scale_x_discrete(position = "bottom", labels = varLabels)

  # ggsave(file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.png")),
  #        # file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
  #        plot,
  #        width = 10,
  #        height = 10,
  #        units = "in")

  ggsave(file.path(figpath, paste0("No_AR_EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
         # file.path(figpath, paste0("EC_Quartile_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
         plot,
         width = 5,
         height = 5,
         units = "in")

}


# Doesn't work for quartiles in the same way.

# figpath <- file.path(file.path(EndowDropbox, "Figures", "paper", "FRank-Quartiles"))
#
# # loop through layers
#
# # layers <- unique(merge$layer)
# layers <- c("sum", "rev_sum")
#
# corr_matrices <- list()
#
# for (l in layers) {
#
#   dat <- merge[merge$layer == l, ]
#   corr_matrices[[l]] <-
#     matrix(NA, nrow = 1, ncol = 4)
#
#   for (own in 1:4) {
#
#     for (alter in 1:4) {
#
#       var_name <- paste0("EC_q", own, "_to_q", alter, "_capita")
#       div_name <- paste0("EC_all_to_q", alter, "_capita")
#
#       corr_matrices[[l]][own, alter] <-
#         cor(dat[, "gini_wealth_per_capita"], (dat[, var_name]/dat[,div_name]))
#
#     }
#
#   }
#
#   for (q in 1:4) {
#
#     var_name <- paste0("avg_frank_q", q, "_capita")
#
#     corr_matrices[[l]][1, q] <-
#       cor(dat[, "gini_wealth_per_capita"], (dat[, var_name]/dat[, "avg_frank_all_capita"]))
#
#   }
#
#   # plot
#
#   varLabels <-
#     c("Q1",
#       "Q2",
#       "Q3",
#       "Q4")
#
#   dat_to_plot <- corr_matrices[[l]] %>% reshape2::melt()
#   names(dat_to_plot) <- c("OwnQuartile", "AlterQuartile", "Correlation")
#
#   dat_to_plot$OwnQuartile <- as.factor(dat_to_plot$OwnQuartile)
#   dat_to_plot$AlterQuartile <- as.factor(dat_to_plot$AlterQuartile)
#
#   plot <-
#     ggplot(dat_to_plot,
#            aes(x = AlterQuartile, y = OwnQuartile, fill = Correlation)) +
#     geom_tile() +
#     scale_fill_gradient2("Correlation",
#                          low = "red",
#                          mid = "white",
#                          high = "cyan",
#                          limits = c(-1, 1),
#                          breaks = seq(-1, 1, by = 0.5)) +
#     geom_text(aes(label = Correlation %>% round(2) %>% format(digits = 2)),
#               color = "black") +
#     theme_minimal() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank()) +
#     labs(title = " ", x = "Alter Quartile", y = "Ego Quartile") +
#     scale_x_discrete(position = "bottom", labels = varLabels)
#
#   # ggsave(file.path(figpath, paste0("AvgFrank_Ratio_Corrs_Heatmap_", l, "_with_GINIPC.png")),
#   #        plot,
#   #        width = 10,
#   #        height = 3,
#   #        units = "in")
#
#   ggsave(file.path(figpath, paste0("AvgFrank_Ratio_Corrs_Heatmap_", l, "_with_GINIPC.pdf")),
#          plot,
#          width = 5,
#          height = 5,
#          units = "in")
#
# }

