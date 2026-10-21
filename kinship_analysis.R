## Kinship Network Analysis
##
## site-wise correlations are done in sitewise_regplots_clean.R
## and sitewise_regplots_iec.R (saved to paper-1st-draft/kinship/rank-rank)
##
## Examines whether the within-site wealth–degree relationship differs for
## kin vs non-kin network ties. For each site, computes the probability that
## a tie connects sharing units that are primary kin, then produces sitewise
## forest plots comparing kin and non-kin networks.
##
## Also examines whether kinship mediates the RAAW–Gini relationship.
##
## Appendix figures:
##   kinship/aaw_kin/sitewise_dotplot_rel_avg_frank_capita_primary.pdf
##   kinship/aaw_kin/scatter_rel_avg_frank_capita_primary_connectedkin_sum_vs_non_primary_connectedkin_sum.png

library(dplyr)
library(tidyr)
library(DescTools)
library(igraph)
library(stargazer)
library(plm)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ineq)
library(data.table)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")
load(file.path(path, "su_nets_expanded.rdata"))

EC <- read.csv(file.path(path, "EC_Revised.csv"))
iec <- read.csv(file.path(path, "IEC_Revised.csv"))
gini <- read.csv(file.path(path, "gini_site_data.csv"))
sitelevel_kinship <- read.csv(file.path(path, "sitelevel_kinship.csv"))

### site level : number of edges on connected kin network relative to sum network ####

results <- list()
for (site_name in names(su_nets_expanded)){
  site_graphs <- su_nets_expanded[[site_name]]
  primary_count <- gsize(site_graphs[["primary_connectedkin_sum"]])
  secondary_count <- gsize(site_graphs[["secondary_connectedkin_sum"]])
  net_count <- gsize(site_graphs[["sum"]])


  prop_primary <- if (net_count > 0) (primary_count / net_count) * 100 else NA
  prop_secondary <- if (net_count > 0) (secondary_count / net_count) * 100 else NA

  results[[site_name]] <- data.frame(
    site = site_name,
    primary_connectedkin_gsize = primary_count,
    secondary_connectedkin_gsize = secondary_count,
    sum_gsize = net_count,
    prop_primary = prop_primary,
    prop_secondary = prop_secondary
  )
}
prop_connectedkin_site_sum <- bind_rows(results)
outpath <- file.path(EndowDropbox, "Figures/Kinship/sum/")

plot_data <- prop_connectedkin_site_sum %>%
  mutate(site = reorder(site, prop_secondary)) %>%
  dplyr::select(site, prop_primary, prop_secondary) %>%
  pivot_longer(cols = starts_with("prop_"),
               names_to = "type",
               values_to = "percentage") %>%
  mutate(
    type = recode(type,
                  "prop_primary" = "Primary Kin Connections",
                  "prop_secondary" = "Secondary Kin Connections"),
    label = paste0(round(percentage, 0))
  )

p <- ggplot(plot_data, aes(x = site, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, angle = 45) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Site", y = "Percentage of Connections on Sum Network", fill = "Type",
       title = "Primary and Secondary Connected Kin (% of Total Connections on Sum Network)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
filename <- "prop_connectedkin_site_sum.png"
ggsave(filename = file.path(outpath, filename), plot = p, width = 15, height = 9, dpi = 300)

### bar plot comparing both proportion of kins that are primary versus secondary
results <- list()

for (site_name in names(su_nets_expanded)) {
  site_graphs <- su_nets_expanded[[site_name]]

  # Calculate mean degree (average number of connections per node)
  primary_deg <- degree(site_graphs[["primary_kin"]])
  secondary_deg <- degree(site_graphs[["secondary_kin"]])

  results[[site_name]] <- data.frame(
    site = site_name,
    primary_kin = mean(primary_deg),
    secondary_kin = mean(secondary_deg),
    diff = mean(secondary_deg) - mean(primary_deg)
  )
}

kin_counts <- bind_rows(results)

plot_data <- kin_counts %>%
  arrange(diff) %>%
  mutate(site = factor(site, levels = site)) %>%
  pivot_longer(cols = c("primary_kin", "secondary_kin"),
               names_to = "type",
               values_to = "avg_degree") %>%
  mutate(
    type = recode(type,
                  primary_kin = "Primary Kin",
                  secondary_kin = "Secondary Kin")
  )

p <- ggplot(plot_data, aes(x = site, y = avg_degree, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Site", y = "Average Degree (Connections per Node)", fill = "Type",
       title = "Average Primary vs. Secondary Kinship Degree per Site") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figpath <- file.path(EndowDropbox, "Figures/Kinship/")
ggsave(file.path(figpath, "average_primary_vs_secondary_kin_degree.png"),
       plot = p, width = 12, height = 6, dpi = 300)

#### Friend Rank scatter : connected kin vs. non-kin connections average friend rank #####
EC$rel_avg_frank_absolute <-
  EC$avg_frank_low_absolute / EC$avg_frank_high_absolute

EC$rel_avg_frank_capita <-
  EC$avg_frank_low_capita / EC$avg_frank_high_capita


human_aaw_vars <- list(
  avg_frank_absolute = "AAW (absolute)",
  avg_frank_low_absolute = "AAW (absolute)",
  avg_frank_low_capita = "AAW (per capita)",
  avg_frank_all_absolute = "AAW (absolute)",
  avg_frank_all_capita = "AAW (per capita)",
  rel_avg_frank_absolute = "RAAW (absolute)",
  rel_avg_frank_capita = "RAAW"
)

human_net_names <- list(
  primary_connectedkin_sum = "Primary Kin Connections",
  non_primary_connectedkin_sum = "Non-Primary Kin Connections",
  secondary_connectedkin_sum = "Secondary Kin Connections",
  non_secondary_connectedkin_sum = "Non-Secondary Kin Connections"
)

plot_ec_scatter <- function(var = "avg_frank_low_absolute", net1 = "primary_connectedkin_sum", net2 = "non_primary_connectedkin_sum") {
  df_net1 <- EC %>%
    filter(layer == net1) %>%
    dplyr::select(site, value_net1 = all_of(var))

  df_net2 <- EC %>%
    filter(layer == net2) %>%
    dplyr::select(site, value_net2 = all_of(var))

  df_combined <- left_join(df_net1, df_net2, by = "site")

  cor_stats <- cor.test(df_combined$value_net1, df_combined$value_net2)
  cor_val <- round(cor_stats$estimate, 2)
  p_val <- round(cor_stats$p.value, 4)
  cor_label <- paste0("Correlation: ", cor_val, ", p = ", p_val)

  plot <- ggplot(df_combined, aes(x = value_net1, y = value_net2, label = site)) +
    #geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", formula = y ~ x, colour = "blue", se = FALSE) +
    #geom_text_repel(size = 3, max.overlaps = Inf) +
    geom_label(
      family = "roboto",
      size = 1.5,
      alpha = 0.5,
      linewidth = 0.25,
      label.padding = unit(0.3, "lines"),
      label.r = unit(0.6, "lines")
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = cor_label,
      hjust = 1.1,
      vjust = -0.7,
      size = 4
    ) +
    theme_classic() +
    xlab(human_net_names[[net1]]) +
    ylab(human_net_names[[net2]]) +
    xlim(0.4, 1.7) +
    ylim(0.4, 1.7) +
    ggtitle(paste0(
      human_aaw_vars[[var]],
      ": ",
      human_net_names[[net1]],
      " vs. ",
      human_net_names[[net2]]
    ))

  figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "kinship", "aaw_kin")
  dir.create(figpath, showWarnings = FALSE, recursive = TRUE)

  filename <- paste0("scatter_", var, "_", net1, "_vs_", net2, ".png")
  ggsave(file.path(figpath, filename), plot = plot, width = 6, height = 6, dpi = 300)
}

#plot_ec_scatter("avg_frank_low_absolute", "primary_connectedkin_sum", "non_primary_connectedkin_sum")
#plot_ec_scatter("avg_frank_low_capita", "primary_connectedkin_sum", "non_primary_connectedkin_sum")
plot_ec_scatter("rel_avg_frank_absolute", "primary_connectedkin_sum", "non_primary_connectedkin_sum")
plot_ec_scatter("rel_avg_frank_capita", "primary_connectedkin_sum", "non_primary_connectedkin_sum")


plot_sitewise_ec_dots <- function(var = "avg_frank_low_absolute", type = "primary") {
  if (type == "primary") {
    target_layers <- c("sum", "primary_connectedkin_sum", "non_primary_connectedkin_sum")
    layer_labels <- c(
      sum = "Composite Network",
      primary_connectedkin_sum = "Primary Kin",
      non_primary_connectedkin_sum = "Non-Primary Kin"
    )
  } else if (type == "secondary") {
    target_layers <- c("sum", "secondary_connectedkin_sum", "non_secondary_connectedkin_sum")
    layer_labels <- c(
      sum = "Composite Network",
      secondary_connectedkin_sum = "Secondary Kin",
      non_secondary_connectedkin_sum = "Non-Secondary Kin"
    )
  }
  df <- EC %>%
    filter(layer %in% target_layers) %>%
    dplyr::select(site, layer, value = all_of(var)) %>%
    mutate(layer = factor(layer, levels = target_layers, labels = layer_labels))

  # plot
  plot <- ggplot(df, aes(x = site, y = value, color = layer, group = site)) +
    geom_point(position = position_dodge(width = 0.5), size = 1.5) +
    geom_line(aes(group = site), color = "grey70", alpha = 0.5) +
    scale_color_manual(
      name = "Network Layer",
      values = c(
        "Composite Network" = "gray50",
        "Primary Kin" = "#d73027",         # red
        "Non-Primary Kin" = "#4575b4",     # blue
        "Secondary Kin" = "#d73027",       # red
        "Non-Secondary Kin" = "#4575b4"    # blue
      )
    ) +
    theme_classic() +
    labs(
      # title = paste0(human_aaw_vars[[var]], " by Network Layer"),
      x = "Site",
      y = human_aaw_vars[[var]]
    ) +
    #ylim(0.25,0.75)+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )

  # save
  figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "kinship", "aaw_kin")
  filename <- paste0("sitewise_dotplot_", var, "_", type, ".pdf")
  ggsave(file.path(figpath, filename), plot = plot, width = 9, height = 5, dpi = 300)
}
## fig used in paper.
plot_sitewise_ec_dots("avg_frank_all_absolute", type = "primary")
plot_sitewise_ec_dots("avg_frank_all_capita", type = "primary")

plot_sitewise_ec_dots("avg_frank_low_absolute", type = "primary")
plot_sitewise_ec_dots("avg_frank_low_capita", type = "primary")

plot_sitewise_ec_dots("rel_avg_frank_absolute", type = "primary")
plot_sitewise_ec_dots("rel_avg_frank_capita", type = "primary")