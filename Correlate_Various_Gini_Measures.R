## Gini Measure Correlations
##
## For each site we can compute several variants of the Gini coefficient
## (unweighted vs weighted, using overall wealth / per-capita / per-adult /
## per-able normalisations). This script checks how correlated these are —
## establishing that the choice of specification does not much affect results.
##
## Outputs (Appendix figure):
##   Figures/gini_measure_correlations_heatmap.pdf

library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")

gini <- read.csv(file.path(path, "gini_site_data.csv"))

# ==============================================================================
# Compute and plot pairwise correlations
# ==============================================================================

vars_to_corr <- c(
  "gini_wealth_per_capita",
  "gini_weighted_wealth_per_capita",
  "gini_wealth_per_adult",
  "gini_weighted_wealth_per_adult",
  #"gini_wealth_per_able",
  #"gini_weighted_wealth_per_able",
  "gini_wealth"
)

gini_sub <- gini[, vars_to_corr]

names(gini_sub) <- c(
  "Per Capita Wealth, Unweighted",
  "Per Capita Wealth, Weighted",
  "Per Adult Wealth, Unweighted",
  "Per Adult Wealth, Weighted",
  #"Per Able Wealth, Unweighted",
  #"Per Able Wealth, Weighted",
  "Overall Wealth"
)

# Note: "per able" measures can be NaN when a site has a sharing unit with
# zero able-bodied members (wealth / 0 = Inf, and Gini is undefined).
# pairwise.complete.obs handles this gracefully.

cor_df <- cor(gini_sub, use = "pairwise.complete.obs") %>%
  reshape2::melt() %>%
  filter(Var1 != Var2)

plot <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#ea68aa", high = "#162857") +
  geom_text(
    aes(Var2, Var1, label = value %>% round(2) %>% format(digits = 2)),
    color = "white", size = 5
  ) +
  theme_classic() +
  theme(
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = " ", x = "", y = "") +
  guides(fill = guide_colourbar(title = "Correlation"))

ggsave(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "gini_measure_correlations_heatmap.pdf"),
  plot, units = "in", width = 10, height = 8.5, dpi = 300
)
ggsave(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "gini_measure_correlations_heatmap.png"),
  plot, units = "in", width = 10, height = 8.5, dpi = 300
)