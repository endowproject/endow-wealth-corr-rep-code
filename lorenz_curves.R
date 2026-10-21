## Lorenz Curves
##
## For each site, plots Lorenz curves for three wealth normalisations:
##   - Raw sharing-unit wealth
##   - Per-capita wealth (expanded to individual-level)
##   - Per-adult wealth (expanded to adult-level)
##
## Individual site plots are saved to LorenzCurves/Raw/, /Per Capita/, /Per Adult/,
## and /Combined/. Multi-panel PDFs (9 sites per page) are saved as:
##   LorenzCurves/Combined/00_Lorenz_Curves_Part_1.pdf ... Part_N.pdf
## These are the figures referenced in the paper appendix.

library(cowplot)
library(data.table)
library(dplyr)
library(grid)
library(gridExtra)
library(ineq)
library(ggplot2)

# ==============================================================================
# Setup
# ==============================================================================

path    <- file.path(EndowGitHub, "DerivedData")
figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "lorenz")

for (d in c("Raw", "Per Capita", "Per Adult", "Combined")) {
  dir.create(file.path(figpath, d), showWarnings = FALSE, recursive = TRUE)
}

data <- readRDS(file.path(path, "su_master.rds"))

safe_expand <- function(data, count_var) {
  bad <- is.na(data[[count_var]]) | data[[count_var]] < 0 | !is.finite(data[[count_var]])
  data <- data[!bad, ]
  data[rep(seq_len(nrow(data)), data[[count_var]]), ]
}

# ==============================================================================
# Per-site Lorenz curves
# ==============================================================================

unique_sites <- unique(data$site)

plot_list  <- list()   # stores combined plot for each site
lorenz_data <- NULL    # will accumulate all sites' data for multi-panel PDFs

for (s in unique_sites) {

  site_data <- data %>% filter(site == s & network == "sum")

  # --- Raw (SU-level) wealth ---

  L_wealth       <- Lc(site_data$wealth)
  lorenz_df_wealth <- data.frame(
    Population = L_wealth$p,
    Wealth     = L_wealth$L,
    Curve_Type = "Absolute Wealth",
    Site       = s
  )

  p_wealth <- ggplot(lorenz_df_wealth, aes(x = Population, y = Wealth)) +
    geom_line(color = "#1f77b4", linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "darkgray", linewidth = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    labs(
      title = paste("Lorenz Curve for Absolute Wealth (Site:", s, ")"),
      x = "Cumulative Share of Sharing Units",
      y = "Cumulative Share of Wealth"
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title       = element_text(face = "bold"),
      axis.text        = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )

  ggsave(
    filename = file.path(figpath, "Raw", paste0(s, "_Raw.png")),
    plot = p_wealth, dpi = 300, width = 8, height = 6
  )

  # --- Per-capita wealth (expanded to person-level) ---

  expanded_person <- safe_expand(site_data, "su_size")
  L_per_capita    <- Lc(expanded_person$wealth_per_capita)
  lorenz_df_per_capita <- data.frame(
    Population = L_per_capita$p,
    Wealth     = L_per_capita$L,
    Curve_Type = "Per Capita Wealth",
    Site       = s
  )

  p_per_capita <- ggplot(lorenz_df_per_capita, aes(x = Population, y = Wealth)) +
    geom_line(color = "#ff7f0e", linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "darkgray", linewidth = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    labs(
      title = paste("Lorenz Curve for Per Capita Wealth (Site:", s, ")"),
      x = "Cumulative Share of Population",
      y = "Cumulative Share of Wealth"
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title       = element_text(face = "bold"),
      axis.text        = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )

  ggsave(
    filename = file.path(figpath, "Per Capita", paste0(s, "_PerCapita.png")),
    plot = p_per_capita, dpi = 300, width = 8, height = 6
  )

  # --- Per-adult wealth (expanded to adult-level) ---

  expanded_adult <- safe_expand(site_data, "adult_count")
  L_per_adult    <- Lc(expanded_adult$wealth_per_adult)
  lorenz_df_per_adult <- data.frame(
    Population = L_per_adult$p,
    Wealth     = L_per_adult$L,
    Curve_Type = "Per Adult Wealth",
    Site       = s
  )

  p_per_adult <- ggplot(lorenz_df_per_adult, aes(x = Population, y = Wealth)) +
    geom_line(color = "#2ca02c", linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "darkgray", linewidth = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    labs(
      title = paste("Lorenz Curve for Per Adult Wealth (Site:", s, ")"),
      x = "Cumulative Share of Adults",
      y = "Cumulative Share of Wealth"
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title       = element_text(face = "bold"),
      axis.text        = element_text(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )

  ggsave(
    filename = file.path(figpath, "Per Adult", paste0(s, "_PerAdult.png")),
    plot = p_per_adult, dpi = 300, width = 8, height = 6
  )

  # --- Combined plot for this site (all three curves) ---

  combined_df <- bind_rows(lorenz_df_wealth, lorenz_df_per_capita, lorenz_df_per_adult)

  p_combined <- ggplot(combined_df, aes(x = Population, y = Wealth, color = Curve_Type)) +
    geom_line(linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "darkgray", linewidth = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
    scale_color_manual(
      values = c(
        "Absolute Wealth"   = "#1f77b4",
        "Per Capita Wealth" = "#ff7f0e",
        "Per Adult Wealth"  = "#2ca02c"
      ),
      labels = c(
        "Absolute Wealth"   = "SU-Level Distribution",
        "Per Capita Wealth" = "Person-Level Distribution",
        "Per Adult Wealth"  = "Adult-Level Distribution"
      )
    ) +
    labs(
      title = s,
      x     = "Cumulative Share of Population",
      y     = "Cumulative Share of Wealth"
    ) +
    theme_classic()

  plot_list[[s]] <- p_combined

  ggsave(
    filename = file.path(figpath, "Combined", paste0(s, "_Combined.png")),
    plot = p_combined, dpi = 300, width = 8, height = 6
  )

  # Accumulate data for the multi-panel PDFs below
  lorenz_data <- bind_rows(lorenz_data, combined_df)

  #message(paste("Saved Lorenz curves for site:", s))
}

# ==============================================================================
# Multi-panel PDFs: 12 sites per page
# These are the figures referenced in the paper appendix.
# ==============================================================================

n_pages <- lorenz_data %>% pull(Site) %>% n_distinct() %>% `/`(12) %>% ceiling()

lorenz_plots <- lapply(seq_len(n_pages), function(i) {
  lorenz_data %>%
    ggplot(aes(x = Population, y = Wealth, color = Curve_Type)) +
    geom_line(linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "darkgray") +
    ggforce::facet_wrap_paginate(~ Site, ncol = 3, nrow = 4, page = i) +
    scale_color_manual(
      values = c(
        "Absolute Wealth"   = "#1f77b4",
        "Per Capita Wealth" = "#ff7f0e",
        "Per Adult Wealth"  = "#2ca02c"
      )
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(x = "Cumulative Share of Population", y = "Cumulative Share of Wealth") +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
})

pdf(file.path(figpath, "Lorenz_Curves.pdf"), width = 10, height = 12)
walk(lorenz_plots, print)
dev.off()


# ==============================================================================
# Aggregate plots: all sites overlaid, Absolute and Per Capita
# Distinguished by the site_colour_map / site_linetype_map lookup defined above.
# ==============================================================================

agg_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text        = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.text      = element_text(size = 7)
  )


site_colours <- c(
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
  "#a65628", "#f781bf", "#999999", "#1b9e77", "#d95f02"
)

site_linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

# Build a lookup for all sites present
n_sites   <- length(site_codes)

site_style <- data.frame(
  site     = site_codes,
  colour   = site_colours[  ((seq_len(n_sites) - 1) %% length(site_colours))   + 1],
  linetype = site_linetypes[((seq_len(n_sites) - 1) %/% length(site_colours))  + 1],
  stringsAsFactors = FALSE
)

site_colour_map   <- setNames(site_style$colour,   site_style$site)
site_linetype_map <- setNames(site_style$linetype, site_style$site)

# --- Absolute wealth, all sites ---

p_agg_absolute <- lorenz_data %>%
  filter(Curve_Type == "Absolute Wealth") %>%
  ggplot(aes(x = Population, y = Wealth, color = Site, linetype = Site, group = Site)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1,
              color = "darkgray", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = site_colour_map) +
  scale_linetype_manual(values = site_linetype_map) +
  labs(
    title = "Absolute Wealth",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Wealth"
  ) +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3)) +
  agg_theme

# --- Per capita wealth, all sites ---

p_agg_per_capita <- lorenz_data %>%
  filter(Curve_Type == "Per Capita Wealth") %>%
  ggplot(aes(x = Population, y = Wealth, color = Site, linetype = Site, group = Site)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1,
              color = "darkgray", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = site_colour_map) +
  scale_linetype_manual(values = site_linetype_map) +
  labs(
    title = "Per Capita Wealth",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Wealth"
  ) +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3)) +
  agg_theme

p_agg_combined <- p_agg_per_capita + p_agg_absolute +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(figpath, "Lorenz_Curves_AllSites_Panel.png"),
  plot = p_agg_combined, dpi = 300, width = 14, height = 8
)