## Wealth Ratio vs Gini Robustness Checks
##
## Produces cross-site scatter plots of 90/10 and 80/20 percentile wealth
## ratios against the Gini coefficient and against EC / Friend Rank measures.
## These serve as robustness checks that results are not sensitive to the
## choice of inequality measure.
##
## Appendix figures:
##   ninety-ten/WealthPC_90_10_vs_Gini.pdf
##   ninety-ten/WealthPC_80_20_vs_Gini.pdf
##   ninety-ten/WealthPC_90_10_vs_Rel_Sum_Frank.pdf
##   ninety-ten/WealthPC_80_20_vs_Rel_Sum_Frank.pdf

library(dplyr)
library(ggplot2)
library(ggrepel)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# ==============================================================================
# Setup
# ==============================================================================

EndowDerivedData <- file.path(EndowGitHub, "DerivedData")
figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "ninety-ten")

# Ensure output directory exists
if (!dir.exists(figpath)) {
    dir.create(figpath, recursive = TRUE)
}

# Load data
gini_dat <- read.csv(file.path(EndowDerivedData, "gini_site_data.csv"))
ec_dat <- read.csv(file.path(EndowDerivedData, "EC_Revised_Reshape.csv"))
lasso_dat <- read.csv(file.path(EndowDerivedData, "DataForLASSO.csv"))

# Calculate rel_sum_frank in lasso_dat
lasso_dat$rel_sum_frank <- lasso_dat$avg_frank_low_capita.rev_sum / lasso_dat$avg_frank_high_capita.rev_sum

# Select only relevant columns from lasso_dat to avoid column name collisions
# We only need Site and the calculated rel_sum_frank
lasso_subset <- lasso_dat[, c("Site", "rel_sum_frank")]

# Merge gini_dat and ec_dat
# Both contain "site", so we merge on it.
site_df <- merge(gini_dat, ec_dat, by = "site")

# Merge with lasso_subset
# lasso_subset uses "Site" which corresponds to "site"
site_df <- merge(site_df, lasso_subset, by.x = "site", by.y = "Site")

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) {
    "p < 0.0001"
  } else {
    paste0("p = ", formatC(p, digits = digits, format = "f"))
  }
}

# Function to create and save scatter plot
create_scatter_plot <- function(data, x_var, y_var, x_label, y_label, filename) {
    # Calculate correlation
    corr <- cor.test(data[[x_var]], data[[y_var]], use = "complete.obs")
    #cat(paste0("Correlation between ", x_var, " and ", y_var, ": ", round(corr$estimate, 4), "\n"))

    # Create plot
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
        #geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        #geom_text_repel(
        #    aes(label = site),
        #    size = 3,
        #    box.padding = 0.3,
        #    point.padding = 0.3,
        #    segment.color = "gray50",
        #    min.segment.length = 0.2,
        #    segment.size = 0.5,
        #    alpha = 0.9,
        #    color = "black"
        #) +
        geom_label(
            aes(label = site),
            family = "roboto",
            position = ggplot2::position_jitter(width = 0.1),
            size = 1.5,
            alpha = 0.5,
            linewidth = 0.25,
            label.padding = unit(0.3, "lines"),
            label.r = unit(0.6, "lines")
        ) +
        labs(
            x = x_label,
            y = y_label,
            subtitle = paste0(
                "Correlation: ",
                sprintf("%.2f", corr$estimate),
                ", ",
                format_pval(corr$p.value)
            )
        ) +
        theme_classic()

    # Save plot robustly
    width <- 5
    height <- 5
    dpi <- 300

    # Try saving to figpath first if it exists
    saved <- FALSE
    if (exists("figpath") && dir.exists(figpath)) {
        try(
            {
                ggsave(file.path(figpath, filename), plot = p, width = width, height = height, dpi = dpi)
                cat(paste0("Saved plot to: ", file.path(figpath, filename), "\n"))
                saved <- TRUE
            },
            silent = TRUE
        )
    }

    # Fallback to current directory if not saved
    if (!saved) {
        ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
        cat(paste0("Saved plot to current directory: ", filename, "\n"))
    }

    return(p)
}

# Plot 1: 80th/20th Percentile vs Wealth Gini
p1 <- create_scatter_plot(
    site_df,
    "wealth_pc_80_20",
    "gini_wealth_per_capita",
    "80th/20th Percentile Wealth per Capita",
    "Gini (Wealth per Capita)",
    "WealthPC_80_20_vs_Gini.pdf"
)

# Plot 2: 90th/10th Percentile vs Wealth Gini
p2 <- create_scatter_plot(
    site_df,
    "wealth_pc_90_10",
    "gini_wealth_per_capita",
    "90th/10th Percentile Wealth per Capita",
    "Gini (Wealth per Capita)",
    "WealthPC_90_10_vs_Gini.pdf"
)

# Plot 3: EC sum vs 80th/20th Percentile
p3 <- create_scatter_plot(
    site_df,
    "EC_capita.sum",
    "wealth_pc_80_20",
    "EC (Wealth per Capita)",
    "80th/20th Percentile Wealth per Capita",
    "WealthPC_80_20_vs_EC_Sum.pdf"
)

# Plot 4: EC sum vs 90th/10th Percentile
p4 <- create_scatter_plot(
    site_df,
    "EC_capita.sum",
    "wealth_pc_90_10",
    "EC (Wealth per Capita)",
    "90th/10th Percentile Wealth per Capita",
    "WealthPC_90_10_vs_EC_Sum.pdf"
)

# Plot 5: Rel Sum Frank vs 80th/20th Percentile
p5 <- create_scatter_plot(
    site_df,
    "rel_sum_frank",
    "wealth_pc_80_20",
    "RAAW",
    "80th/20th Percentile Wealth per Capita",
    "WealthPC_80_20_vs_Rel_Sum_Frank.pdf"
)

# Plot 6: Rel Sum Frank vs 90th/10th Percentile
p6 <- create_scatter_plot(
    site_df,
    "rel_sum_frank",
    "wealth_pc_90_10",
    "RAAW",
    "90th/10th Percentile Wealth per Capita",
    "WealthPC_90_10_vs_Rel_Sum_Frank.pdf"
)
