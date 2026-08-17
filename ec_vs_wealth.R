## Measurement validation: EC vs Wealth and Gini vs Wealth
##
## We check whether measurement error in wealth could be driving the observed
## EC–Gini relationship. The concern: in sites with lower Gini (less wealth
## spread), small valuation errors are more likely to misclassify a household
## as "above" or "below" median, which could artifically inflate EC. One test
## is to check whether EC or Gini correlates with median wealth — if
## misclassification were a major driver, we would expect a stronger
## correlation with absolute wealth levels.
##
## Outputs:
##   figures_wealth_mismeasurement/NICE_Median_Wealth_Gini_Wealth.pdf
##   figures_wealth_mismeasurement/NICE_Median_WPC_Gini_WPC.pdf
##   figures_wealth_mismeasurement/NICE_Median_WPC_RAAW.pdf
##   (plus a range of exploratory scatter plots in figures_wealth_mismeaasurement/)

library(ggplot2)
library(ggpubr)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

path <- file.path(EndowGitHub, "DerivedData")

site_data <- read.csv(file.path(path, "gini_site_data.csv"))
ec <- read.csv(file.path(path, "EC_Revised_Reshape.csv"))
md <- merge(site_data, ec, by = "site")

md$rel_frank_capita <-
  md$avg_frank_low_capita.sum / md$avg_frank_high_capita.sum

md$RC_capita.sum = md$EC_capita.sum / md$EC_capita_high.sum

plotFunction <- function(xvar, yvar) {
  ct <- cor.test(md[[xvar]], md[[yvar]], method = "pearson")
  title_str <- sprintf("r = %.3f, p = %.3f", ct$estimate, ct$p.value)

  a <- ggplot(md, aes(x = get(xvar), y = get(yvar), label = site))
  b <- a +
    #geom_point() +
    #ggrepel::geom_text_repel(aes(label = site), size = 3, max.overlaps = 10) +
    geom_label(
      aes(label = site),
      family = "roboto",
      size = 1.5,
      alpha = 0.5,
      linewidth = 0.25,
      label.padding = unit(0.3, "lines"),
      label.r = unit(0.6, "lines")
    ) +
    theme_classic() +
    xlab(xvar) +
    ylab(yvar)
  c <- b + ggtitle(title_str)

  ggsave(file.path(EndowDropbox,
                   "Figures",
                   "paper-1st-draft", "figures_wealth_mismeasurement",
                   paste0(xvar, "_", yvar, ".pdf")),
         c,
         units = "in",
         width = 10, height = 10, dpi = 300)

}

plotFunction("mean_wealth", "EC.sum")
plotFunction("median_wealth", "EC.sum")
plotFunction("max_wealth", "EC.sum")
plotFunction("mean_wealth_per_capita", "EC.sum")
plotFunction("median_wealth_per_capita", "EC.sum")
plotFunction("max_wealth_per_capita", "EC.sum")
plotFunction("mean_wealth", "gini_wealth")
plotFunction("median_wealth", "gini_wealth")
plotFunction("max_wealth", "gini_wealth")
plotFunction("mean_wealth_per_capita", "gini_wealth")
plotFunction("median_wealth_per_capita", "gini_wealth")
plotFunction("max_wealth_per_capita", "gini_wealth")
plotFunction("mean_wealth", "gini_wealth_per_capita")
plotFunction("median_wealth", "gini_wealth_per_capita")
plotFunction("max_wealth", "gini_wealth_per_capita")
plotFunction("mean_wealth_per_capita", "gini_wealth_per_capita")
plotFunction("median_wealth_per_capita", "gini_wealth_per_capita")
plotFunction("max_wealth_per_capita", "gini_wealth_per_capita")

# ========================================

# Make the nice looking plots.

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) {
    "p < 0.0001"
  } else {
    paste0("p = ", formatC(p, digits = digits, format = "f"))
  }
}

weighted_correlation <-
  cor.test(md$median_wealth_per_capita, md$gini_wealth_per_capita)

p <- ggplot(md, aes(x = median_wealth_per_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(aes(label = site), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Median Wealth per Capita ($)",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "figures_wealth_mismeasurement",
                 "NICE_Median_WPC_Gini_WPC.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(md$median_wealth, md$gini_wealth)

p <- ggplot(md, aes(x = median_wealth, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(aes(label = site), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Median Wealth ($)",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "figures_wealth_mismeasurement",
                 "NICE_Median_Wealth_Gini_Wealth.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)


weighted_correlation <-
  cor.test(md$median_wealth_per_capita, md$rel_frank_capita)

p <- ggplot(md, aes(x = median_wealth_per_capita, y = rel_frank_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(aes(label = site), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Median Wealth per Capita ($)",
    y = "Relative Average Alter Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "figures_wealth_mismeasurement",
                 "NICE_Median_WPC_RAAW.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(md$median_wealth_per_capita, md$RC_capita.sum)

p <- ggplot(md, aes(x = median_wealth_per_capita, y = RC_capita.sum)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(aes(label = site), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Median Wealth per Capita ($)",
    y = "Economic Connectedness"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "figures_wealth_mismeasurement",
                 "NICE_Median_WPC_RC.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)