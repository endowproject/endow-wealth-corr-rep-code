## LASSO and Related Cross-Site Analysis
##
## This is the main cross-site analysis script. It produces:
##
##   (a) Cross-site scatter plots (main text, cross-site/):
##       IDeg_Gini_vs_Gini.pdf, ODeg_Gini_vs_Gini.pdf,
##       Rel_Frank_vs_Gini.pdf, Modularity_vs_Gini.pdf
##
##   (b) Correlations with Gini and with Relative Frank:
##       CorrelationsWithgini_wealth_per_capita.pdf
##       CorrelationsWithrel_frank_capita.pdf
##
##   (c) LASSO variable selection figure (Appendix, LASSO-And-Related/):
##       LASSO_gini_wealth_per_capita_with_rel_frank_capitaand_xvars.pdf
##
##   (d) Wage labor and property rights vs Gini (Appendix):
##       WageLabor_vs_Gini.pdf, Property_Rights_vs_Gini.pdf
##
##   (e) Modularity figures (Appendix, wealth modularity/):
##       mod_facet_all.pdf
##
##   (f) Bivariate and Regression tables (Appendix, tables/):
##       CorrelationsWithgini_wealth_per_capita.tex
##       CorrelationsWithrel_frank_capita.tex
##       Stepwise_Regression_from_Lasso.tex
##
##   (g) Descriptive tables of site-level variables (Appendix, tables/)
##       other_site_vars_table.tex
##       wealth_site_vars_table.tex
##       wealth_mod_site_vars_table.tex
##       wealth_mod_site_vars_table.tex

library(dplyr)
library(tidyr)
library(DescTools)
library(igraph)
library(stargazer)
library(plm)
library(ggrepel)
library(glmnet)
library(stringr)
library(ggplot2)
library(ggcorrplot)
library(randomForest)
library(reshape)
library(xtable)
library(Hmisc)
library(purrr)
library(ppcor)
library(tidytext)
library(patchwork)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

stargazer_silent <- function(...) {
  invisible(capture.output(stargazer(...)))
}

add_stargazer_note <- function(tex_path, note_text) {
  lines <- readLines(tex_path)

  tabular_start <- grep("\\\\begin\\{tabular\\}", lines)
  tabular_end   <- grep("\\\\end\\{tabular\\}", lines)

  if (length(tabular_start) != 1 || length(tabular_end) != 1) {
    warning("Could not uniquely locate tabular block in ", tex_path, " — file left unchanged.")
    return(invisible(NULL))
  }

  # Remove stargazer's own note line(s), whatever they contain.
  # These appear between the last \hline and \end{tabular}, and contain "Note" or a lone stray "&" multicolumn line.
  note_line_idx <- grep("Note", lines)
  note_line_idx <- note_line_idx[note_line_idx > tabular_start & note_line_idx < tabular_end]

  if (length(note_line_idx) > 0) {
    lines <- lines[-note_line_idx]
    tabular_start <- grep("\\\\begin\\{tabular\\}", lines)
    tabular_end   <- grep("\\\\end\\{tabular\\}", lines)
  }

  lines <- append(lines, "\\begin{threeparttable}", after = tabular_start - 1)
  tabular_end <- grep("\\\\end\\{tabular\\}", lines)

  tablenotes_block <- c(
    "\\begin{tablenotes}",
    "\\small",
    paste0("\\item ", note_text),
    "\\end{tablenotes}",
    "\\end{threeparttable}"
  )

  lines <- append(lines, tablenotes_block, after = tabular_end)
  writeLines(lines, tex_path)
}

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")

dat <-
  read.csv(file.path(path, "DataForLASSO.csv"))

dat$RC_capita.sum = dat$EC_capita.sum / dat$EC_capita_high.sum

dat$RC_capita.rev_sum = dat$EC_capita.rev_sum / dat$EC_capita_high.rev_sum

dat$RC_size_adjusted.sum = dat$EC_size_adjusted.sum / dat$EC_size_adjusted_high.sum

dat$RC_size_adjusted.rev_sum = dat$EC_size_adjusted.rev_sum / dat$EC_size_adjusted_high.rev_sum

dat$rel_frank_capita <-
  dat$avg_frank_low_capita.sum / dat$avg_frank_high_capita.sum

dat$rel_frank_capita_rev <-
  dat$avg_frank_low_capita.rev_sum / dat$avg_frank_high_capita.rev_sum

dat$rel_median_frank_capita <-
  dat$median_frank_low_capita.sum / dat$median_frank_high_capita.sum

dat$rel_median_frank_capita_rev <-
  dat$median_frank_low_capita.rev_sum / dat$median_frank_high_capita.rev_sum

dat$rel_sum_frank_capita <-
  dat$avg_sum_frank_low_capita.sum / dat$avg_sum_frank_high_capita.sum

dat$rel_sum_frank_capita_rev <-
  dat$avg_sum_frank_low_capita.rev_sum / dat$avg_sum_frank_high_capita.rev_sum

dat$rel_sum_fwealth <- # these are listed as capita, but that just means we weight by su_size when we average and we use ego wealth as wpc for belo vs above median
  dat$avg_sum_fwealth_low_capita.sum / dat$avg_sum_fwealth_high_capita.sum

dat$rel_sum_fwealth_rev <-
  dat$avg_sum_fwealth_low_capita.rev_sum / dat$avg_sum_fwealth_high_capita.rev_sum

dat$rel_sum_fwealth_absolute <-
  dat$avg_sum_fwealth_low_absolute.sum / dat$avg_sum_fwealth_high_absolute.sum

dat$rel_sum_fwealth_absolute_rev <-
  dat$avg_sum_fwealth_low_absolute.rev_sum / dat$avg_sum_fwealth_high_absolute.rev_sum

dat$rel_median_fwealth_absolute <-
  dat$median_fwealth_low_absolute.sum / dat$median_fwealth_high_absolute.sum

dat$rel_median_fwealth_absolute_rev <-
  dat$median_fwealth_low_absolute.rev_sum / dat$median_fwealth_high_absolute.rev_sum

dat$rel_sum_rank_fwealth_capita <-
  dat$avg_sum_frank_low_capita.sum / dat$avg_sum_frank_high_capita.sum

dat$rel_sum_rank_fwealth_capita_rev <-
  dat$avg_sum_frank_low_capita.rev_sum / dat$avg_sum_frank_high_capita.rev_sum

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) {
    "p < 0.0001"
  } else {
    paste0("p = ", formatC(p, digits = digits, format = "f"))
  }
}

# ---------------------------------------

# Add in Modularity Data
modularity <- readRDS(file.path(path, "modularity_data_set_wealth_per_capita.rds"))
modularity <- modularity[!modularity$site %in% SITES_TO_DROP, ]
modularity$site_name <- modularity$site

# plot modularity
plot_final_df <- modularity %>% left_join(dat, by = "site_name")

dat_to_plot <-
  plot_final_df %>%
  dplyr::select(site_name, max_normed_modularity, gini_wealth_per_capita) %>%
  distinct()

weighted_correlation <-
  cor.test(dat_to_plot$gini_wealth_per_capita, dat_to_plot$max_normed_modularity)

plot_modularity <- dat_to_plot %>%
  ggplot(aes(x = max_normed_modularity, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Normed Wealth Modularity",
    y = "Gini of Wealth Per Capita"
  ) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Modularity_vs_Gini.pdf"),
       units = "in", width = 5, height = 5)

# ---------------------------------------

weighted_correlation <-
  cor.test(dat$ideg_pc_gini, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = ideg_pc_gini, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Gini of Support Provisioning per Capita",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "IDeg_Gini_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$odeg_pc_gini, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = odeg_pc_gini, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Gini of Support Access per Capita",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "ODeg_Gini_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_frank_capita, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = rel_frank_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Average Alter Wealth",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Frank_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_frank_capita_rev, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = rel_frank_capita_rev, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Average Alter Wealth (of supportees)",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Frank_Rev_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_median_frank_capita, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = rel_median_frank_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Median Alter Wealth of the Poor",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Median_Frank_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_frank_capita, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = rel_sum_frank_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Sum Alter Wealth of the Poor",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_Frank_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_frank_capita, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = rel_sum_frank_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Sum Alter Wealth of the Poor",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_Frank_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_fwealth, dat$gini_wealth)

ggplot(dat, aes(x = rel_sum_fwealth, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of USD Alter Wealth of the Poor",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_FWealth_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_fwealth_absolute, dat$gini_wealth)

ggplot(dat, aes(x = rel_sum_fwealth_absolute, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of Alter Wealth of the Poor (of supporters)",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_FWealth_Absolute_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_fwealth_absolute_rev, dat$gini_wealth)

ggplot(dat, aes(x = rel_sum_fwealth_absolute_rev, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of Alter Wealth of the Poor (of supporters)",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_FWealth_Absolute_Rev_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_median_fwealth_absolute, dat$gini_wealth)

ggplot(dat, aes(x = rel_median_fwealth_absolute, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Median of Alter Wealth of the Poor (of supporters)",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Median_FWealth_Absolute_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_median_fwealth_absolute_rev, dat$gini_wealth)

ggplot(dat, aes(x = rel_median_fwealth_absolute_rev, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Median of Alter Wealth of the Poor (of supporters)",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Median_FWealth_Absolute_Rev_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_sum_rank_fwealth_capita, dat$gini_wealth)

ggplot(dat, aes(x = rel_sum_rank_fwealth_capita, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of USD Alter Wealth of the Poor",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_Rank_FWealth_Absolute_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

dat$prop_time_salary <-
  as.numeric(dat$prop_time_salaried_outside) +
  as.numeric(dat$prop_time_salaried_within)

weighted_correlation <-
  cor.test(dat$prop_time_salary, dat$gini_wealth_per_capita)

ggplot(dat, aes(x = prop_time_salary, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Time in Wage/Salaried Labor (%)",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "WageLabor_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$prop_time_salary, dat$rel_frank_capita)

ggplot(dat, aes(x = prop_time_salary, y = rel_frank_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Time in Wage/Salaried Labor (%)",
    y = "Relative Average Alter Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "WageLabor_vs_AAW.pdf"),
       units = "in",
       width = 5, height = 5)

### Without AR

dat_no_AR <- dat[dat$site_name != "AR", ]

weighted_correlation <-
  cor.test(dat_no_AR$ideg_pc_gini, dat_no_AR$gini_wealth_per_capita)

ggplot(dat_no_AR, aes(x = ideg_pc_gini, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Gini of Support Provisioning per Capita",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "IDeg_Gini_vs_Gini_No_AR.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat_no_AR$rel_frank_capita, dat_no_AR$gini_wealth_per_capita)

ggplot(dat_no_AR, aes(x = rel_frank_capita, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Average Alter Wealth",
    y = "Gini of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Frank_vs_Gini_No_AR.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat_no_AR$rel_sum_fwealth, dat_no_AR$gini_wealth)

ggplot(dat_no_AR, aes(x = rel_sum_fwealth, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of USD Alter Wealth of the Poor",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_FWealth_vs_Gini_No_AR.pdf"),
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat_no_AR$rel_sum_fwealth_absolute, dat_no_AR$gini_wealth)

ggplot(dat_no_AR, aes(x = rel_sum_fwealth_absolute, y = gini_wealth)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
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
    x = "Relative Sum of USD Alter Wealth of the Poor",
    y = "Gini of Wealth"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "Rel_Sum_FWealth_Absolute_vs_Gini_No_AR.pdf"),
       units = "in",
       width = 5, height = 5)


# Modularity
n_pages <- modularity %>% pull(site) %>% n_distinct() %>% `/`(12) %>% ceiling()

mod_facet_all <- lapply(seq_len(n_pages), function(i) {
  modularity %>%
    group_by(site) %>%
    mutate(wealth_rank = percent_rank(wealth_cut)) %>%
    ungroup() %>%
    ggplot() +
    ggforce::facet_wrap_paginate(~site, scales = "free", ncol = 3, nrow = 4, page = i) +
    geom_point(aes(
      x = wealth_rank,
      y = modularity,
      color = factor(significant)),
      size = 1.2) +
    scale_color_manual(
      values = c("0" = "grey70", "1" = "darkblue"),
      labels = c("0" = "Non-significant", "1" = "Significant")
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
      ) +
    labs(x = "Wealth Percentile Rank", y = "Modularity", color = "Significance")
})

pdf(file.path(EndowDropbox, "Figures", "paper-1st-draft", "wealth modularity", "mod_facet_all.pdf"), width = 10, height = 12)
walk(mod_facet_all, print)
dev.off()

# ---------------------------------------

# Light Cleaning

dat$prop_cash_exchange <-
  as.numeric(dat$prop_prod_market_outside) +
  as.numeric(dat$prop_prod_market_within)

dat$prop_exchange_outside <-
  as.numeric(dat$prop_prod_market_outside) +
  as.numeric(dat$prop_prod_nonmonetary_outside)

dat$prop_time_salary <-
  as.numeric(dat$prop_time_salaried_outside) +
  as.numeric(dat$prop_time_salaried_within)

dat$prop_time_outside <-
  as.numeric(dat$prop_time_salaried_outside) +
  as.numeric(dat$prop_time_nonmonetary_outside) +
  as.numeric(dat$prop_time_sale_outside)

shock_vars <- colnames(dat)[grep("how_common", colnames(dat))]

dat$shock_frequency <- rowSums(dat[,shock_vars], na.rm = TRUE)
dat$shock_mean <- rowMeans(dat[,shock_vars], na.rm = TRUE)
dat$shock_1plus <- rowSums(dat[,shock_vars] > 0, na.rm = TRUE)
dat$shock_2plus <- rowSums(dat[,shock_vars] > 1, na.rm = TRUE)
dat$shock_3plus <- rowSums(dat[,shock_vars] > 2, na.rm = TRUE)

hraf_vars <-
  c("CommunalLandOnly",
    "PrivateLandPredominantly",
    "LandMarketExists",
    "DepAnimals_num",
    "DepAgriculture_num",
    "DepTrade_num",
    "AnyClassDistinctions",
    "StructuralClassDistinctions")

x_vars <-
  c(# "mean_wealth_per_capita",
    "median_wealth_per_capita",
    # "gdp_percap_2019",
    # "gini_2019",
    "NY.GDP.PCAP.KD_value",
    "SI.POV.GINI_value",
    "prop_cash_exchange",
    # "prop_prod_market_outside",
    # "prop_prod_market_within",
    # "prop_prod_nonmonetary_within",
    # "prop_prod_nonmonetary_outside",
    "prop_time_salary",
    # "prop_time_salaried_outside",
    # "prop_time_salaried_within",
    "strong_norms_on_production",
    "how_common_climate_shock",
    "how_common_disease_shock",
    "how_common_violence_shock",
    "how_common_price_shock",
    #"shock_mean",
    "shock_2plus",
    "SE.ADT.LITR.ZS_value",
    "hii_250_mean",
    #"MAP_accessibility_buffer250_spatialmean_value",
    #"CHIRPS_precipitation_buffer250_years0_spatialmean_temporalmean_value",
    "v2x_corr_value",
    "v2xpe_exlecon_value",
    "v2xcl_prpty_value",
    "v2xcl_dmove_value",
    hraf_vars,
    #"CSP_gHM_250_mean", # Using hii_250_mean instead: the CSP Human mod variable is only for 2016
    "MAP_accessibility_1000_mean",
    "pop_density",
    "nightlight",
    "precip_all",
    "bedd")

x_var_labels <- c(
  median_wealth_per_capita = "Median Wealth per Capita",
  NY.GDP.PCAP.KD_value = "National GDP per Capita",
  SI.POV.GINI_value = "National Gini",
  prop_cash_exchange = "Cash-Oriented Production %",
  prop_prod_market_outside = "Production for Market outside Site %",
  prop_prod_market_within = "Production for Market within Site %",
  prop_time_salaried_outside = "Time for Salary outside Site %",
  prop_time_salaried_within = "Time for Salary within Site %",
  prop_exchange_outside = "Outside-Oriented Production %",
  prop_time_salary = "Time in Wage/Salaried Labor %",
  prop_time_outside = "Time in Outside Work %",
  strong_norms_on_production = "Strong Production Norms",
  how_common_climate_shock = "Climate Shock Frequency",
  how_common_disease_shock = "Disease Shock Frequency",
  how_common_violence_shock = "Violence Shock Frequency",
  how_common_price_shock = "Price Shock Frequency",
  shock_mean = "Average Shock Frequency",
  shock_2plus = "Count of More-Than-Yearly Shocks",
  SE.ADT.LITR.ZS_value = "National Literacy Rate",
  hii_250_mean = "Human Influence Index",
  # MAP_accessibility_buffer250_spatialmean_value = "Travel Time to Cities", # Using 1km buffer below as better coverage
  # CHIRPS_precipitation_buffer250_years0_spatialmean_temporalmean_value = "Rainfall", # using precip_all below as better coverage
  v2x_corr_value = "Corruption Index",
  v2xpe_exlecon_value = "Socioeconomic Exclusion Index",
  v2xcl_prpty_value = "Private Property Rights Index",
  v2xcl_dmove_value = "Freedom of Movement Index",
  CommunalLandOnly = "Communal Land Only",
  PrivateLandPredominantly = "Predominantly Private Land",
  LandMarketExists = "Land Market Exists",
  DepAnimals_num = "Dependence on Animals",
  DepAgriculture_num = "Dependence on Agriculture",
  DepTrade_num = "Dependence on Trade",
  AnyClassDistinctions = "Any Class Distinctions",
  StructuralClassDistinctions = "Structural Class Distinctions",
  prop_primaryconnected_directed_gsize = "% Connections to Primary Kin",
  #CSP_gHM_250_mean = "Human Modification %", # Using HII instead: the CSP Human mod variable is only for 2016
  MAP_accessibility_1000_mean = "Time to Dense Pop Area",
  pop_density = "Population Density",
  nightlight = "Night Lights",
  precip_all = "Precipitation",
  bedd = "Biologically-Effective Degree Days",
  n_su = "Number of Sharing Units"
)

y_vars <-
  c("gini_wealth_per_capita",
    "gini_weighted_wealth_per_capita",
    "gini_wealth_per_adult",
    "gini_weighted_wealth_per_adult",
    "gini_wealth",
    "gini_size_adjusted_wealth")

y_var_labels <- c(
  gini_wealth_per_capita = "Wealth per Capita Gini",
  gini_weighted_wealth_per_capita = "Wealth per Capita Gini (Weighted)",
  gini_wealth_per_adult = "Wealth per Adult Gini",
  gini_weighted_wealth_per_adult = "Wealth per Adult Gini (Weighted)",
  gini_wealth = "Wealth Gini",
  gini_size_adjusted_wealth = "Size-Adjusted Wealth Gini"
)

## add on relative contentedness, etc., here?

# dat$rel_frank_capita <-
#   dat$avg_frank_low_capita.sum / dat$avg_frank_high_capita.sum

dat$rel_frank_capita_exchange <-
  dat$avg_frank_low_capita.exchange / dat$avg_frank_high_capita.exchange


ec_vars <-
  c("EC_capita.sum",
    # "EC_capita.exchange",
    # "EC_absolute.sum",
    # "EC_absolute.exchange",
    # "RC_size_adjusted.sum",
    "RC_capita.sum",
    "rel_frank_capita")
    # "rel_frank_capita_exchange")

ec_var_labels <- c(
  EC_capita.sum = "Economic Connectedness",
  EC_capita.exchange = "EC: Exchange Layer",
  EC_absolute.sum = "EC Absolute: Sum Layer",
  EC_absolute.exchange = "EC Absolute: Exchange Layer",
  RC_size_adjusted.sum = "RC Size-Adjusted: Sum Layer",
  RC_capita.sum = "Relative Connectedness",
  rel_frank_capita = "Relative Average Alter Wealth",
  rel_frank_capita_exchange = "Relative Average Alter Wealth: Exchange"
)

# Matt asked for the x variables to be subsetted:

matt_x_vars <-
  c("NY.GDP.PCAP.KD_value",
    #"SI.POV.GINI_value",
    "strong_norms_on_production",
    "v2xcl_dmove_value",
    "v2x_corr_value",
    "v2xcl_prpty_value",
    #"literacy",
    #"how_common_price_shock",
    #"how_common_climate_shock",
    "shock_2plus",
    "prop_cash_exchange",
    # "prop_prod_market_outside",
    # "prop_prod_market_within",
    # "prop_prod_nonmonetary_within",
    # "prop_prod_nonmonetary_outside",
    # "prop_time_salaried_outside",
    # "prop_time_salaried_within",
    "prop_time_salary",
    "prop_primaryconnected_directed_gsize",
    #"CSP_gHM_250_mean",
    "hii_250_mean",
    #"MAP_accessibility_1000_mean",
    #"pop_density",
    #"nightlight", # removing, as colinear with human modification variables and we are missing for some sites
    #"precip_all",
    "bedd")

all_labels <-
  c(x_var_labels, y_var_labels, ec_var_labels)

# ----------------------------------------

all_vars <-
  c(matt_x_vars, ec_vars, y_vars, "n_su")

dat_sub <-
  dat[, c("site_name", all_vars)]

# ----------------------------------------

weighted_correlation <-
  cor.test(dat$v2xcl_prpty_value, dat$gini_wealth_per_capita)

ggplot(dat_sub, aes(x = v2xcl_prpty_value, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  #geom_text_repel(aes(label = site_name), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site_name),
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
    x = "Private Property Rights Index",
    y = "Gini Wealth per Capita"
  ) +
  guides(size = "none") +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "Property_Rights_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)


weighted_correlation <-
  cor.test(dat$prop_cash_exchange, dat$gini_wealth_per_capita)

ggplot(dat_sub, aes(x = prop_cash_exchange, y = gini_wealth_per_capita)) +
  #geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  #geom_text_repel(aes(label = site_name), size = 3, max.overlaps = 10) +
  geom_label(
    aes(label = site_name),
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
    x = "Percent of Produce for Cash Exchange",
    y = "Gini Wealth per Capita"
  ) +
  guides(size = "none") +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "Cash_Exchange_vs_Gini.pdf"),
       units = "in",
       width = 5, height = 5)

# ----------------------------------------

# Map of Correlations for All Variables

corrmap_vars_to_drop <-
  c("gini_weighted_wealth_per_capita",
    "gini_wealth_per_adult",
    "gini_weighted_wealth_per_adult",
    "gini_wealth")

## Drop the Site Code
var_data <-
  dat_sub[, all_vars[! all_vars %in% corrmap_vars_to_drop]]

var_data <- as.data.frame(lapply(var_data, function(x) as.numeric(as.character(x))))

# Step 3: Compute correlation matrix
corr_matrix <- cor(var_data, use = "pairwise.complete.obs")

# Step 4: Plot the correlation map
colnames(corr_matrix) <- all_labels[colnames(corr_matrix)]
rownames(corr_matrix) <- all_labels[rownames(corr_matrix)]
ggcorrplot(
  corr_matrix,
  method = "square",
  # type = "lower",
  lab = TRUE,
  lab_size = 2.5,
  colors = c("red", "white", "blue"),
  # title = "Correlation Map of Key Variables",
  show.legend = FALSE,
  outline.col = "gray90",
  show.diag = T,
  digits = 2
)

## Save to EndowDropbox, Figures, paper, LASSO_AND_RELATED

ggsave(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related", "AllVariableCorrelationMap.png"),
  width = 8, height = 8, dpi = 300
)

## Check irreperesentable condition

## Code below Doesn't work.

# # beta <- lm(gini_wealth_per_capita ~ rel_frank_capita + matt_x_vars, data = dat)
# beta <- lm(
#   reformulate(
#     c("rel_frank_capita", matt_x_vars),
#     response = "gini_wealth_per_capita"
#   ),
#   data = dat
# )
#
# beta <- beta$coefficients[2:length(beta$coefficients)]
# corrs <- corr_matrix[all_labels[c("rel_frank_capita", matt_x_vars)], all_labels[c("rel_frank_capita", matt_x_vars)]]
# sigma <- sqrt(as.numeric(crossprod(beta, corrs %*% beta) / 5)) ## SNR = 5
# devtools::install_github("donaldRwilliams/IRCcheck")
# library(IRCcheck)
# X <- MASS::mvrnorm(n = 500, mu = rep(0, nrow(corrs)), Sigma = corrs)
# 1 - irc_regression(X, 1:10)

# ----------------------------------------

# Function to compute correlation and standard error
cor_se <- function(x, y) {
  r <- cor(x, y, use = "complete.obs")
  n <- sum(complete.cases(x, y))
  se <- sqrt((1 - r^2) / (n - 2))
  tibble(correlation = r, se = se, n = n)
}
# Fisher z-transformation for confidence intervals
cor_ci <- function(x, y, conf.level = 0.95) {
  r <- cor(x, y, use = "complete.obs")
  n <- sum(complete.cases(x, y))
  if (n < 4) return(tibble(correlation = NA, lower = NA, upper = NA, n = n))
  z <- 0.5 * log((1 + r) / (1 - r))
  se_z <- 1 / sqrt(n - 3)
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha/2)
  z_lower <- z - z_crit * se_z
  z_upper <- z + z_crit * se_z
  r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  tibble(correlation = r, lower = r_lower, upper = r_upper, n = n)
}

make_results <- function(xvars, yvar_col) {
  res <- map_dfr(
    xvars,
    ~ cor_ci(dat_sub[[.x]], yvar_col) |> mutate(variable = .x)
  ) |>
    mutate(
      sig_col = case_when(
        lower > 0 ~ "pos_sig",
        upper < 0 ~ "neg_sig",
        TRUE      ~ "not_sig"
      )
    )
  res$variable_label <- all_labels[res$variable]
  res$variable_label <- ifelse(
    is.na(res$variable_label), res$variable, res$variable_label
  )
  res$variable_label <-
    factor(res$variable_label,
           levels = res$variable_label[order(res$correlation)])
  res
}

make_plot <- function(res, x_label) {
  ggplot(res, aes(y = variable_label, x = correlation, color = sig_col)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(
      values = c(pos_sig = "blue", neg_sig = "red", not_sig = "gray60"),
      guide  = "none"
    ) +
    theme_classic() +
    labs(y = NULL, x = paste0("Correlation with ", x_label)) +
    xlim(-1, 1)
}

# Correlations and Confidence Intervals with GINIs

vars_to_corr <- c(ec_vars, matt_x_vars)

for (yvar in y_vars) {
  y_col   <- dat_sub[[yvar]]
  x_label <- all_labels[yvar]

  # Plot with all vars_to_corr
  make_plot(make_results(vars_to_corr, y_col), x_label) |> print()
  ggsave(
    file.path(EndowDropbox, "Figures", "paper-1st-draft",
              paste0("CorrelationsWith", yvar, ".pdf")),
    width = 10, height = 4, dpi = 300
  )

  # Similar, but rel_frank_capita only
  make_plot(make_results(c("rel_frank_capita", matt_x_vars), y_col), x_label) |> print()
  ggsave(
    file.path(EndowDropbox, "Figures", "paper-1st-draft",
              paste0("CorrelationsWith", yvar, "_only_raaw.pdf")),
    width = 10, height = 3.5, dpi = 300
  )
}

# Correlations and Confidence Intervals with EC

for (ec_var in ec_vars) {
  y_col   <- dat_sub[[ec_var]]
  x_label <- all_labels[ec_var]

  make_plot(make_results(matt_x_vars, y_col), x_label) |> print()
  ggsave(
    file.path(EndowDropbox, "Figures", "paper-1st-draft",
              paste0("CorrelationsWith", ec_var, ".pdf")),
    width = 10, height = 4, dpi = 300
  )
}

# ----------------------------------------

# LASSO of GINI on other variables

matt_x_vars_sub <- ## may want to automate this at some point, to select if coefficient > 0.03 in LASSO
  c("v2xcl_prpty_value",
    #"how_common_price_shock",
    #"how_common_climate_shock",
    "shock_2plus",
    "v2xcl_dmove_value",
    "prop_primaryconnected_directed_gsize")

for (yvar in y_vars) {

  y <- dat_sub[, yvar]

  for (ecvar in ec_vars) {

    X_lasso <-
      as.matrix(dat_sub[, c(matt_x_vars, ecvar)])

    X_and_y <-
      cbind(X_lasso, y)

    X_lasso_complete <-
      X_lasso_complete <- X_lasso[complete.cases(X_and_y), ]

    y_complete <- y[complete.cases(X_and_y)]

    colnames(X_lasso_complete) <- all_labels[colnames(X_lasso_complete)]

    ## standardize X_lasso_complete and y_complete
    y_complete <- scale(y_complete)
    X_lasso_complete <- scale(X_lasso_complete)

    ## get the Puffer transform too
    svdX <- svd(X_lasso_complete)
    F_transform <- svdX$u %*% diag(1 / svdX$d) %*% t(svdX$u)
    X_tilde <- F_transform %*% X_lasso_complete
    y_tilde <- F_transform %*% y_complete

    lasso_fit <- glmnet(X_lasso_complete, y_complete, alpha = 1)
    coef_mat <- as.matrix(coef(lasso_fit))
    coef_df <- as.data.frame(t(coef_mat[-1, ]))  # drop intercept
    coef_df$lambda <- lasso_fit$lambda

    lasso_fit_puffer <- glmnet(X_tilde, y_tilde, alpha = 1)
    coef_mat_puffer <- as.matrix(coef(lasso_fit_puffer))
    coef_df_puffer <- as.data.frame(t(coef_mat_puffer[-1, ]))  # drop intercept
    coef_df_puffer$lambda <- lasso_fit_puffer$lambda

    coef_long <- coef_df %>%
      pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient")

    coef_long_puffer <- coef_df_puffer %>%
      pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient")

    coef_long$label <- all_labels[coef_long$variable]
    coef_long$label[is.na(coef_long$label)] <- coef_long$variable
    coef_long$log_lambda <- log(coef_long$lambda)

    coef_long_puffer$label <- all_labels[coef_long_puffer$variable]
    coef_long_puffer$label[is.na(coef_long_puffer$label)] <- coef_long_puffer$variable
    coef_long_puffer$log_lambda <- log(coef_long_puffer$lambda)

    label_points <- coef_long %>%
      filter(lambda == min(lambda)) %>%
      filter(abs(coefficient) > 0.03)

    label_points_puffer <- coef_long_puffer %>%
      filter(lambda == min(lambda)) %>%
      filter(abs(coefficient) > 0.03)

    label_points$log_lambda <- log(label_points$lambda)
    label_points_puffer$log_lambda <- log(label_points_puffer$lambda)

    min_log_lambda <- min(coef_long$log_lambda)
    max_log_lambda <- max(coef_long$log_lambda)

    min_log_lambda_puffer <- min(coef_long_puffer$log_lambda)
    max_log_lambda_puffer <- max(coef_long_puffer$log_lambda)

    ggplot(coef_long, aes(x = log_lambda, y = coefficient, group = label)) +
      geom_line(aes(color = label), linewidth = 0.6) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.7, linetype = "dotted") +
      geom_text_repel(
        data = label_points,
        aes(label = label, color = label),
        nudge_x = -0.4,
        hjust = 1,
        direction = "y",
        segment.color = NA,
        size = 3
      ) +
      scale_x_continuous(expand = expansion(mult = c(0.4, 0.05))) +
      labs(
        # title = "LASSO Coefficient Paths Including EC_capita.sum",
        x = "log(Lambda)",
        y = "Coefficient"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    ggsave(
      file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
                paste0("LASSO_", yvar, "_with_", ecvar, "and_xvars", ".pdf")),
      width = 10, height = 6, dpi = 300
    )

    ggplot(coef_long_puffer, aes(x = log_lambda, y = coefficient, group = label)) +
      geom_line(aes(color = label), linewidth = 0.6) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.7, linetype = "dotted") +
      geom_text_repel(
        data = label_points_puffer,
        aes(label = label, color = label),
        nudge_x = -0.4,
        hjust = 1,
        direction = "y",
        segment.color = NA,
        size = 3
      ) +
      scale_x_continuous(expand = expansion(mult = c(0.4, 0.05))) +
      labs(
        # title = "LASSO Coefficient Paths Including EC_capita.sum",
        x = "log(Lambda)",
        y = "Coefficient"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    ggsave(
      file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
                paste0("Puffer_LASSO_", yvar, "_with_", ecvar, "and_xvars", ".pdf")),
      width = 10, height = 6, dpi = 300
    )

    ## Do the random forest too

    rf_fit <- randomForest(x = X_lasso_complete, y = y_complete, ntree = 5000)
    colnames(rf_fit$importance) <- "Reduction in Residual Sum of Squares"
    png(filename = file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
                             paste0("RF_", yvar, "_with_", ecvar, "and_xvars", ".png")))
    # varImpPlot(rf_fit, main = NULL, xlab = "Reduction in Residual Sum of Squares")
    varImpPlot(rf_fit, main = NULL)
    dev.off()

    ## And the multiple regression:

    reg_data <- dat_sub[, c(yvar, ecvar, matt_x_vars)]
    #reg_data <- reg_data[complete.cases(reg_data), ]

    # Standardize all columns (mean=0, sd=1)
    reg_data_std <- as.data.frame(scale(reg_data))

    vars <- c(ecvar, matt_x_vars)

    formula_std_no_intercept <- as.formula(
      paste(paste0(yvar, " ~"), paste(matt_x_vars, collapse = " + "), "- 1")
    )

    formula_std_no_intercept2 <- as.formula(
      paste(paste0(yvar, " ~"), paste(vars, collapse = " + "), "- 1")
    )

    lm_fit <- lm(formula_std_no_intercept, data = reg_data_std)
    lm_fit2 <- lm(formula_std_no_intercept2, data = reg_data_std)

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "text",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[vars],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars.txt"))
    )

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[matt_x_vars],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars.tex"))
    )

    formula_std_no_intercept3 <- as.formula(
      paste(paste0(ecvar, " ~"), paste(matt_x_vars, collapse = " + "), "- 1")
    )

    lm_fit3 <- lm(formula_std_no_intercept3, data = reg_data_std)

    stargazer_silent(
      lm_fit3, lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = gsub("%", "\\\\%", all_labels[c(ecvar, yvar, yvar)]),
      covariate.labels = gsub("%", "\\\\%", all_labels[c(ecvar, matt_x_vars)]),
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Full_Set_Regression_", yvar, "_with_", ecvar, "_and_xvars.tex"))
    )

    vars <- c(ecvar, matt_x_vars)

    formula_std_no_intercept <- as.formula(
      paste(paste0(yvar, " ~"), paste(matt_x_vars, collapse = " + "), "- 1")
    )

    formula_std_no_intercept2 <- as.formula(
      paste(paste0(yvar, " ~"), paste(vars, collapse = " + "), "- 1")
    )

    lm_fit <- lm(formula_std_no_intercept, data = reg_data_std)
    lm_fit2 <- lm(formula_std_no_intercept2, data = reg_data_std)

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "text",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[vars],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars.txt"))
    )

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[matt_x_vars],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars.tex"))
    )

    formula_std_no_intercept3 <- as.formula(
      paste(paste0(ecvar, " ~"), paste(matt_x_vars, collapse = " + "), "- 1")
    )

    lm_fit3 <- lm(formula_std_no_intercept3, data = reg_data_std)

    stargazer_silent(
      lm_fit3, lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = gsub("%", "\\\\%", all_labels[c(ecvar, yvar, yvar)]),
      covariate.labels = gsub("%", "\\\\%", all_labels[c(ecvar, matt_x_vars)]),
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Full_Set_Regression_", yvar, "_with_", ecvar, "_and_xvars.tex"))
    )

    # -----------------------------------

    vars <- c(ecvar, matt_x_vars_sub)

    formula_std_no_intercept <- as.formula(
      paste(paste0(yvar, " ~"), paste(matt_x_vars_sub, collapse = " + "), "- 1")
    )

    formula_std_no_intercept2 <- as.formula(
      paste(paste0(yvar, " ~"), paste(vars, collapse = " + "), "- 1")
    )

    lm_fit <- lm(formula_std_no_intercept, data = reg_data_std)
    lm_fit2 <- lm(formula_std_no_intercept2, data = reg_data_std)

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "text",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[vars],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars_sub.txt"))
    )

    stargazer_silent(
      lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = all_labels[yvar],
      covariate.labels = all_labels[matt_x_vars_sub],
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Regression_", yvar, "_with_", ecvar, "_and_xvars_sub.tex"))
    )

    formula_std_no_intercept3 <- as.formula(
      paste(paste0(ecvar, " ~"), paste(matt_x_vars_sub, collapse = " + "), "- 1")
    )

    lm_fit3 <- lm(formula_std_no_intercept3, data = reg_data_std)

    stargazer_silent(
      lm_fit3, lm_fit, lm_fit2,
      type = "latex",
      # title = "Regression of Wealth GINI on Site-Level Variables",
      dep.var.labels = gsub("%", "\\\\%", all_labels[c(ecvar, yvar, yvar)]),
      covariate.labels = gsub("%", "\\\\%", all_labels[c(ecvar, matt_x_vars_sub)]),
      digits = 3,
      # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
      no.space = TRUE,
      font.size = "small",
      single.row = TRUE,
      out = file.path(EndowDropbox, "Tables",
                      paste0("Full_Set_Regression_", yvar, "_with_", ecvar, "_and_xvars_sub.tex"))
    )

    ## LASSO with no AR

    X_lasso <-
      as.matrix(dat_sub[dat_sub$site != "AR", c(matt_x_vars, ecvar)])

    y_no_ar <- y[dat_sub$site != "AR"]

    X_and_y <-
      cbind(X_lasso, y_no_ar)

    X_lasso_complete <-
      X_lasso_complete <- X_lasso[complete.cases(X_and_y), ]

    y_complete <- y_no_ar[complete.cases(X_and_y)]

    colnames(X_lasso_complete) <- all_labels[colnames(X_lasso_complete)]

    ## standardize X_lasso_complete and y_complete
    y_complete <- scale(y_complete)
    X_lasso_complete <- scale(X_lasso_complete)

    lasso_fit <- glmnet(X_lasso_complete, y_complete, alpha = 1)
    coef_mat <- as.matrix(coef(lasso_fit))
    coef_df <- as.data.frame(t(coef_mat[-1, ]))  # drop intercept
    coef_df$lambda <- lasso_fit$lambda

    coef_long <- coef_df %>%
      pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient")

    coef_long$label <- all_labels[coef_long$variable]
    coef_long$label[is.na(coef_long$label)] <- coef_long$variable
    coef_long$log_lambda <- log(coef_long$lambda)

    label_points <- coef_long %>%
      filter(lambda == min(lambda)) %>%
      filter(abs(coefficient) > 0.03)

    label_points$log_lambda <- log(label_points$lambda)

    min_log_lambda <- min(coef_long$log_lambda)
    max_log_lambda <- max(coef_long$log_lambda)

    ggplot(coef_long, aes(x = log_lambda, y = coefficient, group = label)) +
      geom_line(aes(color = label), linewidth = 0.6) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.7, linetype = "dotted") +
      geom_text_repel(
        data = label_points,
        aes(label = label, color = label),
        nudge_x = -0.4,
        hjust = 1,
        direction = "y",
        segment.color = NA,
        size = 3
      ) +
      scale_x_continuous(expand = expansion(mult = c(0.4, 0.05))) +
      labs(
        # title = "LASSO Coefficient Paths Including EC_capita.sum",
        x = "log(Lambda)",
        y = "Coefficient"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    ggsave(
      file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
                paste0("No_AR_LASSO_", yvar, "_with_", ecvar, "and_xvars", ".pdf")),
      width = 10, height = 6, dpi = 300
    )

  }

}

# ----------------------------------------



# LASSO of EC on other variables

for (yvar in ec_vars) {

  y <- dat_sub[, yvar]

  X_lasso <-
    as.matrix(dat_sub[, c(matt_x_vars)])

  X_and_y <-
    cbind(X_lasso, y)

  X_lasso_complete <-
    X_lasso_complete <- X_lasso[complete.cases(X_and_y), ]

  y_complete <- y[complete.cases(X_and_y)]

  colnames(X_lasso_complete) <- all_labels[colnames(X_lasso_complete)]

  lasso_fit <- glmnet(X_lasso_complete, y_complete, alpha = 1)
  coef_mat <- as.matrix(coef(lasso_fit))
  coef_df <- as.data.frame(t(coef_mat[-1, ]))  # drop intercept
  coef_df$lambda <- lasso_fit$lambda

  coef_long <- coef_df %>%
    pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient")

  coef_long$label <- all_labels[coef_long$variable]
  coef_long$label[is.na(coef_long$label)] <- coef_long$variable
  coef_long$log_lambda <- log(coef_long$lambda)

  label_points <- coef_long %>%
    filter(lambda == min(lambda)) %>%
    filter(abs(coefficient) > 0.05)

  label_points$log_lambda <- log(label_points$lambda)

  min_log_lambda <- min(coef_long$log_lambda)
  max_log_lambda <- max(coef_long$log_lambda)

  ggplot(coef_long, aes(x = log_lambda, y = coefficient, group = label)) +
    geom_line(aes(color = label), linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.7, linetype = "dotted") +
    geom_text_repel(
      data = label_points,
      aes(label = label, color = label),
      nudge_x = -0.4,
      hjust = 1,
      direction = "y",
      segment.color = NA,
      size = 3
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.4, 0.05))) +
    labs(
      # title = "LASSO Coefficient Paths Including EC_capita.sum",
      x = "log(Lambda)",
      y = "Coefficient"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey95"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  ggsave(
    file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
              paste0("LASSO_", yvar, "_with_", "xvars", ".png")),
    width = 8, height = 6, dpi = 300
  )

  ## Do the random forest too

  rf_fit <- randomForest(x = X_lasso_complete, y = y_complete, ntree = 5000)
  colnames(rf_fit$importance) <- "Reduction in Residual Sum of Squares"
  png(filename = file.path(EndowDropbox, "Figures", "paper-1st-draft", "LASSO-And-Related",
                           paste0("RF_", yvar, "_with_", "xvars", ".png")))
  # varImpPlot(rf_fit, main = NULL, xlab = "Reduction in Residual Sum of Squares")
  varImpPlot(rf_fit, main = NULL)
  dev.off()

  ## And the multiple regression:

  reg_data <- dat_sub[, c(yvar, matt_x_vars)]
  #reg_data <- reg_data[complete.cases(reg_data), ]

  # Standardize all columns (mean=0, sd=1)
  reg_data_std <- as.data.frame(scale(reg_data))

  formula_std_no_intercept <- as.formula(
    paste(paste0(yvar, " ~"), paste(matt_x_vars, collapse = " + "), "- 1")
  )

  lm_fit <- lm(formula_std_no_intercept, data = reg_data_std)

  stargazer_silent(
    lm_fit,
    type = "text",
    # title = "Regression of Wealth GINI on Site-Level Variables",
    dep.var.labels = all_labels[yvar],
    covariate.labels = all_labels[matt_x_vars],
    digits = 3,
    # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
    no.space = TRUE,
    font.size = "small",
    single.row = TRUE,
    out = file.path(EndowDropbox, "Tables",
                    paste0("Regression_", yvar, "_with_", "xvars.txt"))
  )

  stargazer_silent(
    lm_fit,
    type = "latex",
    # title = "Regression of Wealth GINI on Site-Level Variables",
    dep.var.labels = all_labels[yvar],
    covariate.labels = all_labels[matt_x_vars],
    digits = 3,
    # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
    no.space = TRUE,
    font.size = "small",
    single.row = TRUE,
    out = file.path(EndowDropbox, "Tables",
                    paste0("Regression_", yvar, "_with_", "xvars.tex"))
  )

  # Run on the reduced set of x variables:

  formula_std_no_intercept_sub <- as.formula(
    paste(paste0(yvar, " ~"), paste(matt_x_vars_sub, collapse = " + "), "- 1")
  )

  lm_fit_sub <- lm(formula_std_no_intercept_sub, data = reg_data_std)

  stargazer_silent(
    lm_fit_sub,
    type = "text",
    # title = "Regression of Wealth GINI on Site-Level Variables",
    dep.var.labels = all_labels[yvar],
    covariate.labels = all_labels[matt_x_vars],
    digits = 3,
    # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
    no.space = TRUE,
    font.size = "small",
    single.row = TRUE,
    out = file.path(EndowDropbox, "Tables",
                    paste0("Regression_", yvar, "_with_", "xvars.txt"))
  )

  stargazer_silent(
    lm_fit_sub,
    type = "latex",
    # title = "Regression of Wealth GINI on Site-Level Variables",
    dep.var.labels = all_labels[yvar],
    covariate.labels = all_labels[matt_x_vars],
    digits = 3,
    # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
    no.space = TRUE,
    font.size = "small",
    single.row = TRUE,
    out = file.path(EndowDropbox, "Tables",
                    paste0("Regression_", yvar, "_with_", "xvars_sub.tex"))
  )

}

# ==============================================================================

# The tables Matt requested
## NOTE: Commenting out, as above now remove some of these variables, like how_common_price_shock

reg_data <- dat_sub[, c("gini_wealth_per_capita", "rel_frank_capita", "n_su", matt_x_vars)]
rownames(reg_data) <- dat_sub$site_name
#reg_data <- reg_data[complete.cases(reg_data), ]
reg_data_std <- as.data.frame(scale(reg_data))

# lm_fit1 <- lm(gini_wealth_per_capita ~
#                 rel_frank_capita +
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value - 1,
#               data = reg_data_std)

# lm_fit2 <- lm(gini_wealth_per_capita ~
#                 rel_frank_capita +
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock - 1,
#               data = reg_data_std)

# lm_fit3 <- lm(gini_wealth_per_capita ~
#                 rel_frank_capita +
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock +
#                 NY.GDP.PCAP.KD_value +
#                 SI.POV.GINI_value +
#                 strong_norms_on_production +
#                 v2xcl_dmove_value +
#                 v2x_corr_value +
#                 literacy +
#                 prop_cash_exchange - 1,
#               data = reg_data_std)

# lm_fit4 <- lm(rel_frank_capita ~
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value - 1,
#               data = reg_data_std)

# lm_fit5 <- lm(rel_frank_capita ~
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock - 1,
#               data = reg_data_std)

# lm_fit6 <- lm(rel_frank_capita ~
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock +
#                 NY.GDP.PCAP.KD_value +
#                 SI.POV.GINI_value +
#                 strong_norms_on_production +
#                 v2xcl_dmove_value +
#                 v2x_corr_value +
#                 literacy +
#                 prop_cash_exchange - 1,
#               data = reg_data_std)

# # nightlights_reg <-
# #   lm(nightlight ~
# #        how_common_price_shock +
# #        prop_time_salary +
# #        v2xcl_prpty_value +
# #        prop_primaryconnected_directed_gsize +
# #        how_common_climate_shock +
# #        NY.GDP.PCAP.KD_value +
# #        SI.POV.GINI_value +
# #        strong_norms_on_production +
# #        v2xcl_dmove_value +
# #        v2x_corr_value +
# #        literacy +
# #        prop_cash_exchange +
# #        #CSP_gHM_250_mean +
# #        hii_250_mean +
# #        MAP_accessibility_1000_mean +
# #        pop_density +
# #        precip_all +
# #        bedd - 1,
# #      data = reg_data_std)

# matt_xvars_order <-
#   c("how_common_price_shock",
#     "prop_time_salary",
#     "v2xcl_prpty_value",
#     "prop_primaryconnected_directed_gsize",
#     "how_common_climate_shock",
#     "NY.GDP.PCAP.KD_value",
#     "SI.POV.GINI_value",
#     "strong_norms_on_production",
#     "v2xcl_dmove_value",
#     "v2x_corr_value",
#     "literacy",
#     "prop_cash_exchange")

# labels_order <-
#   c(all_labels["rel_frank_capita"], all_labels[matt_xvars_order])

# stargazer(
#   lm_fit1, lm_fit2, lm_fit3, lm_fit4, lm_fit5, lm_fit6,
#   type = "text",
#   # title = "Regression of Wealth GINI on Site-Level Variables",
#   dep.var.labels = all_labels[c("gini_wealth_per_capita", "rel_frank_capita")],
#   covariate.labels = labels_order,
#   digits = 3,
#   # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
#   no.space = TRUE,
#   single.row = TRUE,
#   out = file.path(EndowDropbox, "Tables",
#                   "Regression_Lasso_vars_for_matt.txt")
# )

# stargazer(
#   lm_fit1, lm_fit2, lm_fit3, lm_fit4, lm_fit5, lm_fit6,
#   type = "latex",
#   # title = "Regression of Wealth GINI on Site-Level Variables",
#   dep.var.labels = gsub("%", "\\\\%", all_labels[c("gini_wealth_per_capita", "rel_frank_capita")]),
#   covariate.labels = gsub("%", "\\\\%", labels_order),
#   digits = 3,
#   # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
#   no.space = TRUE,
#   single.row = TRUE,
#   out = file.path(EndowDropbox, "Tables",
#                   "Regression_Lasso_vars_for_matt.tex")
# )

# # Regressions Matt requested, with full variable set.

# reg_data <- dat_sub[, c("gini_wealth_per_capita", "rel_frank_capita", matt_x_vars)]
# rownames(reg_data) <- dat_sub$site_name
# #reg_data <- reg_data[complete.cases(reg_data), ]
# reg_data_std <- as.data.frame(scale(reg_data))

# # lm_fit1 <- lm(gini_wealth_per_capita ~
# #                 rel_frank_capita +
# #                 how_common_price_shock +
# #                 prop_time_salary +
# #                 v2xcl_prpty_value - 1,
# #               data = reg_data_std)

# # lm_fit2 <- lm(gini_wealth_per_capita ~
# #                 rel_frank_capita +
# #                 how_common_price_shock +
# #                 prop_time_salary +
# #                 v2xcl_prpty_value +
# #                 prop_primaryconnected_directed_gsize +
# #                 how_common_climate_shock - 1,
# #               data = reg_data_std)

# lm_fit3 <- lm(gini_wealth_per_capita ~
#                 rel_frank_capita +
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock +
#                 NY.GDP.PCAP.KD_value +
#                 SI.POV.GINI_value +
#                 strong_norms_on_production +
#                 v2xcl_dmove_value +
#                 v2x_corr_value +
#                 literacy +
#                 prop_cash_exchange +
#                 #CSP_gHM_250_mean +
#                 hii_250_mean +
#                 MAP_accessibility_1000_mean +
#                 #nightlight +
#                 pop_density +
#                 precip_all +
#                 bedd - 1,
#               data = reg_data_std)

# lm_fit6 <- lm(rel_frank_capita ~
#                 how_common_price_shock +
#                 prop_time_salary +
#                 v2xcl_prpty_value +
#                 prop_primaryconnected_directed_gsize +
#                 how_common_climate_shock +
#                 NY.GDP.PCAP.KD_value +
#                 SI.POV.GINI_value +
#                 strong_norms_on_production +
#                 v2xcl_dmove_value +
#                 v2x_corr_value +
#                 literacy +
#                 prop_cash_exchange +
#                 #CSP_gHM_250_mean +
#                 hii_250_mean +
#                 MAP_accessibility_1000_mean +
#                 #nightlight +
#                 pop_density +
#                 precip_all +
#                 bedd - 1,
#               data = reg_data_std)

# matt_xvars_order <-
#   c("how_common_price_shock",
#     "prop_time_salary",
#     "v2xcl_prpty_value",
#     "prop_primaryconnected_directed_gsize",
#     "how_common_climate_shock",
#     "NY.GDP.PCAP.KD_value",
#     "SI.POV.GINI_value",
#     "strong_norms_on_production",
#     "v2xcl_dmove_value",
#     "v2x_corr_value",
#     "literacy",
#     "prop_cash_exchange",
#     #"CSP_gHM_250_mean",
#     "hii_250_mean",
#     "MAP_accessibility_1000_mean",
#     #"nightlight",
#     "pop_density",
#     "precip_all",
#     "bedd")

# labels_order <-
#   c(all_labels["rel_frank_capita"], all_labels[matt_xvars_order])

# stargazer(
#   lm_fit3, lm_fit6,
#   type = "text",
#   # title = "Regression of Wealth GINI on Site-Level Variables",
#   dep.var.labels = all_labels[c("gini_wealth_per_capita", "rel_frank_capita")],
#   covariate.labels = labels_order,
#   digits = 3,
#   # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
#   no.space = TRUE,
#   single.row = TRUE,
#   out = file.path(EndowDropbox, "Tables",
#                   "Other_Regression_Lasso_vars_for_matt.txt")
# )

# stargazer(
#   lm_fit1, lm_fit2, lm_fit3, lm_fit4, lm_fit5, lm_fit6,
#   type = "latex",
#   # title = "Regression of Wealth GINI on Site-Level Variables",
#   dep.var.labels = gsub("%", "\\\\%", all_labels[c("gini_wealth_per_capita", "rel_frank_capita")]),
#   covariate.labels = gsub("%", "\\\\%", labels_order),
#   digits = 3,
#   # report = "vcstp",          # show coefficients, std errors, t-stats, p-values
#   no.space = TRUE,
#   single.row = TRUE,
#   out = file.path(EndowDropbox, "Tables",
#                   "Regression_Lasso_vars_for_matt.tex")
# )

# Updated stepwise set: go off of bivariate and LASSO for EC measure

lm_fit1 <- lm(
  gini_wealth_per_capita ~
    rel_frank_capita +
    prop_time_salary +
    v2xcl_prpty_value +
    hii_250_mean -
    #v2xcl_dmove_value -
    1,
  data = reg_data_std
)

lm_fit2 <- lm(
  gini_wealth_per_capita ~
    rel_frank_capita +
    prop_time_salary +
    v2xcl_prpty_value +
    #v2xcl_dmove_value +
    hii_250_mean +
    bedd +
    prop_primaryconnected_directed_gsize +
    strong_norms_on_production -
    1,
  data = reg_data_std
)

lm_fit3 <- lm(
  gini_wealth_per_capita ~
    rel_frank_capita +
    prop_time_salary +
    v2xcl_prpty_value +
    hii_250_mean +
    bedd +
    prop_primaryconnected_directed_gsize +
    strong_norms_on_production +
    NY.GDP.PCAP.KD_value +
    shock_2plus +
    v2xcl_dmove_value +
    v2x_corr_value +
    prop_cash_exchange -
    1,
  data = reg_data_std
)

## Check consequence of adding site size (n_su)
lm_fit1_nsu <- update(lm_fit1, . ~ . + n_su)
lm_fit2_nsu <- update(lm_fit2, . ~ . + n_su)
lm_fit3_nsu <- update(lm_fit3, . ~ . + n_su)

gini_label_order <- c(
  "rel_frank_capita",
  "prop_time_salary",
  "v2xcl_prpty_value",
  "hii_250_mean",
  "bedd",
  "prop_primaryconnected_directed_gsize",
  "strong_norms_on_production",
  "NY.GDP.PCAP.KD_value",
  "shock_2plus",
  "v2xcl_dmove_value",
  "v2x_corr_value",
  "prop_cash_exchange",
  "n_su"
)

stargazer_silent(
  lm_fit1,
  lm_fit2,
  lm_fit3,
  type = "latex",
  no.space = TRUE,
  font.size = "small",
  single.row = TRUE,
  title = "Stepwise regression results predicting the Gini of wealth per capita.",
  notes = NULL,
  dep.var.labels = gsub("%", "\\\\%", all_labels[c("gini_wealth_per_capita")]),
  covariate.labels = gsub("%", "\\\\%", all_labels[gini_label_order]),
  digits = 3,
  label = "tab:stepwise_regression_lasso",
  out = file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso.tex")
)

add_stargazer_note(
  file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso.tex"),
  "\\textit{Note:} Variables first included based on significant bivariate correlation, then those highlighted by the LASSO, and finally the full set of variables under consideration. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$. Standard errors in parentheses."
)


stargazer_silent(
  lm_fit1_nsu,
  lm_fit2_nsu,
  lm_fit3_nsu,
  type = "latex",
  no.space = TRUE,
  font.size = "small",
  single.row = TRUE,
  title = "Stepwise regression results predicting the Gini of wealth per capita.",
  notes = NULL,
  dep.var.labels = gsub("%", "\\\\%", all_labels[c("gini_wealth_per_capita")]),
  covariate.labels = gsub("%", "\\\\%", all_labels[gini_label_order]),
  digits = 3,
  label = "tab:stepwise_regression_lasso_with_nsu",
  out = file.path(
    EndowDropbox,
    "Tables",
    "Stepwise_Regression_from_Lasso_with_nsu.tex"
  )
)

add_stargazer_note(
  file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso_with_nsu.tex"),
  "\\textit{Note:} Variables first included based on significant bivariate correlation, then those highlighted by the LASSO, and finally the full set of variables under consideration. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$. Standard errors in parentheses."
)

## Predict rel_frank_capita
lm_fit1_ec <- update(lm_fit1, rel_frank_capita ~ . - rel_frank_capita)
lm_fit2_ec <- update(lm_fit2, rel_frank_capita ~ . - rel_frank_capita)
lm_fit3_ec <- update(lm_fit3, rel_frank_capita ~ . - rel_frank_capita)
lm_fit1_ecsu <- update(lm_fit1, rel_frank_capita ~ . - rel_frank_capita + n_su)
lm_fit2_ecsu <- update(lm_fit2, rel_frank_capita ~ . - rel_frank_capita + n_su)
lm_fit3_ecsu <- update(lm_fit3, rel_frank_capita ~ . - rel_frank_capita + n_su)

stargazer_silent(
  lm_fit1_ec,
  lm_fit2_ec,
  lm_fit3_ec,
  type = "latex",
  no.space = TRUE,
  font.size = "small",
  single.row = TRUE,
  title = "Stepwise regression results predicting Relative Average Alter Wealth.",
  notes = NULL,
  dep.var.labels = gsub("%", "\\\\%", all_labels[c("rel_frank_capita")]),
  covariate.labels = gsub("%", "\\\\%", all_labels[gini_label_order[-1]]),
  digits = 3,
  label = "tab:stepwise_regression_lasso_ec",
  out = file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso_EC.tex")
)

add_stargazer_note(
  file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso_EC.tex"),
  "\\textit{Note:} Variables included in the same order as done when predicting the Gini of wealth per capita. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$. Standard errors in parentheses."
)

## Check robustness of results to outliers using Cook's D and DFBETAS

models_list <- list(lm_fit1, lm_fit2, lm_fit3)
model_names <- 1:3 #paste(rep(c("Wealth per Capita Gini", "AAW of Poor"), each = 3), 1:3)
n <- length(lm_fit1$fitted.values)

# Collect all diagnostics
all_diagnostics <- lapply(seq_along(models_list), function(i) {
  fit <- models_list[[i]]

  # Cook's D
  cooksd <- cooks.distance(fit)
  influential_ids <- which(cooksd > 4/n)
  model_full <- fit
  model_reduced <- update(fit, subset = -influential_ids)
  cbind(
    Full = coef(model_full),
    Reduced = coef(model_reduced),
    Pct_Change = (coef(model_reduced) - coef(model_full))/coef(model_full) * 100
  )

  cook_df <- data.frame(
    obs = names(model_full$fitted.values),
    cooksd = cooksd,
    model = model_names[i],
    stringsAsFactors = FALSE
  )

  # DFBETAS - convert to long format
  dfb <- dfbetas(fit)
  rownames(dfb) <- names(model_full$fitted.values)
  dfb_df <- as.data.frame(dfb)
  dfb_df$obs <- names(model_full$fitted.values)
  dfb_df$model <- model_names[i]

  dfb_long <- dfb_df %>%
    pivot_longer(cols = -c(obs, model),
                 names_to = "coefficient",
                 values_to = "dfbetas")

  list(cook = cook_df, dfbetas = dfb_long)
})

# Combine
cook_data <- do.call(rbind, lapply(all_diagnostics, `[[`, "cook"))
dfbetas_long <- do.call(rbind, lapply(all_diagnostics, `[[`, "dfbetas"))

# Plot and Compare
model_comparisons <- list()
full_models <- list()
reduced_models <- list()
for (i in seq_along(models_list)) {
  fit <- models_list[[i]]
  mod_name <- model_names[i]
  threshold <- 2/sqrt(n)

  # Identify influential cases
  cooksd <- cooks.distance(fit)
  influential_ids <- which(cooksd > 4/n)

  if(length(influential_ids) > 0) {

    # Refit without influential cases
    model_full <- fit
    full_models[[i]] <- model_full
    model_reduced <- update(fit, subset = -influential_ids)
    reduced_models[[i]] <- model_reduced

    pvals_full <- summary(model_full)$coefficients[, "Pr(>|t|)"]
    pvals_reduced <- summary(model_reduced)$coefficients[, "Pr(>|t|)"]

    # Compare coefficients
    comparison <- cbind(
      Full = coef(model_full),
      Full_pval = pvals_full,
      Reduced = coef(model_reduced),
      Reduced_pval = pvals_reduced,
      Pct_Change = round((coef(model_reduced) - coef(model_full))/coef(model_full) * 100, 2)
    )

  }

  if(length(influential_ids) > 0) {
    model_comparisons[[mod_name]] <- comparison
  }

  # Cook's D
  p_cook <- cook_data %>%
    filter(model == mod_name) %>%
    arrange(desc(cooksd)) %>%
    mutate(obs = factor(obs, levels = obs)) %>%
    ggplot(aes(x = obs, y = cooksd)) +
    geom_segment(aes(xend = obs, yend = 0)) +
    geom_hline(yintercept = 4/n, linetype = "dashed", color = "red") +
    labs(title = mod_name, y = "Cook's D", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

  # DFBETAS
  p_dfbetas <- dfbetas_long %>%
    filter(model == mod_name) %>%
    mutate(obs = reorder_within(obs, dfbetas, coefficient)) %>%
    ggplot(aes(x = obs, y = dfbetas)) +
    geom_segment(aes(xend = obs, yend = 0)) +
    geom_hline(yintercept = c(-threshold, threshold),
               linetype = "dashed", color = "red") +
    facet_wrap(~ coefficient, nrow = 2, scales = "free_x") +
    scale_x_reordered() +
    labs(y = "DFBETAS", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))

  # Combine
  print(p_cook / p_dfbetas + plot_layout(heights = c(1, 2)))
}
stargazer_silent(c(rbind(full_models, reduced_models)), type = "text")

stargazer_silent(
  c(rbind(full_models, reduced_models)),
  type = "latex",
  no.space = TRUE,
  font.size = "small",
  single.row = TRUE,
  title = "Stepwise regression results predicting the Gini of wealth per capita, with versions using the full set of sites and then with sites removed that have high Cook's D.",
  notes = NULL,
  dep.var.labels = gsub("%", "\\\\%", all_labels[c("gini_wealth_per_capita")]),
  covariate.labels = gsub("%", "\\\\%", all_labels[gini_label_order]),
  out = file.path(
    EndowDropbox,
    "Tables",
    "Stepwise_Regression_from_Lasso_following_cooksd.tex"
  )
)

add_stargazer_note(
  file.path(EndowDropbox, "Tables", "Stepwise_Regression_from_Lasso_following_cooksd.tex"),
  "\\textit{Note:} Variables first included based on significant bivariate correlation, then those highlighted by the LASSO, and finally the full set of variables under consideration. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$. Standard errors in parentheses."
)