## Relative Frank vs Wealth Gini — Multi-panel Scatter Plots
##
## Produces multi-panel figures for the cross-site correlation between
## the Friend Rank Ratio (AAW: average alter wealth of the poor relative to
## the rich) and the wealth Gini, repeated across four wealth normalisations:
## per-capita, absolute, per-adult, and size-adjusted.
##
## Equivalent figures are also produced for: in-degree Gini, out-degree Gini,
## Economic Connectedness (EC), and Relative Connectedness (RC), and reversed
## versions for AAW, EC, and RC.
##
## Also produces tables with the various site-level variables.
##
## Appendix figures (cross-site/):
##   GiniWealthFriendRankScatterAll.pdf
##   GiniWealthInDegreeScatterAll.pdf
##   GiniWealthOutDegreeScatterAll.pdf
##   GiniWealthECScatterAll.pdf
##   GiniWealthRCScatterAll.pdf
##   GiniWealthFriendRankScatterAll_rev.pdf
##   GiniWealthECScatterAll_rev.pdf
##   GiniWealthRCScatterAll_rev.pdf
##
## Appendix tables (tables/DescriptiveStats):
##   deg_gini_site_vars_table.tex
##   ec_site_vars_table.tex
##   raaw_site_vars_table.tex
##   rc_site_vars_table.tex


library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(ggstance)
library(purrr)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")
dat  <- read.csv(file.path(path, "DataForLASSO.csv"))

# Derived variables -----------------------------------------------------------

dat <- dat %>% mutate(
  # Outgoing friend-rank ratios
  rel_frank_capita        = avg_frank_low_capita.sum        / avg_frank_high_capita.sum,
  rel_frank_absolute      = avg_frank_low_absolute.sum      / avg_frank_high_absolute.sum,
  rel_frank_adult         = avg_frank_low_adult.sum         / avg_frank_high_adult.sum,
  rel_frank_size_adjusted = avg_frank_low_size_adjusted.sum / avg_frank_high_size_adjusted.sum,
  # Outgoing relative connectedness
  RC_capita        = EC_capita.sum        / EC_capita_high.sum,
  RC_absolute      = EC_absolute.sum      / EC_absolute_high.sum,
  RC_adult         = EC.sum               / EC_high.sum,
  RC_size_adjusted = EC_size_adjusted.sum / EC_size_adjusted_high.sum,
  # Incoming friend-rank ratios
  rel_frank_capita_rev        = avg_frank_low_capita.rev_sum        / avg_frank_high_capita.rev_sum,
  rel_frank_absolute_rev      = avg_frank_low_absolute.rev_sum      / avg_frank_high_absolute.rev_sum,
  rel_frank_adult_rev         = avg_frank_low_adult.rev_sum         / avg_frank_high_adult.rev_sum,
  rel_frank_size_adjusted_rev = avg_frank_low_size_adjusted.rev_sum / avg_frank_high_size_adjusted.rev_sum,
  # Incoming relative connectedness
  RC_capita_rev        = EC_capita.rev_sum        / EC_capita_high.rev_sum,
  RC_absolute_rev      = EC_absolute.rev_sum      / EC_absolute_high.rev_sum,
  RC_adult_rev         = EC.rev_sum               / EC_high.rev_sum,
  RC_size_adjusted_rev = EC_size_adjusted.rev_sum / EC_size_adjusted_high.rev_sum
)

# ==============================================================================
# Shared helpers
# ==============================================================================

cor_fun <- function(x, y) {
  r  <- cor(x, y, use = "complete.obs")
  n  <- sum(complete.cases(x, y))
  se <- sqrt((1 - r^2)^2 / (n - 2))
  list(corr = r, se = se)
}

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) "p < 0.0001" else paste0("p = ", formatC(p, digits = digits, format = "f"))
}

# Fixed across all figures
WEIGHTS   <- c("capita", "absolute", "adult", "size_adjusted")
Y_VARS    <- c(capita = "gini_wealth_per_capita", absolute = "gini_wealth",
               adult  = "gini_wealth_per_adult",  size_adjusted = "gini_size_adjusted_wealth")
Y_LABELS  <- c(capita = "Wealth Gini (Per Capita)",    absolute = "Wealth Gini (Absolute)",
               adult  = "Wealth Gini (Per Adult)",     size_adjusted = "Wealth Gini (Size-Adjusted)")
COLORS    <- c(capita = "#377EB8", absolute = "#E41A1C",
               adult  = "#4DAF4A", size_adjusted = "#984EA3")
CLR_LABS  <- c(capita = "Per Capita", absolute = "Absolute",
               adult  = "Per Adult",  size_adjusted = "Size-Adjusted")

# ==============================================================================
# Core figure function
# ==============================================================================

#' Build and save a 5-panel scatter figure.
#'
#' @param dat         Data frame.
#' @param x_vars      Named character vector (keys: capita/absolute/adult/size_adjusted)
#'                    giving the x-variable column name for each panel.
#' @param x_labels    Named character vector of x-axis labels, same keys.
#' @param measure_label  String shown on the correlation summary panel (panel 1).
#' @param out_file    PDF filename (basename only).
#' @param figpath     Directory to write the PDF into.

make_gini_figure <- function(dat, x_vars, x_labels, measure_label, out_file, figpath) {

  # --- correlation summary panel ---------------------------------------------
  corr_df <- map_dfr(WEIGHTS, function(w) {
    res <- cor_fun(dat[[x_vars[w]]], dat[[Y_VARS[w]]])
    tibble(weight = w, corr = res$corr, se = res$se)
  }) %>%
    mutate(
      lb     = pmax(corr - 1.96 * se, -1),
      ub     = pmin(corr + 1.96 * se,  1),
      type   = measure_label,
      weight = factor(weight, levels = c("size_adjusted", "adult", "absolute", "capita"))
    )

  pos    <- ggstance::position_dodgev(height = -0.5)
  p_corr <- corr_df %>%
    ggplot(aes(x = corr, y = type, color = weight)) +
    geom_vline(xintercept = 0, alpha = 0.5, linewidth = 0.5, color = "black") +
    geom_linerange(aes(xmin = lb, xmax = ub), position = pos) +
    geom_point(position = pos, size = 3) +
    scale_color_manual(values = COLORS, labels = CLR_LABS, breaks = WEIGHTS) +
    guides(color = guide_legend(nrow = 4)) +
    scale_y_discrete("", labels = measure_label) +
    scale_x_continuous("Correlation with Wealth Gini", limits = c(-1, 1)) +
    theme_classic() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.position.inside = c(0.06, 0.2),
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.2)),
      axis.ticks.x = element_blank()
    ) +
    coord_flip()

  # --- scatter panels (one per weight) ---------------------------------------
  make_scatter <- function(w, x_range, y_range) {
    xv <- x_vars[w]
    yv <- Y_VARS[w]
    xl <- x_labels[w]
    yl <- Y_LABELS[w]
    ct <- cor.test(dat[[xv]], dat[[yv]])
    ggplot(dat, aes(x = .data[[xv]], y = .data[[yv]])) +
      #geom_point(color = "gray50") +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = COLORS[w]) +
      #geom_text_repel(
      #  aes(label = site_name),
      #  size = 3,
      #  max.overlaps = 10,
      #  color = "gray50"
      #) +
      geom_label(
        aes(label = site_name),
        family = "roboto",
        size = 1.5,
        alpha = 0.5,
        linewidth = 0.25,
        label.padding = unit(0.3, "lines"),
        label.r = unit(0.6, "lines"),
        color = "gray50"
      ) +
      coord_cartesian(xlim = x_range, ylim = y_range) +
      theme_classic() +
      labs(
        x = xl,
        y = yl,
        subtitle = paste0(
          "Correlation: ",
          sprintf("%.2f", ct$estimate),
          ", ",
          format_pval(ct$p.value)
        )
      )
  }

  # compute shared ranges across all four panels' x- and y-columns, before mapping
  x_range <- range(unlist(dat[, x_vars[WEIGHTS]]), na.rm = TRUE)
  y_range <- range(unlist(dat[, Y_VARS[WEIGHTS]]), na.rm = TRUE)

  scatter_panels <- map(
    setNames(WEIGHTS, WEIGHTS),
    make_scatter,
    x_range = x_range,
    y_range = y_range
  )

  # --- assemble and save -----------------------------------------------------
  pdf(file.path(figpath, out_file), height = 10, width = 10)
  gridExtra::grid.arrange(
    grobs  = list(p_corr,
                  scatter_panels$capita,   scatter_panels$absolute,
                  scatter_panels$adult,    scatter_panels$size_adjusted),
    widths  = c(1, 1),
    heights = c(1, 1, 0.8),
    layout_matrix = rbind(c(2, 3), c(4, 5), c(1, 1))
  )
  dev.off()
}

# ==============================================================================
# Figure configs
# ==============================================================================

figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site")

figures <- list(

  FriendRank = list(
    x_vars   = c(capita = "rel_frank_capita", absolute = "rel_frank_absolute",
                 adult  = "rel_frank_adult",  size_adjusted = "rel_frank_size_adjusted"),
    x_labels = c(capita = "Relative Average Alter Wealth (Per Capita)",    absolute = "Relative Average Alter Wealth (Absolute)",
                 adult  = "Relative Average Alter Wealth (Per Adult)",     size_adjusted = "Relative Average Alter Wealth (Size-Adjusted)"),
    measure  = "Relative Average Alter Wealth (of supporters)",
    file     = "GiniWealthFriendRankScatterAll.pdf"
  ),

  FriendRank_rev = list(
    x_vars   = c(capita = "rel_frank_capita_rev", absolute = "rel_frank_absolute_rev",
                 adult  = "rel_frank_adult_rev",  size_adjusted = "rel_frank_size_adjusted_rev"),
    x_labels = c(capita = "Relative Average Alter Wealth (Per Capita)",    absolute = "Relative Average Alter Wealth (Absolute)",
                 adult  = "Relative Average Alter Wealth (Per Adult)",     size_adjusted = "Relative Average Alter Wealth (Size-Adjusted)"),
    measure  = "Relative Average Alter Wealth (of supportees)",
    file     = "GiniWealthFriendRankScatterAll_rev.pdf"
  ),

  InDegreeGini = list(
    x_vars   = c(capita = "ideg_pc_gini", absolute = "ideg_gini",
                 adult  = "ideg_pa_gini", size_adjusted = "ideg_size_adjusted_gini"),
    x_labels = c(capita = "Support Provisioning Gini (Per Capita)",    absolute = "Support Provisioning Gini (Absolute)",
                 adult  = "Support Provisioning Gini (Per Adult)",     size_adjusted = "Support Provisioning Gini (Size-Adjusted)"),
    measure  = "Support Provisioning Gini",
    file     = "GiniWealthInDegreeScatterAll.pdf"
  ),

  OutDegreeGini = list(
    x_vars   = c(capita = "odeg_pc_gini", absolute = "odeg_gini",
                 adult  = "odeg_pa_gini", size_adjusted = "odeg_size_adjusted_gini"),
    x_labels = c(capita = "Support Access Gini (Per Capita)",    absolute = "Support Access Gini (Absolute)",
                 adult  = "Support Access Gini (Per Adult)",     size_adjusted = "Support Access Gini (Size-Adjusted)"),
    measure  = "Support Access Gini",
    file     = "GiniWealthOutDegreeScatterAll.pdf"
  ),

  EC = list(
    x_vars   = c(capita = "EC_capita.sum", absolute = "EC_absolute.sum",
                 adult  = "EC.sum",        size_adjusted = "EC_size_adjusted.sum"),
    x_labels = c(capita = "Economic Connectedness (Per Capita)",    absolute = "Economic Connectedness (Absolute)",
                 adult  = "Economic Connectedness (Per Adult)",     size_adjusted = "Economic Connectedness (Size-Adjusted)"),
    measure  = "Economic Connectedness (to supporters)",
    file     = "GiniWealthECScatterAll.pdf"
  ),

  EC_rev = list(
    x_vars   = c(capita = "EC_capita.rev_sum", absolute = "EC_absolute.rev_sum",
                 adult  = "EC.rev_sum",        size_adjusted = "EC_size_adjusted.rev_sum"),
    x_labels = c(capita = "Economic Connectedness (Per Capita)",    absolute = "Economic Connectedness (Absolute)",
                 adult  = "Economic Connectedness (Per Adult)",     size_adjusted = "Economic Connectedness (Size-Adjusted)"),
    measure  = "Economic Connectedness (to supportees)",
    file     = "GiniWealthECScatterAll_rev.pdf"
  ),

  RC = list(
    x_vars   = c(capita = "RC_capita", absolute = "RC_absolute",
                 adult  = "RC_adult",  size_adjusted = "RC_size_adjusted"),
    x_labels = c(capita = "Relative Connectedness (Per Capita)",    absolute = "Relative Connectedness (Absolute)",
                 adult  = "Relative Connectedness (Per Adult)",     size_adjusted = "Relative Connectedness (Size-Adjusted)"),
    measure  = "Relative Connectedness (to supporters)",
    file     = "GiniWealthRCScatterAll.pdf"
  ),

  RC_rev = list(
    x_vars   = c(capita = "RC_capita_rev", absolute = "RC_absolute_rev",
                 adult  = "RC_adult_rev",  size_adjusted = "RC_size_adjusted_rev"),
    x_labels = c(capita = "Relative Connectedness (Per Capita)",    absolute = "Relative Connectedness (Absolute)",
                 adult  = "Relative Connectedness (Per Adult)",     size_adjusted = "Relative Connectedness (Size-Adjusted)"),
    measure  = "Relative Connectedness (to supportees)",
    file     = "GiniWealthRCScatterAll_rev.pdf"
  )
)

# ==============================================================================
# Generate all figures
# ==============================================================================

walk(figures, function(cfg) {
  make_gini_figure(
    dat           = dat,
    x_vars        = cfg$x_vars,
    x_labels      = cfg$x_labels,
    measure_label = cfg$measure,
    out_file      = cfg$file,
    figpath       = figpath
  )
})

# ==============================================================================
# Site-level LaTeX tables of variables with various weightings
# ==============================================================================

# reorder
TABLE_WEIGHTS <- c("absolute", "capita", "adult", "size_adjusted")

# Header-row label for each measure
figures$FriendRank$header_prefix     <- "RAAW (of supporters)"
figures$FriendRank_rev$header_prefix <- "RAAW (of supportees)"

figures$InDegreeGini$header_prefix   <- "Support Provisioning Gini"
figures$OutDegreeGini$header_prefix  <- "Support Access Gini"

figures$EC$header_prefix             <- "EC (to supporters)"
figures$EC_rev$header_prefix         <- "EC (to supportees)"

figures$RC$header_prefix             <- "RC (to supporters)"
figures$RC_rev$header_prefix         <- "RC (to supportees)"

tabpath <- file.path(EndowDropbox, "Tables", "DescriptiveStats")

# Pairs to combine into single tables, in display order (left group, right group)
table_pairs <- list(
  list(a = figures$OutDegreeGini, b = figures$InDegreeGini,
       short_label = "deg_gini",   caption = "Site-Level Access and Provisioning Support Gini Variables"),
  list(a = figures$EC,            b = figures$EC_rev,
       short_label = "ec",         caption = "Site-Level Economic Connectedness Variables"),
  list(a = figures$RC,            b = figures$RC_rev,
       short_label = "rc",         caption = "Site-Level Relative Connectedness Variables"),
  list(a = figures$FriendRank,    b = figures$FriendRank_rev,
       short_label = "raaw",       caption = "Site-Level Relative Average Alter Wealth Variables")
)


make_paired_site_vars_table <- function(dat, cfg_a, cfg_b, short_label, caption,
                                         tabpath, digits = 2) {

  vars_a <- cfg_a$x_vars[TABLE_WEIGHTS]
  vars_b <- cfg_b$x_vars[TABLE_WEIGHTS]

  table_data <- dat[, c("site_name", vars_a, vars_b)]

  weight_labels <- paste0("\\makecell{", CLR_LABS[WEIGHTS], "}")
  col_labels    <- c("Site", weight_labels, weight_labels)

  table_data %>%
    kableExtra::kable(
      format     = "latex",
      booktabs   = TRUE,
      longtable  = TRUE,
      caption    = caption,
      label      = paste0("site_vars_", short_label),
      digits     = digits,
      align      = "c",
      linesep    = "",
      col.names  = col_labels,
      escape     = FALSE
    ) %>%
    kableExtra::add_header_above(
      setNames(c(1, 4, 4), c(" ", cfg_a$header_prefix, cfg_b$header_prefix))
    ) %>%
    kableExtra::kable_styling(
      latex_options = "repeat_header",
      font_size     = 9
    ) %>%
    kableExtra::save_kable(
      file.path(tabpath, paste0(short_label, "_site_vars_table.tex"))
    )
}

# ==============================================================================
# Generate all tables
# ==============================================================================

walk(table_pairs, function(pr) {
  make_paired_site_vars_table(
    dat         = dat,
    cfg_a       = pr$a,
    cfg_b       = pr$b,
    short_label = pr$short_label,
    caption     = pr$caption,
    tabpath     = tabpath
  )
})