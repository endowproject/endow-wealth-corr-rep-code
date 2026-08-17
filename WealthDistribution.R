## Wealth Distribution Plots
##
## Generates boxplots of sharing-unit wealth across sites for three
## wealth normalisations: absolute, per-capita, and per-adult.
##
## Outputs (Appendix figures):
##   Figures/paper-1st-draft/
##     wealth-boxplots/AbsoluteWealthBoxPlot.pdf
##     wealth-boxplots/AbsoluteWealthPerCapitaBoxPlot.pdf
##     wealth-boxplots/AbsoluteWealthPerAdultBoxPlot.pdf
##   Figures/WealthDistributions/ByVariable/<type>/<site>_<type>.pdf
##   Figures/WealthDistributions/BySite/<site>/<type>.pdf

library(forcats)
library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# Setup
# ==============================================================================

path    <- file.path(EndowGitHub, "DerivedData")
figpath <- file.path(EndowDropbox, "Figures")

wealth_data <- readRDS(file.path(path, "su_master.rds"))

# One row per SU (network == "sum" gives the combined-layer observation)
wealth_data <- wealth_data[wealth_data$network == "sum", ]

# ==============================================================================
# Helper: create and save a flipped boxplot
# ==============================================================================

create_boxplot <- function(data, x_var, x_label, plot_name,
                            x_transformation = "identity",
                            cutoff = NA) {
  if(is.na(cutoff)) cutoff <- max(data[[x_var]], na.rm = TRUE)

  x_lower <- if (x_transformation %in% c("log10", "log2", "log")) {
    min(data[[x_var]][data[[x_var]] > 0], na.rm = TRUE)
  } else {
    0
  }

  data %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      median = median(.data[[x_var]], na.rm = TRUE)
    ) %>% # order sites by median wealth
    dplyr::arrange(median) %>%
    dplyr::pull(site) -> ordered_sites

  data$site_order <- factor(data[["site"]], levels = ordered_sites)

  plot <- ggplot(data = data, aes_string(x = x_var, y = "site_order")) +
    geom_boxplot() +
    theme_classic() +
    labs(x = x_label, y = "Site") +
    scale_x_continuous(
      labels = scales::comma,
      limits = c(x_lower, cutoff),
      trans = x_transformation) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()

  ggsave(
    filename = file.path(figpath, plot_name),
    plot     = plot,
    width    = 10,
    height   = 6
  )
}

safe_expand <- function(data, count_var) {
  bad <- is.na(data[[count_var]]) | data[[count_var]] < 0 | !is.finite(data[[count_var]])
  data <- data[!bad, ]
  data[rep(seq_len(nrow(data)), data[[count_var]]), ]
}

# ==============================================================================
# Cross-site boxplots (absolute, log, and relative)
# ==============================================================================

# Absolute and log wealth (SU-level)
create_boxplot(
  wealth_data, "wealth",
  "Sharing Unit Wealth in USD",
  "paper-1st-draft/wealth-boxplots/AbsoluteWealthBoxPlot.pdf",
  cutoff = 500000
)

create_boxplot(
  wealth_data, "wealth",
  "Sharing Unit Wealth in USD",
  "paper-1st-draft/wealth-boxplots/LogWealthBoxPlot.pdf",
  x_transformation = "log10"
)

# Relative wealth (relative to site mean)
wealth_data <- wealth_data %>%
  group_by(site) %>%
  mutate(wealth_relative = wealth / mean(wealth, na.rm = TRUE)) %>%
  ungroup()

create_boxplot(
  wealth_data, "wealth_relative",
  "Sharing Unit Wealth Relative to Site Average",
  "WealthBoxPlots/Raw/RelativeWealthBoxPlot.pdf"
)

# Expand to person-level for per-capita plots
wealth_data_person <- safe_expand(wealth_data, "su_size")
wealth_data_person <- wealth_data_person %>%
  group_by(site) %>%
  mutate(wealth_per_capita_relative = wealth_per_capita / mean(wealth_per_capita, na.rm = TRUE)) %>%
  ungroup()

create_boxplot(
  wealth_data_person, "wealth_per_capita",
  "Per Capita Wealth in USD",
  "paper-1st-draft/wealth-boxplots/AbsoluteWealthPerCapitaBoxPlot.pdf",
  cutoff = 200000
)
create_boxplot(
  wealth_data_person, "wealth_per_capita_relative",
  "Per Capita Wealth Relative to Site Average",
  "WealthBoxPlots/PerCapita/RelativeWealthPerCapitaBoxPlot.pdf"
)
create_boxplot(
  wealth_data_person, "wealth_per_capita",
  "Per Capita Wealth in USD",
  "paper-1st-draft/wealth-boxplots/LogWealthPerCapitaBoxPlot.pdf",
  x_transformation = "log10"
)

# Expand to adult-level for per-adult plots
wealth_data_adult <- safe_expand(wealth_data, "adult_count")
wealth_data_adult <- wealth_data_adult %>%
  group_by(site) %>%
  mutate(wealth_per_adult_relative = wealth_per_adult / mean(wealth_per_adult, na.rm = TRUE)) %>%
  ungroup()

create_boxplot(
  wealth_data_adult, "wealth_per_adult",
  "Per Adult Wealth in USD",
  "paper-1st-draft/wealth-boxplots/AbsoluteWealthPerAdultBoxPlot.pdf",
  cutoff = 200000
)
create_boxplot(
  wealth_data_adult, "wealth_per_adult_relative",
  "Per Adult Wealth Relative to Site Average",
  "WealthBoxPlots/PerAdult/RelativeWealthPerAdultBoxPlot.pdf"
)
create_boxplot(
  wealth_data_adult, "wealth_per_adult",
  "Per Adult Wealth in USD",
  "paper-1st-draft/wealth-boxplots/LogWealthPerAdultBoxPlot.pdf",
  x_transformation = "log10"
)

# ==============================================================================
# Per-site histogram distributions
# ==============================================================================

plot_types <- list(
  list(var = "wealth",          label = "Wealth ($)",
       folder = "Wealth_Absolute",         y_label = "Number of Sharing Units"),
  list(var = "wealth_relative", label = "Wealth / Average Wealth",
       folder = "Wealth_Relative",         y_label = "Number of Sharing Units"),
  list(var = "wealth_per_adult",         label = "Wealth per Adult ($)",
       folder = "WealthPerAdult_Absolute", y_label = "Number of Adults"),
  list(var = "wealth_per_capita",        label = "Wealth per Capita ($)",
       folder = "WealthPerCapita_Absolute", y_label = "Number of People")
)

for (s in unique(wealth_data$site)) {

  site_df    <- wealth_data[wealth_data$site == s, ]
  adult_df   <- safe_expand(site_df, "adult_count")
  person_df  <- safe_expand(site_df, "su_size")

  site_df$wealth_relative <-
    site_df$wealth / mean(site_df$wealth, na.rm = TRUE)

  data_sources <- list(
    "Wealth_Absolute"        = list(data = site_df,   var = "wealth"),
    "Wealth_Relative"        = list(data = site_df,   var = "wealth_relative"),
    "WealthPerAdult_Absolute"  = list(data = adult_df,  var = "wealth_per_adult"),
    "WealthPerCapita_Absolute" = list(data = person_df, var = "wealth_per_capita")
  )

  site_folder <- file.path(figpath, "WealthDistributions", "BySite", s)
  if (!dir.exists(site_folder)) dir.create(site_folder, recursive = TRUE)

  for (nm in names(data_sources)) {
    src  <- data_sources[[nm]]
    xlab <- switch(nm,
      "Wealth_Absolute"          = "Wealth ($)",
      "Wealth_Relative"          = "Wealth / Average Wealth",
      "WealthPerAdult_Absolute"  = "Wealth per Adult ($)",
      "WealthPerCapita_Absolute" = "Wealth per Capita ($)"
    )
    ylab <- if (grepl("Absolute", nm) && grepl("Adult", nm)) "Number of Adults" else
            if (grepl("Capita", nm)) "Number of People" else "Number of Sharing Units"

    p <- ggplot(data.frame(x = src$data[[src$var]]), aes(x)) +
      geom_histogram(col = "white", boundary = 0, bins = 15) +
      xlab(xlab) + ylab(ylab) + theme_classic()

    ggsave(
      file.path(figpath, "WealthDistributions", "ByVariable", nm,
                paste0(s, "_", nm, ".pdf")),
      width = 7, height = 7,
      plot = p
    )
    ggsave(file.path(site_folder, paste0(nm, ".pdf")), width = 7, height = 7, plot = p)
  }
}
