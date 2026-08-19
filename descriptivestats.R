## Descriptive Statistics
##
## Generates summary statistics and community-overview figures.
##
## Outputs:
##   Figures/paper-1st-draft/sites_map.png           - Fig. 1 in the paper
##   Figures/paper-1st-draft/site_descriptives.pdf   — Fig. 2 in the paper
##   Figures/paper-1st-draft/site_su_descrip.pdf     — Appendix figure
##   Figures/paper-1st-draft/subsistence.png         - Appendix paper
##   Tables/DescriptiveStats/su_summary.tex          — Table S1 in the paper
##   Tables/DescriptiveStats/summary_table.csv       — pooled SU summary stats

library(stargazer)
library(dplyr)
library(ggplot2)
library(plm)
library(tidyverse)
library(gridExtra)
library(igraph)
library(sf)
library(rnaturalearth)
library(patchwork)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# EndowDerivedData is defined in .Rprofile on Windows; on Mac we fall back to
# the DerivedData subfolder of this repository.
if (!exists("EndowDerivedData")) {
  EndowDerivedData <- file.path(EndowGitHub, "DerivedData")
}

path <- EndowDerivedData
figpath <- file.path(EndowDropbox, "Figures", "DescriptiveStats")
tablepath <- file.path(EndowDropbox, "Tables", "DescriptiveStats")

su_df <- readRDS(file.path(path, "su_master.rds"))
su_df <- su_df %>% filter(network == "sum")

scd <- read.csv(file.path(path, "site_country_descriptives.csv"))
ethno_meta <- read.csv(file.path(path, "word_form_extractions.csv"))

dfl <- read.csv(file.path(path, "DataForLASSO.csv"))

site_codes <- sort(dfl$site_name)

load(file.path(path, "su_alters.rdata"))
load(file.path(path,"su_nets_expanded.rdata"))
load(file.path(path, "su_meta.rdata"))
load(file.path(path, "people_observations.rdata"))

networks <- Map(read.csv, Sys.glob(file.path(EndowDatabase, "networks", "*_networks.csv"))) |>
  setNames(regmatches(
    Sys.glob(file.path(EndowDatabase, "networks", "*_networks.csv")),
    regexpr("[A-Z]{2}(?=.*\\.csv)", Sys.glob(file.path(EndowDatabase, "networks", "*_networks.csv")), perl = TRUE)
  ))

# Drop sites we explicitly don't want to be including
# The SITES_TO_DROP variable is defined in 00_run_all.R
# (other objects should already be subsetted)
scd <- scd[!scd$SiteCode %in% SITES_TO_DROP, ]

############ Elly's New Code: xx need to integrate properly, some later code can perhaps be removed.

# Create Summary of Sharing Unit Characteristics by Site

## NOTE! The CR su_meta df has *multiple* entries for some su_ids (because there are also 'famid's)
## for our counts now, all these vars are the same, so just removing duplicates
su_meta$CR <- su_meta$CR[duplicated(su_meta$CR$su_id) == FALSE,]

su_summary <- su_meta %>%
  imap_dfr(
    ~ tibble(
      site = .y,
      su_count = nrow(.x),
      mean_su_size = mean(.x$su_size, na.rm = TRUE),
      sd_su_size = sd(.x$su_size, na.rm = TRUE),
      max_su_size = max(.x$su_size, na.rm = TRUE),
      #mean_adult = mean(.x$adult_count, na.rm = TRUE),
      mean_num_surveyed = mean(.x$num_surv, na.rm = TRUE),
      #max_num_surveyed = max(.x$num_surv, na.rm = TRUE),
      prop_sampled = mean(replace(.x$su_sampled, is.na(.x$su_sampled), FALSE)),
      prop_withwealth = mean(!is.na(.x$su_wealth)),
      mean_wealth = mean(.x$su_wealth, na.rm = TRUE),
      median_wealth = median(.x$su_wealth, na.rm = TRUE),
      sd_wealth = sd(.x$su_wealth, na.rm = TRUE),
      missing_wealth = sum(is.na(.x$su_wealth)),
      missing_networks = sum(.x$su_sampled == FALSE | is.na(.x$su_sampled))
    )
  )

# Overall stats:
#sum(su_summary$missing_wealth)
#sum(su_summary$missing_networks)

#sum(su_summary$missing_wealth) / sum(su_summary$su_count) * 100
#sum(su_summary$missing_networks) / sum(su_summary$su_count) * 100

su_summary <- su_summary %>%
  left_join(
    scd %>% dplyr::select(SiteCode, Name, Country, Fieldwork.year),
    by = join_by("site" == "SiteCode")
  ) %>%
  dplyr::select(-c(missing_wealth, missing_networks)) %>%
  relocate(Country, Fieldwork.year, .after = site) %>%
  relocate(Name, .after = Fieldwork.year) %>%
  arrange(site)

## ADD MORE ANTHROS AS NEEDED
su_summary$Name[su_summary$site == "EK"] <- "Alexandra Alvergne and Banrida Langstieh"
su_summary$Name[su_summary$site == "HI"] <- "Brooke Scelza and Sean Prall"
su_summary$Name[su_summary$site == "MG"] <- "Bapu Vaitla and Christopher Golden"
su_summary$Name[su_summary$site == "PE"] <- "Emmanuel Maliti and Monique Borgerhoff Mulder"
su_summary$Name[su_summary$site == "AR"] <- "Madalena Monteban, Juan Pablo Ferreiro and Federico Fernandez"
su_summary$Name[su_summary$site == "CR"] <- "Christine Beitl and Wendy Ch\\'{a}vez-P\\'{a}ez"
su_summary$Name[su_summary$site == "AH"] <- "Michele Barnes and Joshua Cinner"
su_summary$Name[su_summary$site == "SH"] <- "Katherine Starkweather"
su_summary$Name[su_summary$site == "RA"] <- "Angelina Demarco"
su_summary$Name[su_summary$site == "CP"] <- "Kathryn Oths"
su_summary$Name[su_summary$Name == "Ed Seabright"] <- "Edmond Seabright"
su_summary$Name[su_summary$site == "BD"] <- "Mary Shenk and Nurul Alami"
su_summary$Name[su_summary$site == "SN"] <- "John Ziker and Karl Mertens"
su_summary$Name[su_summary$Name == "Siobhan Mattison"] <- "Siobh\\'{a}n Cully"

## bring in participants from ethnographers' Word metadata files.
## rephrase some to align
ethno_meta$fieldsite_particip[ethno_meta$fieldsite_code == "BD"] <- "Bengalis"
ethno_meta$fieldsite_particip[ethno_meta$fieldsite_code == "DJ"] <- "Xwla and Fon"
ethno_meta$fieldsite_particip[ethno_meta$fieldsite_code == "FF"] <- "Damara, Nama, Ovaherero, and Ovambo"
ethno_meta$fieldsite_particip[ethno_meta$fieldsite_code == "FJ"] <- "Yasawans"
ethno_meta$fieldsite_particip[ethno_meta$fieldsite_code == "TP"] <- "Tiriyó, Wayana, and Akuriyó"

su_summary <- su_summary %>%
  left_join(
    ethno_meta %>% dplyr::select(fieldsite_code, fieldsite_particip),
    by = join_by("site" == "fieldsite_code")
  ) %>%
  relocate(fieldsite_particip, .after = site)

## missing responses!
su_summary$fieldsite_particip[su_summary$site == "LB"] <- "Mosuo"
su_summary$fieldsite_particip[su_summary$site == "RA"] <- "Haitians"
su_summary$fieldsite_particip[su_summary$site == "SH"] <- "Shodagor"
su_summary$fieldsite_particip[su_summary$site == "TZ"] <- "High Atlas Amazighs"
su_summary$fieldsite_particip[su_summary$site == "VT"] <- "Ni-Vanuatu"
su_summary$fieldsite_particip[su_summary$site == "YN"] <- "Mosuo"

digits_vector <- ifelse(grepl("max|min|count|_wealth", colnames(su_summary)), 0, 2)

kableExtra::kable(
  su_summary,
  digits = digits_vector,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  caption = "Summary of each community and its sharing unit characteristics, sampling, and wealth. The values for the material wealth of sharing units are reported in U.S. dollars, circa the time of data collection at the respective field sites. The Members columns show the average, standard deviation, and maximum number of sharing unit members in each community. The Sampling columns show the average number of people surveyed in each sharing unit for the network questions, and subsequently the percent of sharing units with network and wealth data. The Wealth columns show the mean, median, and standard deviation of the total wealth of sharing units in each community.",
  label = "su_summary",
  col.names = c(
    "Code",
    "Participants",
    "Country",
    "Year",
    "Lead(s)",
    "\\# SU",
    "Mean",
    "SD",
    "Max", # "Mean \\# Adults",
    "\\# Surv",
    "\\% w/ Net",
    "\\% w/ Wealth",
    "Mean",
    "Median",
    "SD"
  ),
  linesep = "",
  escape = FALSE
) |>
  kableExtra::kable_styling(
    latex_options = "repeat_header",
    font_size = 8
  ) |>
  kableExtra::add_header_above(
    c(
      " " = 6, # Site, Particip, Country, Year, Lead, # of SUs
      "Members" = 3, # Mean, SD, Max, Mean # Adults
      "Sampling" = 3, # Mean # Surveyed, Prop. Sampled, Prop. w/ Wealth Data
      "Wealth" = 3 # Mean, Median, SD
    ),
    escape = FALSE
  ) |>
  kableExtra::column_spec(2, width = "3cm") |>
  kableExtra::column_spec(3, width = "2cm") |>
  kableExtra::column_spec(5, width = "3cm") |>
  kableExtra::save_kable(file.path(
    EndowDropbox,
    "Tables",
    "DescriptiveStats",
    "su_summary.tex"
  ))

################# SUMMARY STATS #########################
# List of variables to process
variables <- c(
  "adult_count",
  "su_size",
  "wealth",
  "degree",
  "able_count",
  "num_surveyed"
)

summary_table <- purrr::map_dfr(variables, \(var) {
  su_df |>
    dplyr::summarise(
      variable = var,
      mean     = mean(.data[[var]], na.rm = TRUE),
      sd       = sd(.data[[var]],   na.rm = TRUE),
      min      = min(.data[[var]],  na.rm = TRUE),
      max      = max(.data[[var]],  na.rm = TRUE)
    )
}) |>
  dplyr::bind_rows(
    su_df |>
      dplyr::count(site) |>
      dplyr::summarise(
        variable = "su_count",
        mean     = mean(n, na.rm = TRUE),
        sd       = sd(n,   na.rm = TRUE),
        min      = min(n,  na.rm = TRUE),
        max      = max(n,  na.rm = TRUE)
      )
  ) |>
  dplyr::select(variable, mean, sd, min, max)

filename = file.path(tablepath, "summary_table.csv")
write.csv(summary_table, filename, row.names = FALSE)

################# PLOTS ####################

# box plot for sharing unit size
plot_su_size <- ggplot(su_df, aes(x = site, y = su_size)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Number of Members in the Sharing Unit by Community",
    x = "Community",
    y = "Number of Members"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 10))
ggsave(
  filename = file.path(figpath, "su_size_boxplot.png"),
  plot = plot_su_size,
  width = 10,
  height = 8,
  dpi = 300
)

# Plot for number of adults
plot_adult_count <- ggplot(su_df, aes(x = site, y = adult_count)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Number of Adults in the Sharing Unit by Community",
    x = "Community",
    y = "Number of Adults"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 10))
ggsave(
  filename = file.path(figpath, "adult_count_boxplot.png"),
  plot = plot_adult_count,
  width = 10,
  height = 8,
  dpi = 300
)

# box plot of number of respondents
plot_surveyed_count <- ggplot(su_df, aes(x = site, y = num_surveyed)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Number of Respondents in the Sharing Unit by Community",
    x = "Community",
    y = "Number Surveyed"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 10))
ggsave(
  filename = file.path(figpath, "surveyed_boxplot.png"),
  plot = plot_surveyed_count,
  width = 10,
  height = 8,
  dpi = 300
)


## wealth, wealth per adult

plot_wealth <- ggplot(su_df, aes(x = site, y = wealth)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Wealth in Sharing Units by Community",
    x = "Community",
    y = "Wealth"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100000) +
  theme_classic()
ggsave(
  filename = file.path(figpath, "wealth_boxplot.png"),
  plot = plot_wealth,
  width = 10,
  height = 8,
  dpi = 300
)


plot_wealth_pa <- ggplot(su_df, aes(x = site, y = wealth_per_adult)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Wealth Per Adult in Sharing Units by Community",
    x = "Community",
    y = "Wealth Per Adult"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 50000) +
  theme_classic()
ggsave(
  filename = file.path(figpath, "wealth_per_adult_boxplot.png"),
  plot = plot_wealth_pa,
  width = 10,
  height = 8,
  dpi = 300
)


# density plot of all sites
variables_to_plot <- c("wealth", "wealth_per_adult", "rank_wealth")

for (site in site_codes) {
  for (var in variables_to_plot) {
    # Filter data for the current site
    site_data <- su_df %>% filter(site == site) %>% dplyr::select(site, !!sym(var))

    # Plotting the density of the current variable
    plot <- ggplot(site_data, aes_string(x = var)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(
        title = paste("Density of", var, "for Site", site),
        x = var,
        y = "Density"
      ) +
      theme_classic() +
      theme(legend.title = element_blank(), legend.position = "none")

    # Define the filename
    filename <- paste(site, var, "density_plot.png", sep = "_")
    filepath <- file.path(figpath, "DensityPlots", filename)

    # Save the plot
    ggsave(filepath, plot, width = 10, height = 6, dpi = 300)
  }
}

n_su <- dfl %>%
  ggplot(aes(y = n_su, x = "")) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Number of Sharing Units",
    x = "Community Size"
  )

mean_deg <- dfl %>%
  ggplot(aes(y = mean_degree, x = "")) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Mean Degree",
    x = "Support Partners\n(Within Community)"
  ) +
  scale_y_continuous(
    limits = c(2, 40),
    breaks = seq(4, 36, by = 4)
  )

su_size <- su_df %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    mean = mean(su_size, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(y = mean, x = "")
  ) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
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
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Mean Number of Members",
    x = "Sharing Unit Size"
  ) +
  scale_y_continuous(
    limits = c(2, 17),
    breaks = seq(4, 16, by = 4)
  )


su_kin <- su_df %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    mean = mean(primary_kin, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(y = mean, x = "")
  ) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
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
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Average Count",
    x = "Co-Resident Kin"
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2)
  )



alters <- purrr::imap_dfr(su_alters, function(x, site_name) {
  x |>
    dplyr::group_by(tie) |>
    dplyr::summarise(
      tot_alt = mean(alter_count),
      tot_ext = {
        ext <- externals_count
        ext[is.na(ext) & alter_count > 0] <- 0
        mean(ext)
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      site     = site_name,
      prop_ext = tot_ext / tot_alt
    )
}) |>
  dplyr::select(site, tie, tot_alt, tot_ext, prop_ext)

su_alt <-
  ggplot(data = alters[alters$tie == "all", ], aes(y = tot_alt, x = "")) +
  geom_violin(
    width = 0.25,
    trim = TRUE
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
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
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Average Count",
    x = "Support Partners"
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  )

su_ext <-
  ggplot(data = alters[alters$tie == "all", ], aes(y = prop_ext, x = "")) +
  geom_violin(
    width = 0.25,
    trim = TRUE
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
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
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Average Proportion",
    x = "External Partners"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  )

su_df %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    median = median(wealth, na.rm = TRUE)
  ) %>% # order sites by median wealth
  arrange(median) %>%
  pull(site) -> ordered_sites

su_df$site_order <- factor(su_df$site, levels = ordered_sites)

su_wealth <- ggplot(su_df, aes(x = site_order, y = wealth)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(x = "", y = "Sharing Unit Wealth (USD)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 100000) +
  coord_flip()


#joint <- egg::ggarrange(su_size, su_kin, su_alt, su_ext, su_wealth,  ncol = 5)
#ggsave(file.path(figpath, "site_su_descrip.pdf"), joint, width = 16, height = 8)

site_su_descrip <- egg::ggarrange(n_su, su_size, mean_deg, su_wealth, ncol = 4)
pdf(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "site_su_descrip.pdf"),
  width = 12,
  height = 6
)
showtext::showtext_begin()
grid::grid.draw(site_su_descrip)
showtext::showtext_end()
dev.off()

#### Geospatial ####

## Our sites are often REMOTE

## population density
pdens <- ggplot(
  data = dfl,
  aes(y = pop_density, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = People ~ per ~ km^2,
    x = "Population Density"
  )


## nightlights
nlight <- ggplot(
  data = dfl,
  aes(y = nightlight, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = Average ~ Day / Night ~ Band ~ radiance ~ (nW / cm^2 / sr),
    x = "Nightlight Radiance"
  )


## accessibility; land-based travel time to the nearest densely-populated area. Densely-populated areas are defined as contiguous areas with 1,500 or more inhabitants per square kilometer or a majority of built-up land cover types coincident with a population center of at least 50,000 inhabitants.
access <- ggplot(
  data = dfl,
  aes(y = MAP_accessibility_1000_mean / 60, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Travel time (hours)",
    x = "Accessibility to Nearest Densely-Populated Area"
  )

## Prop Time Salary
dfl$prop_time_salary <-
  as.numeric(dfl$prop_time_salaried_outside) +
  as.numeric(dfl$prop_time_salaried_within)

shock_vars <- colnames(dfl)[grep("how_common", colnames(dfl))]
dfl$shock_2plus <- rowSums(dfl[, shock_vars] > 1, na.rm = TRUE)

wage_lab <- ggplot(
  data = dfl,
  aes(y = prop_time_salary, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Percent",
    x = "Time in\nWage/Salaried Labor"
  )


shocks <- ggplot(
  data = dfl,
  aes(y = shock_2plus, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.2, height = 0.05),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Count",
    x = "More-Than-Yearly\nShocks"
  )


pdf(file.path(figpath, "endow_remote.pdf"), width = 8.5, height = 4)
gridExtra::grid.arrange(
  pdens,
  nlight,
  wage_lab,
  ncol = 3,
  top = grid::textGrob(
    "ENDOW sites are generally rural and remote ... ",
    gp = grid::gpar(fontsize = 20)
  )
)
dev.off()

## And are ecologically diverse

## biologically effective degree days
bedd <- ggplot(
  data = dfl,
  aes(y = bedd, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Sum of Daily Mean Temperatures (\u00B0C)\nabove 10\u00B0C and less than 30\u00B0C",
    x = "Biologically-Effective\nDegree Days"
  )


## mean of diurnal temperature range
diurnal <- ggplot(
  data = dfl,
  aes(y = diurnal, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Degrees (\u00B0C)",
    x = "Mean Diurnal Temperature Range"
  )

## precipitation
precip <- ggplot(
  data = dfl,
  aes(y = precip_all, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Average 10-Day Precipitation Sum (mm)",
    x = "Precipitation"
  )

pdf(file.path(figpath, "endow_eco.pdf"), width = 8.5, height = 4)
gridExtra::grid.arrange(
  bedd,
  diurnal,
  precip,
  ncol = 3,
  top = grid::textGrob(
    "... and ecologically diverse.",
    gp = grid::gpar(fontsize = 20)
  )
)
dev.off()

## Human Influence Index
hii <- ggplot(
  data = dfl,
  aes(y = hii_250_mean, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Greater values = greater impact",
    x = "Human Influence Index"
  )

## Property Rights
property <- ggplot(
  data = dfl,
  aes(y = v2xcl_prpty_value, x = "")
) +
  geom_violin(
    width = 0.25
  ) +
  geom_boxplot(width = 0.05, outliers = FALSE) +
  #geom_jitter(alpha = 0.2, width = 0.05, height = 0.05) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    position = ggplot2::position_jitter(width = 0.1),
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    y = "Greater values = more widely-shared",
    x = "Property Rights Index"
  )

site_descrip <- egg::ggarrange(
  #pdens,
  #nlight,
  bedd,
  #precip,
  hii,
  property,
  wage_lab,
  shocks,
  ncol = 5
)

pdf(
  file.path(
    EndowDropbox,
    "Figures",
    "paper-1st-draft",
    "site_descriptives.pdf"
  ),
  width = 12,
  height = 6
)
showtext::showtext_begin()
grid::grid.draw(site_descrip)
showtext::showtext_end()
dev.off()


## SITE MAP (Figure 1)

## NOTE: Not everyone has access to this repo!
eds <- read.csv(file.path(EndowDerivedData, "ethnographer_data_status.csv"), as.is = TRUE, stringsAsFactors = FALSE)

# dividing Elly's into two
#eds[nrow(eds) + 1,] <- eds[eds$SiteCode == "TN",]
#eds$SiteCode[nrow(eds)] <- "TE"
#eds$SiteCode[eds$SiteCode == "TN"] <- "AZ"

# dividing Curtis' into two
#eds[nrow(eds) + 1,] <- eds[eds$SiteCode == "MC",]
#eds$SiteCode[nrow(eds)] <- "AP"

# limit to only those included
eds <- eds[eds$SiteCode %in% site_codes,]

land <- rnaturalearth::ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")

world <- ggplot() +
  geom_sf(data = land, fill = "#999999", colour = NA) +
  geom_point(data = eds, aes(x = RoughLong, y = RoughLat), alpha = 0.7) +
  ggrepel::geom_text_repel(
    data = eds,
    aes(x = RoughLong, y = RoughLat, label = SiteCode),
    family = "roboto",
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.curvature = -0.1
  ) +
  theme(
    text = element_text(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  coord_sf(
    crs = "+proj=eqearth",
    ylim = c(-30, 65),
    default_crs = sf::st_crs(4326)
  )

ggsave(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "sites_map.png"),
  world,
  width = 12,
  height = 6
)

knitr::plot_crop(file.path(EndowDropbox, "Figures", "paper-1st-draft", "sites_map.png")) ## crops margins! Ace!

## Subsistence plot

## missing responses! current best guesses!
dfl$EcolSubsistence1[dfl$site_name == "KO"] <- "Intensive Agriculture, with plow"
dfl$EcolSubsistence2[dfl$site_name == "KO"] <- "Pastoralism"
dfl$EcolSubsistence1[dfl$site_name == "MA"] <- "Pastoralism"
dfl$EcolSubsistence2[dfl$site_name == "MA"] <- "Intensive Agriculture, with plow"
dfl$EcolSubsistence2[dfl$site_name == "MN"] <- "Labor market"
dfl$EcolSubsistence1[dfl$site_name == "TP"] <- "Advanced Horticulture, with metal hoes"
dfl$EcolSubsistence2[dfl$site_name == "TP"] <- "Fishing"



# Generates offsets for n items arranged in a compact centered grid
make_offsets <- function(n, spacing = 0.3) {
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  grid <- expand.grid(
    col = seq_len(ncol) - (ncol + 1) / 2,
    row = seq_len(nrow) - (nrow + 1) / 2
  )[seq_len(n), ]
  tibble(x_off = grid$col * spacing, y_off = grid$row * spacing)
}

ecol_levels <- c(
  "Gathering",
  "Hunting and/or Marine Animals",
  "Fishing",
  "Anadromous Fishing (spawning fish such as Salmon)",
  "Mounted Hunting",
  "Pastoralism",
  "Shifting Cultivation, with digging sticks or wooden hoes",
  "Shifting Cultivation, with metal hoes",
  "Horticultural Gardens or Tree Fruits",
  "Advanced Horticulture, with metal hoes",
  "Intensive Agriculture, with no plow",
  "Intensive Agriculture, with plow",
  "Labor market"
)

used_levels <- ecol_levels[ecol_levels %in% union(dfl$EcolSubsistence1, dfl$EcolSubsistence2)]

dfl <- dfl %>%
  mutate(
    EcolSubsistence1 = factor(
      as.character(EcolSubsistence1),
      levels = used_levels,
      ordered = TRUE
    ),
    EcolSubsistence2 = factor(
      as.character(EcolSubsistence2),
      levels = used_levels,
      ordered = TRUE
    )
  )

plot_df <- dfl %>%
  dplyr::mutate(
    region = countrycode::countrycode(
      iso3c,
      origin = "iso3c",
      destination = "region23"
    ),
    x_num = as.numeric(EcolSubsistence2), # Secondary on x
    y_num = as.numeric(EcolSubsistence1) # Primary on y
  ) %>%
  group_by(x_num, y_num) %>%
  group_modify(~ bind_cols(.x, make_offsets(nrow(.x)))) %>%
  ungroup() %>%
  dplyr::mutate(x_plot = x_num + x_off, y_plot = y_num + y_off)

region_levels <- sort(unique(plot_df$region))
region_colors <- setNames(
  colorspace::qualitative_hcl(length(region_levels), palette = "Dark 3"),
  region_levels[c(1, 2, 3)]#, 4, 5, 6, 7, 9, 8, 10, 11, 12, 13)]
)

x_levels <- levels(dfl$EcolSubsistence2)
y_levels <- levels(dfl$EcolSubsistence1)

x_breaks <- seq(1.5, length(x_levels) - 0.5, by = 1)
y_breaks <- seq(1.5, length(y_levels) - 0.5, by = 1)

subs <- ggplot(plot_df, aes(x_plot, y_plot, label = site_name, fill = region)) +
  geom_vline(xintercept = x_breaks, color = "grey85", linewidth = 0.3) +
  geom_hline(yintercept = y_breaks, color = "grey85", linewidth = 0.3) +
  geom_label(
    size = 2.8,
    family = "roboto",
    label.padding = unit(0.15, "lines"),
    color = "white"
  ) +
  scale_color_manual(values = region_colors, name = "Region") +
  scale_fill_manual(values = region_colors, name = "Region") +
  scale_x_continuous(
    breaks = seq_along(x_levels),
    labels = str_wrap(x_levels, width = 15),
    limits = c(0.5, length(x_levels) + 0.5),
    expand = c(0, 0),
    trans = "reverse",
  ) +
  scale_y_continuous(
    breaks = seq_along(y_levels),
    labels = str_wrap(y_levels, width = 15),
    limits = c(0.5, length(y_levels) + 0.5),
    expand = c(0, 0),
    position = "left",
    trans = "reverse",
  ) +
  theme_classic() +
  labs(x = "Secondary Subsistence", y = "Primary Subsistence") +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.3),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "none"
  )

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    region = countrycode::countrycode(
      iso_a3,
      origin = "iso3c",
      destination = "region23"
    )
  ) %>%
  mutate(
    region_fill = if_else(region %in% region_levels, region, NA_character_)
  )

world_regions <- world %>%
  filter(!is.na(region_fill)) %>%
  group_by(region_fill) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(
    label_point = st_point_on_surface(geometry),
    lon = st_coordinates(label_point)[, 1],
    lat = st_coordinates(label_point)[, 2]
  )

thumbnail <- ggplot(world) +
  geom_sf(aes(fill = region_fill), color = NA) +
  scale_fill_manual(
    values = region_colors,
    na.value = "grey85",
    name = "Region"
  ) +
  #ggrepel::geom_text_repel(
  #  data = world_regions,
  #  aes(x = lon, y = lat, label = region_fill),
  #  size = 2,
  #  color = "black",
  #  max.overlaps = Inf,
  #  min.segment.length = 0,
  #  segment.size = 0.2,
  #  bg.color = "white",
  #  bg.r = 0.1
  #) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_sf(crs = "+proj=eqearth", default_crs = sf::st_crs(4326))

combined <- subs / thumbnail + plot_layout(heights = c(3, 1))

ggsave(
  file.path(EndowDropbox, "Figures", "paper-1st-draft", "subsistence.png"),
  plot = combined,
  width = 10,
  height = 11,
  dpi = 300
)