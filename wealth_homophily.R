## Wealth Homophily
##
## Computes neighbour wealth statistics (average, SD, range) for each sharing
## unit, then aggregates to site level and correlates with the wealth Gini.
## The key question: do places with more wealth inequality also have more
## heterogeneous mixing between rich and poor?
##
## Also produces a version excluding the outlier sites NI and AR
##
## Outputs:
##   Figures/paper-1st-draft/Average Normed SD of Neighbor Wealth.png
##   Figures/paper/wealth_homophily/*.png

library(igraph)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(ineq)

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")
load(file.path(path, "su_nets_expanded.rdata"))  # su_nets_expanded list


gini <- read.csv(file.path(path, "gini_site_data.csv"))
iec <- read.csv(file.path(path, "IEC_Revised.csv"))
iec <- iec %>% dplyr::select(su_id, site, IEC)
iec_sitelevel <- iec  %>%
  group_by(site) %>%
  dplyr::summarise(var_iec = ineq(IEC,na.rm = TRUE))


network = "sum"


# Function to compute neighbor statistics
compute_neighbor_stats_igraph <- function(graph, attr_name) {
  su_wealth_values <- vertex_attr(graph, attr_name)
  mean_site_wealth <- mean(su_wealth_values, na.rm = TRUE)
  stats <- t(sapply(V(graph), function(v) {
    neighbor_ids <- neighbors(graph, v)  # Get neighbors
    neighbor_values <- su_wealth_values[neighbor_ids]  # Extract su_wealth values

    if (length(neighbor_values) > 0){
      c(avg = mean(neighbor_values, na.rm = TRUE),
        range = ifelse(is.infinite( max(neighbor_values, na.rm = TRUE) - min(neighbor_values, na.rm = TRUE)),
                       NA,
                       max(neighbor_values, na.rm = TRUE) - min(neighbor_values, na.rm = TRUE)),
        std = sd(neighbor_values, na.rm = TRUE))
    } else {
      c(avg = NA, range = NA, std = NA)
    }
  }))

  # Return as a dataframe
  return(data.frame(
    su_id = V(graph)$name,
    mean_site_wealth = mean_site_wealth,
    ego_wealth = vertex_attr(graph, attr_name),
    #neighbor level
    avg_alter_su_wealth = stats[, "avg"],
    range_alter_su_wealth = stats[, "range"],
    sd_alter_su_wealth = stats[, "std"]
  ))
}




final_results <- do.call(
  rbind,
  lapply(names(su_nets_expanded), function(site_name) {
    site_networks <- su_nets_expanded[[site_name]]

    if (!(network %in% names(site_networks))) {
      return(NULL)
    }

    ## do once with and once without outliers, i.e., top 5% of wealth distrib
    do.call(
      rbind,
      lapply(c(FALSE, TRUE), function(remove_outliers) {
        sum_network <- site_networks[[network]]
        su_wealth_values <- V(sum_network)$su_wealth
        bad_nodes <- which(
          is.na(su_wealth_values) |
            is.nan(su_wealth_values) |
            is.infinite(su_wealth_values) |
            su_wealth_values == 0
        )
        sum_network <- delete_vertices(sum_network, bad_nodes)
        sum_network <- simplify(sum_network)

        if (remove_outliers) {
          sum_network <- delete_vertices(
            sum_network,
            which(
              V(sum_network)$su_wealth >
                quantile(V(sum_network)$su_wealth, 0.95)
            )
          )
        }

        diads <- as_edgelist(sum_network, names = FALSE)
        su_wealth_values <- V(sum_network)$su_wealth
        wealth_diff <- abs(
          su_wealth_values[diads[, 1]] - su_wealth_values[diads[, 2]]
        )
        sum_wealth_diff <- sum(wealth_diff, na.rm = TRUE)
        sd_wealth_diff <- sd(wealth_diff, na.rm = TRUE)

        stats_df <- compute_neighbor_stats_igraph(sum_network, "su_wealth")
        stats_df$site <- site_name
        stats_df$remove_outliers <- remove_outliers # tag each row

        stats_df$norm_sd_alter_su_wealth <- stats_df$sd_alter_su_wealth /
          stats_df$mean_site_wealth
        stats_df$norm_sd_wealth_diff <- sd_wealth_diff /
          stats_df$mean_site_wealth
        stats_df$norm_ego_su_wealth <- stats_df$ego_wealth /
          stats_df$mean_site_wealth

        return(stats_df)
      })
    )
  })
)

# Convert to dataframe for easy viewing
final_results <- as.data.frame(final_results)


# Compute site-level measures
site_measures <- final_results %>%
  group_by(site, remove_outliers) %>%
  dplyr::summarise(
    mean_range_alter_su_wealth = mean(range_alter_su_wealth, na.rm = TRUE),
    median_range_alter_su_wealth = median(range_alter_su_wealth, na.rm = TRUE),
    sd_ego_su_wealth = sd(ego_wealth, na.rm = TRUE),
    norm_sd_ego_su_wealth = sd(norm_ego_su_wealth, na.rm = TRUE),
    mean_norm_sd_alter_su_wealth = mean(norm_sd_alter_su_wealth, na.rm = TRUE),
    normed_sd_wealth_diff = mean(norm_sd_wealth_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = remove_outliers,
    values_from = c(
      mean_range_alter_su_wealth,
      median_range_alter_su_wealth,
      sd_ego_su_wealth,
      norm_sd_ego_su_wealth,
      mean_norm_sd_alter_su_wealth,
      normed_sd_wealth_diff
    ),
    names_glue = "{.value}_{ifelse(remove_outliers, 'excl', 'incl')}"
  )

combined_data <- merge(site_measures, gini, by = "site")
combined_data <- merge(combined_data, iec_sitelevel, by = "site")
combined_data <- combined_data %>%
  mutate( ## summary vars for both with and without top outliers
    normalized_alter_mean_range_incl = mean_range_alter_su_wealth_incl /
      mean_wealth,
    normalized_alter_median_range_incl = median_range_alter_su_wealth_incl /
      mean_wealth,
    normalized_alter_mean_range_excl = mean_range_alter_su_wealth_excl /
      mean_wealth,
    normalized_alter_median_range_excl = median_range_alter_su_wealth_excl /
      mean_wealth
  )

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) {
    "p < 0.0001"
  } else {
    paste0("p = ", formatC(p, digits = digits, format = "f"))
  }
}

plot_with_correlation <- function(x_var, y_var, x_label, y_label, color, df = combined_data, remove_outliers = FALSE) {

  df <- df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

  # Compute Pearson correlation and p-value
  cor_test <- cor.test(df[[x_var]], df[[y_var]], use = "complete.obs")
  r_value <- round(cor_test$estimate, 2)  # Correlation coefficient
  slope <- round(coef(lm(
    df[[y_var]] ~ df[[x_var]]))[2], 2)
  #p_value <- format_pval(cor_test$p.value)   # P-value

  # Create scatter plot
  plot <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    #geom_point(size = 1) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "blue",
      aes(group = 1),
      size = 0.5
    ) +
    #ggrepel::geom_text_repel(size = 2, max.overlaps = 15) +
    geom_label(
      aes_string(label = "site"),
      family = "roboto",
      size = 1.5,
      alpha = 0.5,
      linewidth = 0.25,
      label.padding = unit(0.3, "lines"),
      label.r = unit(0.6, "lines")
    ) +
    labs(
      title = paste0("Correlation: ", r_value, ", Slope: ", slope),
      x = x_label,
      y = y_label
    ) +
    #ylim(0, 1) +
    theme_classic()

  fname = paste0(x_label, " vs ", y_label,".png")
  ggsave(filename = file.path(figpath, fname), plot = plot, width = 5, height = 5, dpi = 300)

}

figpath <- file.path(EndowDropbox, "Figures", "paper", "wealth_homophily")

plot_with_correlation("normalized_alter_mean_range_incl", "gini_wealth",
                                              "Normalized Mean of Range of Neighbor Wealth", "Gini", "black")


plot_with_correlation("normalized_alter_median_range_incl", "gini_wealth",
                                                "Normalized Median of Range of Neighbor Wealth ", "Gini", "black")


plot_with_correlation(
  "mean_norm_sd_alter_su_wealth_incl",
  "gini_wealth",
  "Average Normed SD of Neighbor Wealth",
  "Gini",
  "black"
)


plot_with_correlation(
  "normed_sd_wealth_diff_incl",
  "gini_wealth",
  "Normed SD of Dyad Wealth difference",
  "Gini",
  "black")


figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft")

plot_with_correlation(
  "norm_sd_ego_su_wealth_incl",
  "mean_norm_sd_alter_su_wealth_incl",
  "Normed SD of SU Wealth",
  "Average Normed SD of Neighbor Wealth",
  "black"
)

plot_with_correlation(
  "norm_sd_ego_su_wealth_excl",
  "mean_norm_sd_alter_su_wealth_excl",
  "Normed SD of SU Wealth (minus top 5 perc)",
  "Average Normed SD of Neighbor Wealth (minus top 5 perc)",
  "black"
)


# version with two lines

plot_with_both <- function(x_var, y_var, x_label, y_label, color, df = combined_data) {

  df <- df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))  # Remove NA values

  # Separate data: One excluding "NI" and "AR", one including everything
  df_no_NI <- df %>% filter(!site %in% c("NI", "AR"))
  df_with_NI <- df

  # Compute Pearson correlation and p-value for both datasets
  cor_test_no_NI <- cor.test(df_no_NI[[x_var]], df_no_NI[[y_var]], use = "complete.obs")
  r_value_no_NI <- round(cor_test_no_NI$estimate, 2)
  #p_value_no_NI <- format_pval(cor_test_no_NI$p.value)

  cor_test_with_NI <- cor.test(df_with_NI[[x_var]], df_with_NI[[y_var]], use = "complete.obs")
  r_value_with_NI <- round(cor_test_with_NI$estimate, 2)
  #p_value_with_NI <- format_pval(cor_test_with_NI$p.value)

  # Create scatter plot
  plot <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    #geom_point(size = 1) +  # Points for all data
    geom_smooth(
      data = df_no_NI,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "black",
      aes(group = 1),
      size = 0.7
    ) +
    geom_smooth(
      data = df_with_NI,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "grey",
      aes(group = 1),
      size = 0.7
    ) +
    #geom_text_repel(aes(label = site), size = 3, max.overlaps = 15) +
    geom_label(
      aes(label = site),
      family = "roboto",
      size = 1.5,
      alpha = 0.5,
      linewidth = 0.25,
      label.padding = unit(0.3, "lines"),
      label.r = unit(0.6, "lines")
    ) +
    annotate(
      "text",
      x = min(df[[x_var]], na.rm = TRUE),
      y = 0.95,
      label = paste0("Correlation: ", r_value_no_NI),
      hjust = 0,
      size = 4,
      color = "black"
    ) +
    annotate(
      "text",
      x = min(df[[x_var]], na.rm = TRUE),
      y = 0.90,
      label = paste0("Correlation: ", r_value_with_NI),
      hjust = 0,
      size = 4,
      color = "grey"
    ) +
    labs(title = paste(y_label, "vs.", x_label), x = x_label, y = y_label) +
    ylim(0, 1) +
    theme_classic() # Use classic theme

  # Save the plot
  fname = paste0(x_label, ".png")
  ggsave(filename = file.path(figpath, fname), plot = plot, width = 8, height = 6, dpi = 300)

}

mean_norm_sd_plot <- plot_with_both(
  "mean_norm_sd_alter_su_wealth_incl",
  "gini_wealth",
  "Average Normed SD of Neighbor Wealth",
  "Gini",
  "black"
)