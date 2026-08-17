## Within-Site Fixed Effects Coefficients — Degree vs Wealth
##
## Runs a manual within-estimator: demean log wealth and log degree by
## site × household size, then standardise the demeaned variables within each
## site. Fits a site-interaction model to extract site-specific coefficients
## and plots them as forest plots.
##
## This is equivalent to a two-way fixed effects regression (site FE + size FE)
## but allows us to extract and plot site-level heterogeneity.
##
## Paper figures (Appendix):
##   size-controls/Fixed_Effects_Site_Coefficients_Outdegree.png
##   size-controls/Fixed_Effects_Site_Coefficients_Indegree.png

library(data.table)
library(ggplot2)

# ==============================================================================
# Setup
# ==============================================================================

path    <- file.path(EndowGitHub, "DerivedData")
figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "size-controls")

master_df <- readRDS(file.path(path, "su_master.rds"))
master_df <- master_df[master_df$network == "sum", ]
master_df <- data.table(master_df)

# ==============================================================================
# Helper: forest plot of site-specific FE coefficients
# ==============================================================================

format_pval <- function(p, digits = 4) {
      stopifnot(is.numeric(p), p >= 0, p <= 1)
      if (p < 0.0001) {
        "p < 0.0001"
      } else {
        paste0("p = ", formatC(p, digits = digits, format = "f"))
      }
    }

make_fe_plot <- function(results, filename, show_pooled = TRUE) {
  results <- results[order(results$coefficient), ]
  site_order <- factor(results$site, levels = unique(results$site))

  # ── Binomial test ─────────────────────────────────────────────────────────────
  n_pos <- sum(results$coefficient > 0)
  p_binom <- binom.test(
    n_pos,
    nrow(results),
    p = 0.5,
    alternative = "two.sided"
  )

  subtitle <- paste0("Binomial sign test: ", format_pval(p_binom$p.value))

  # ── Optional pooled RE estimate ───────────────────────────────────────────────
  if (show_pooled) {
    meta_fit <- metafor::rma(
      yi = coefficient,
      sei = std_error,
      data = results,
      method = "REML"
    )

    pooled_row <- data.frame(
      site = "Pooled",
      coefficient = as.numeric(meta_fit$b),
      std_error = as.numeric(meta_fit$se)
    )

    subtitle <- paste0(
      "Binomial sign test: ",
      format_pval(p_binom$p.value),
      "     |     ",
      "Pooled coefficient = ",
      sprintf("%.2f", pooled_row$coefficient),
      ", ",
      format_pval(as.numeric(meta_fit$pval))
    )

    plot_df <- dplyr::bind_rows(results, pooled_row) |>
      dplyr::mutate(
        site_order = factor(site, levels = c(levels(site_order), "Pooled")),
        conf_low = coefficient - 1.96 * std_error,
        conf_high = coefficient + 1.96 * std_error,
        is_pooled = site == "Pooled"
      )
  } else {
    plot_df <- dplyr::mutate(
      results,
      site_order = site_order,
      conf_low = coefficient - 1.96 * std_error,
      conf_high = coefficient + 1.96 * std_error,
      is_pooled = FALSE
    )
  }

  # ── Plot ──────────────────────────────────────────────────────────────────────
  n_sites <- sum(!plot_df$is_pooled)
  sep_x <- n_sites + 0.5

  plot <-
    ggplot(plot_df, aes(x = site_order, y = coefficient)) +

    {
      if (show_pooled) {
        geom_vline(
          xintercept = sep_x,
          colour = "grey60",
          linewidth = 0.4,
          linetype = "dashed"
        )
      }
    } +

    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high, linewidth = is_pooled),
      width = 0.2
    ) +
    scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 0.9)) +

    geom_point(
      data = ~ dplyr::filter(., !is_pooled),
      aes(color = coefficient > 0),
      size = 2
    ) +
    scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +

    {
      if (show_pooled) {
        geom_point(
          data = ~ dplyr::filter(., is_pooled),
          shape = 23,
          size = 4,
          fill = "#2c3e50",
          colour = "#2c3e50"
        )
      }
    } +

    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme_classic() +
    xlab("Site") +
    ylab("Correlation Coefficient and 95% CI") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(subtitle = subtitle)

  ggsave(
    file.path(figpath, filename),
    plot = plot,
    units = "in",
    width = 10,
    height = 5,
    dpi = 200
  )
}

# ==============================================================================
# Out-degree
# ==============================================================================

df_out <- master_df[out_degree_weighted != 0]
df_out[, log_out_degree_weighted := ifelse(out_degree_weighted > 0, log(out_degree_weighted), NA_real_)]

df_out[, mean_log_wealth    := mean(log_wealth, na.rm = TRUE), by = .(site, su_size)]
df_out[, mean_log_outdegree := mean(log_out_degree_weighted, na.rm = TRUE), by = .(site, su_size)]
df_out[, demean_log_wealth    := log_wealth - mean_log_wealth]
df_out[, demean_log_outdegree := log_out_degree_weighted - mean_log_outdegree]
df_out[, `:=`(
  std_demean_log_wealth    = scale(demean_log_wealth)[, 1],
  std_demean_log_outdegree = scale(demean_log_outdegree)[, 1]
), by = site]

coefs_out <- summary(
  lm(std_demean_log_wealth ~ std_demean_log_outdegree:site, data = df_out)
)$coefficients

rows_out  <- grepl("demean_log_outdegree", rownames(coefs_out))
est_out   <- coefs_out[rows_out, "Estimate"]
se_out    <- coefs_out[rows_out, "Std. Error"]
sites_out <- substr(names(est_out), nchar(names(est_out)) - 1, nchar(names(est_out)))

results_out <- data.frame(site = sites_out, coefficient = est_out, std_error = se_out,
                          row.names = NULL)

make_fe_plot(results_out, "Fixed_Effects_Site_Coefficients_Outdegree.png")

# ==============================================================================
# In-degree
# ==============================================================================

df_in <- master_df[in_degree_weighted != 0]
df_in[, log_in_degree_weighted := ifelse(in_degree_weighted > 0, log(in_degree_weighted), NA_real_)]

df_in[, mean_log_wealth   := mean(log_wealth, na.rm = TRUE), by = .(site, su_size)]
df_in[, mean_log_indegree := mean(log_in_degree_weighted, na.rm = TRUE), by = .(site, su_size)]
df_in[, demean_log_wealth   := log_wealth - mean_log_wealth]
df_in[, demean_log_indegree := log_in_degree_weighted - mean_log_indegree]
df_in[, `:=`(
  std_demean_log_wealth   = scale(demean_log_wealth)[, 1],
  std_demean_log_indegree = scale(demean_log_indegree)[, 1]
), by = site]

coefs_in <- summary(
  lm(std_demean_log_wealth ~ std_demean_log_indegree:site, data = df_in)
)$coefficients

rows_in  <- grepl("demean_log_indegree", rownames(coefs_in))
est_in   <- coefs_in[rows_in, "Estimate"]
se_in    <- coefs_in[rows_in, "Std. Error"]
sites_in <- substr(names(est_in), nchar(names(est_in)) - 1, nchar(names(est_in)))

results_in <- data.frame(site = sites_in, coefficient = est_in, std_error = se_in,
                         row.names = NULL)

make_fe_plot(results_in, "Fixed_Effects_Site_Coefficients_Indegree.png")
