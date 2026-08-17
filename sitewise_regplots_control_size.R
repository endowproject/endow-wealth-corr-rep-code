## Within-Site Sitewise Regression Plots — Size Controls
# Regresses wealth on degree after partialling out sharing-unit size.
# Produces size-residualised forest plots.
#
# Paper figures (Appendix):
#   size-controls/Sitewise_sum_residuals_* — size-residualised plots`
#   notably:
#   Sitewise_sum_residuals_log_in_degree_on_log_size_residuals_log_wealth_on_log_size.png
#   Sitewise_sum_residuals_log_out_degree_on_log_size_residuals_log_wealth_on_log_size.png

library(data.table)
library(dplyr)
library(tidyr)
library(DescTools) ## for Gini() function
library(igraph)
library(stargazer)
library(plm)
library(stringr)
library(ggplot2)

# Load data
path <-
  file.path(EndowGitHub,
            "DerivedData")

master_df <- readRDS(file.path(path, "su_master.rds"))

# Save paths
tables_path <- file.path(EndowDropbox, "Tables", "sitewise")
figpath <- file.path(EndowDropbox, "Figures", "paper-1st-draft", "size-controls")

unique_sites <- unique(master_df$site)

format_pval <- function(p, digits = 4) {
      stopifnot(is.numeric(p), p >= 0, p <= 1)
      if (p < 0.0001) {
        "p < 0.0001"
      } else {
        paste0("p = ", formatC(p, digits = digits, format = "f"))
      }
    }


## The idea is to control for size.
## So have degree on one side, wealth on the other side, but partial out size.
## i.e., regress degree on log(su_size), take the residuals from that regression.
## then regress log(wealth) on log(su_size)

getResiduals <- function(x, y) {
  ok <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)

  resid_vec <- rep(NA_real_, length(x))

  if (sum(ok) >= 2 && sd(x[ok]) > 0 && sd(y[ok]) > 0) {
    dat_fit <- data.frame(x = x[ok], y = y[ok])
    model <- lm(y ~ x, data = dat_fit)
    resid_vec[ok] <- residuals(model)
  }

  resid_vec
}

master_df$log_degree_weighted <- ifelse(
  !is.na(master_df$degree_weighted) & master_df$degree_weighted > 0,
  log(master_df$degree_weighted), NA_real_
)
master_df$log_su_size <- ifelse(
  !is.na(master_df$su_size) & master_df$su_size > 0,
  log(master_df$su_size), NA_real_
)
master_df$log_in_degree_weighted <- ifelse(
  !is.na(master_df$in_degree_weighted) & master_df$in_degree_weighted > 0,
  log(master_df$in_degree_weighted), NA_real_
)
master_df$log_out_degree_weighted <- ifelse(
  !is.na(master_df$out_degree_weighted) & master_df$out_degree_weighted > 0,
  log(master_df$out_degree_weighted), NA_real_
)
master_df$log_adults <- ifelse(
  !is.na(master_df$adult_count) & master_df$adult_count > 0,
  log(master_df$adult_count), NA_real_
)

master_df <- data.table(master_df)

master_df[, residuals_log_wealth_on_log_size := getResiduals(log_su_size, log_wealth), by = .(site, network)]
master_df[, residuals_log_degree_on_log_size := getResiduals(log_su_size, log_degree_weighted), by = .(site, network)]

master_df[, residuals_log_wealth_on_log_size := getResiduals(log_su_size, log_wealth), by = .(site, network)]
master_df[, residuals_log_in_degree_on_log_size := getResiduals(log_su_size, log_in_degree_weighted), by = .(site, network)]
master_df[, residuals_log_out_degree_on_log_size := getResiduals(log_su_size, log_out_degree_weighted), by = .(site, network)]

master_df[, residuals_log_in_degree_on_log_adults := getResiduals(log_adults, log_in_degree_weighted), by = .(site, network)]
master_df[, residuals_log_out_degree_on_log_adults := getResiduals(log_adults, log_out_degree_weighted), by = .(site, network)]
master_df[, residuals_log_wealth_on_log_adults := getResiduals(log_adults, log_wealth), by = .(site, network)]


## Plot

num_loops <- 10
make_sitewise_plots <- function(
  x,
  y,
  network_type,
  ordered = TRUE,
  show_pooled = TRUE
) {
  # ── Shuffle test setup ────────────────────────────────────────────────────────
  results_loops <- list()
  for (loop_num in 1:num_loops) {
    results_loops[[loop_num]] <-
      rep(NA_real_, length(unique_sites))
  }

  results <-
    data.frame(
      site = character(),
      coefficient = numeric(),
      std_error = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

  network_df <- master_df %>% filter(network == network_type)

  s_index <- 0

  for (site in unique_sites) {
    s_index <- s_index + 1

    data_site <- filter(network_df, site == !!site)
    data_site <- data.frame(data_site)

    xn <- as.numeric(data_site[, x])
    yn <- as.numeric(data_site[, y])
    complete_rows <- is.finite(xn) & is.finite(yn)
    if (sum(complete_rows) < 3 ||
        sd(xn[complete_rows]) == 0 ||
        sd(yn[complete_rows]) == 0) {
      next
    }

    data_site <- data_site[complete_rows, ]
    xn <- xn[complete_rows]
    yn <- yn[complete_rows]

    if (all(xn[!is.na(xn)] - mean(xn[!is.na(xn)]) == 0)) {
      next
    }

    std_x <- (xn - mean(xn, na.rm = TRUE)) / sd(xn, na.rm = TRUE)
    std_y <- (yn - mean(yn, na.rm = TRUE)) / sd(yn, na.rm = TRUE)

    formula <- as.formula(paste("std_y ~", "std_x"))
    model <- lm(formula, data = data_site)

    coef_summary <- summary(model)$coefficients
    coef <- coef_summary["std_x", "Estimate"]
    std_err <- coef_summary["std_x", "Std. Error"]
    p_value <- coef_summary["std_x", "Pr(>|t|)"]

    results <- rbind(
      results,
      data.frame(
        site = site,
        coefficient = coef,
        std_error = std_err,
        p_value = p_value
      )
    )

    # ── Shuffle test ────────────────────────────────────────────────────────────
    results_vec <- vector(mode = "numeric", length = num_loops)
    shuffle_formula <- as.formula(paste("std_y ~", "std_x_reshuffle"))

    for (loop_num in 1:num_loops) {
      std_x_reshuffle <- std_x[sample(
        1:length(std_x),
        length(std_x),
        replace = FALSE
      )]
      model_shuffle <- lm(shuffle_formula)
      coef_summary <- summary(model_shuffle)$coefficients
      coef <- coef_summary["std_x_reshuffle", "Estimate"]
      results_vec[loop_num] <- coef
      results_loops[[loop_num]][s_index] <- coef
    }
  }

  if (ordered) {
    results <- results[order(results$coefficient), ]
  }

  # ── Binomial test ─────────────────────────────────────────────────────────────
  num_positive_coefs <- sum(results$coefficient > 0)
  num_coefs <- nrow(results)
  binom_test <- binom.test(
    x = num_positive_coefs,
    n = num_coefs,
    p = 0.5,
    alternative = "two.sided"
  )
  #print(paste0(
  #  "P so many positive coefs given null of 0.5::::   ",
  #  binom_test$p.value
  #))

  # ── Shuffle test p-value ──────────────────────────────────────────────────────
  placebo_results_under_null <- sapply(results_loops, function(x) sum(x > 0, na.rm = TRUE))
  p_val_shuffle_test <- sum(placebo_results_under_null > num_positive_coefs) /
    num_loops
  #print(paste0("P value from shuffle test::::   ", p_val_shuffle_test))

  subtitle <- paste0("Binomial sign test: ", format_pval(binom_test$p.value))

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
      std_error = as.numeric(meta_fit$se),
      p_value = as.numeric(meta_fit$pval)
    )

    subtitle <- paste0(
      "Binomial sign test: ",
      format_pval(binom_test$p.value),
      "     |     ",
      "Pooled coefficient = ",
      sprintf("%.2f", pooled_row$coefficient),
      ", ",
      format_pval(pooled_row$p_value)#,
      #", I\u00b2 = ",
      #sprintf("%.0f%%", meta_fit$I2)
    )

    plot_df <- rbind(results, pooled_row) |>
      dplyr::mutate(
        site_order = factor(site, levels = c(unique(results$site), "Pooled")),
        conf_low = pmax(coefficient - 1.96 * std_error, -1),
        conf_high = pmin(coefficient + 1.96 * std_error, 1),
        is_pooled = site == "Pooled"
      )
  } else {
    plot_df <- results |>
      dplyr::mutate(
        site_order = factor(site, levels = unique(results$site)),
        conf_low = pmax(coefficient - 1.96 * std_error, -1),
        conf_high = pmin(coefficient + 1.96 * std_error, 1),
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
      aes(colour = coefficient > 0),
      size = 2
    ) +
    scale_colour_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +

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

    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +

    theme_classic() +
    scale_y_continuous(limits = c(-1, 1)) +
    xlab("Site") +
    ylab("Correlation Coefficient and 95% CI") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(subtitle = subtitle)

  ggsave(
    file.path(
      figpath,
      paste0("Sitewise_", network_type, "_", x, "_", y, ".png")
    ),
    plot = plot,
    units = "in",
    width = 10,
    height = 5,
    dpi = 200
  )
}

make_sitewise_plots("residuals_log_degree_on_log_size", "residuals_log_wealth_on_log_size", "sum")
make_sitewise_plots("residuals_log_degree_on_log_size", "residuals_log_wealth_on_log_size", "collapse_sum")
make_sitewise_plots("residuals_log_degree_on_log_size", "residuals_log_wealth_on_log_size", "main_supp")

make_sitewise_plots("residuals_log_out_degree_on_log_size", "residuals_log_wealth_on_log_size", "sum")
make_sitewise_plots("residuals_log_in_degree_on_log_size", "residuals_log_wealth_on_log_size", "sum")

make_sitewise_plots("residuals_log_out_degree_on_log_adults", "residuals_log_wealth_on_log_adults", "sum")
make_sitewise_plots("residuals_log_in_degree_on_log_adults", "residuals_log_wealth_on_log_adults", "sum")

master_df[, rank_residuals_log_wealth_on_log_size := percent_rank(residuals_log_wealth_on_log_size), by = .(site, network)]
master_df[, rank_residuals_log_degree_on_log_size := percent_rank(residuals_log_degree_on_log_size), by = .(site, network)]

make_sitewise_plots("rank_residuals_log_degree_on_log_size", "rank_residuals_log_wealth_on_log_size", "sum")
make_sitewise_plots("rank_residuals_log_degree_on_log_size", "rank_residuals_log_wealth_on_log_size", "collapse_sum")
make_sitewise_plots("rank_residuals_log_degree_on_log_size", "rank_residuals_log_wealth_on_log_size", "main_supp")

master_df[, residuals_log_wealth_on_log_adults := getResiduals(log_adults, log_wealth), by = .(site, network)]
master_df[, residuals_log_degree_on_log_adults := getResiduals(log_adults, log_degree_weighted), by = .(site, network)]

make_sitewise_plots("residuals_log_degree_on_log_adults", "residuals_log_wealth_on_log_adults", "sum")
make_sitewise_plots("residuals_log_degree_on_log_adults", "residuals_log_wealth_on_log_adults", "collapse_sum")
make_sitewise_plots("residuals_log_degree_on_log_adults", "residuals_log_wealth_on_log_adults", "main_supp")