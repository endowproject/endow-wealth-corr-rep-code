## Within-site multilevel models for Figure panels A-D
##
## Fits the random-slope-only multilevel analogues of the within-site
## correlation plots and writes paste-ready LaTeX tables.
##
## Output:
##   EndowDropbox/Tables/within_site_multilevel_tables.tex

library(dplyr)
library(lme4)

root_path <- if (exists("EndowGitHub")) EndowGitHub else getwd()
dropbox_path <- if (exists("EndowDropbox")) EndowDropbox else root_path
data_path <- file.path(root_path, "DerivedData")
table_dir <- file.path(dropbox_path, "Tables")
table_path <- file.path(table_dir, "within_site_multilevel_tables.tex")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

fit_within_site_model <- function(data, network_type, x_var, y_var) {
  model_data <- data |>
    filter(network == network_type) |>
    dplyr::select(site, x = all_of(x_var), y = all_of(y_var)) |>
    filter(is.finite(x), is.finite(y)) |>
    group_by(site) |>
    filter(n() >= 3, sd(x, na.rm = TRUE) > 0, sd(y, na.rm = TRUE) > 0) |>
    mutate(
      x_z = as.numeric(scale(x)),
      y_z = as.numeric(scale(y))
    ) |>
    ungroup() |>
    filter(is.finite(x_z), is.finite(y_z))

  fit <- lmer(
    y_z ~ 0 + x_z + (0 + x_z | site),
    data = model_data,
    control = lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )

  variance_components <- as.data.frame(VarCorr(fit))
  slope_sd <- variance_components$sdcor[
    variance_components$grp == "site" &
      variance_components$var1 == "x_z" &
      is.na(variance_components$var2)
  ]

  beta <- unname(fixef(fit)["x_z"])
  se <- coef(summary(fit))["x_z", "Std. Error"]

  data.frame(
    beta = beta,
    se = se,
    ci_low = beta - 1.96 * se,
    ci_high = beta + 1.96 * se,
    slope_sd = slope_sd,
    residual_sd = sigma(fit),
    n = nrow(model_data),
    sites = n_distinct(model_data$site),
    singular = isSingular(fit),
    stringsAsFactors = FALSE
  )
}

fmt_num <- function(x) sprintf("%.3f", x)
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_ci <- function(low, high) paste0("[", fmt_num(low), ", ", fmt_num(high), "]")

su_master <- readRDS(file.path(data_path, "su_master.rds"))

degree_specs <- data.frame(
  panel = c("Panel A", "Panel B"),
  column_label = c("Support Access p.c. rank", "Support Provision p.c. rank"),
  network = c("sum", "sum"),
  x_var = c("rank_out_degree_percap_weighted", "rank_in_degree_percap_weighted"),
  y_var = c("rank_wpc", "rank_wpc"),
  stringsAsFactors = FALSE
)

degree_results <- bind_rows(lapply(seq_len(nrow(degree_specs)), function(i) {
  cbind(
    degree_specs[i, c("panel", "column_label")],
    fit_within_site_model(
      su_master,
      degree_specs$network[i],
      degree_specs$x_var[i],
      degree_specs$y_var[i]
    )
  )
}))

iec <- read.csv(file.path(data_path, "IEC_Revised.csv")) |>
  group_by(site, network) |>
  mutate(rank_avg_frank_capita = percent_rank(avg_frank_capita)) |>
  ungroup()

aaw_specs <- data.frame(
  panel = c("Panel C", "Panel D"),
  column_label = c("$AAW_i$ of supporters rank", "$AAW_i$ of supportees rank"),
  network = c("sum", "rev_sum"),
  x_var = c("rank_avg_frank_capita", "rank_avg_frank_capita"),
  y_var = c("weighted_capita_rank", "weighted_capita_rank"),
  stringsAsFactors = FALSE
)

aaw_results <- bind_rows(lapply(seq_len(nrow(aaw_specs)), function(i) {
  cbind(
    aaw_specs[i, c("panel", "column_label")],
    fit_within_site_model(
      iec,
      aaw_specs$network[i],
      aaw_specs$x_var[i],
      aaw_specs$y_var[i]
    )
  )
}))

make_two_column_table <- function(
  results,
  caption,
  label,
  dependent_label,
  notes
) {
  c(
    "\\begin{table}[ht!]",
    "\\centering",
    paste0("\\caption{\\textbf{", caption, "}}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lcc}",
    "\\toprule",
    paste0(" & \\multicolumn{2}{c}{Dependent variable: ", dependent_label, "} \\\\"),
    "\\cmidrule(lr){2-3}",
    paste0(
      " & \\shortstack{",
      results$panel[1],
      "\\\\",
      results$column_label[1],
      "} & \\shortstack{",
      results$panel[2],
      "\\\\",
      results$column_label[2],
      "} \\\\"
    ),
    "\\midrule",
    paste0("Pooled slope ($\\beta$) & ", fmt_num(results$beta[1]), " & ", fmt_num(results$beta[2]), " \\\\"),
    paste0("Standard error & (", fmt_num(results$se[1]), ") & (", fmt_num(results$se[2]), ") \\\\"),
    paste0("95\\% CI & ", fmt_ci(results$ci_low[1], results$ci_high[1]), " & ",
           fmt_ci(results$ci_low[2], results$ci_high[2]), " \\\\"),
    "\\midrule",
    paste0("SD of site slopes ($\\tau$) & ", fmt_num(results$slope_sd[1]), " & ",
           fmt_num(results$slope_sd[2]), " \\\\"),
    paste0("Residual SD ($\\sigma$) & ", fmt_num(results$residual_sd[1]), " & ",
           fmt_num(results$residual_sd[2]), " \\\\"),
    paste0("Sharing units & ", fmt_int(results$n[1]), " & ", fmt_int(results$n[2]), " \\\\"),
    paste0("Sites & ", fmt_int(results$sites[1]), " & ", fmt_int(results$sites[2]), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "",
    "\\vspace{0.5em}",
    "\\begin{minipage}{0.92\\textwidth}",
    "\\footnotesize",
    paste0("\\emph{Notes}: ", notes),
    "\\end{minipage}",
    "\\end{table}"
  )
}

degree_notes <- paste(
  "This table reports estimates from varying-slope linear mixed models:",
  "$Y_{is} = \\beta_s X_{is} + \\varepsilon_{is}$, with $\\beta_s \\sim N(\\beta,\\tau^2)$ and $\\varepsilon_{is} \\sim N(0,\\sigma^2)$.",
  "Both the dependent variable and predictor are percentile ranks standardized within site.",
  "We omit intercepts because within-site standardization fixes each site's mean at zero.",
  "Support access (i.e., out-degree) and support provisioning (i.e., in-degree) are calculated on the composite network."
)

aaw_notes <- paste(
  "This table reports estimates from varying-slope linear mixed models:",
  "$Y_{is} = \\beta_s X_{is} + \\varepsilon_{is}$, with $\\beta_s \\sim N(\\beta,\\tau^2)$ and $\\varepsilon_{is} \\sim N(0,\\sigma^2)$.",
  "The predictor is the percentile rank of average alter wealth per capita, using alters who are supporters or supportees.",
  "Both the dependent variable and predictor are percentile ranks standardized within site.",
  "We omit intercepts because within-site standardization fixes each site's mean at zero.",
  "We use the composite network."
)

tex_lines <- c(
  make_two_column_table(
    degree_results,
    "Multilevel estimates corresponding to Figure~\\ref{fig:within-site-multipanel}, panels A and B.",
    "tab:within-site-multilevel-degree",
    "wealth per capita rank",
    degree_notes
  ),
  "",
  make_two_column_table(
    aaw_results,
    "Multilevel estimates corresponding to Figure~\\ref{fig:within-site-multipanel}, panels C and D.",
    "tab:within-site-multilevel-aaw",
    "wealth per capita rank",
    aaw_notes
  )
)

writeLines(tex_lines, table_path)
message("Wrote ", table_path)

print(
  bind_rows(
    cbind(table = "degree", degree_results),
    cbind(table = "aaw", aaw_results)
  )
)
