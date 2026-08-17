# This script takes the suid wealths from su_master, and adjusts for them
# on the basis of size.

library(data.table)

path <- file.path(EndowGitHub, "DerivedData")

figpath <- file.path(file.path(EndowDropbox, "Figures"))

node_dat <- readRDS(file.path(path, "su_master.rds"))
node_dat <- node_dat[node_dat$network == "sum", ]

# ------------------------------------------------------

node_dat$log_su_size <- log(node_dat$su_size)

# Key variables:
# log_wealth, log_su_size, site

# ------------------------------------------------------

node_dat <- as.data.table(node_dat)

node_dat[, c("wealth_residual", "expected_log_wealth_size_1") := {
  ok <- !is.na(log_wealth) & !is.na(log_su_size)

  resid_vec <- rep(NA_real_, .N)
  pred_val  <- NA_real_

  if (sum(ok) >= 2) {
    dat_fit <- data.frame(
      log_wealth   = log_wealth[ok],
      log_su_size  = log_su_size[ok]
    )
    model <- lm(log_wealth ~ log_su_size, data = dat_fit)
    resid_vec[ok] <- residuals(model)
    pred_val <- predict(model, newdata = data.frame(log_su_size = 0))
  }

  list(resid_vec, rep(pred_val, .N))
}, by = site]

node_dat$size_adjusted_wealth <-
  exp(node_dat$expected_log_wealth_size_1 + node_dat$wealth_residual)

# ------------------------------------------------------

# Reimport the full su_master dataset. merge back on to it, and overwrite.

su_master_full <- readRDS(file.path(path, "su_master.rds"))
# su_master_full <- as.data.table(su_master_full)

merge_vars <- node_dat[, .(su_id, wealth_residual, expected_log_wealth_size_1, size_adjusted_wealth)]

su_master_full <- merge(su_master_full, merge_vars, by = "su_id", all.x = TRUE)

message("Saving su_master with size-adjusted wealth")
saveRDS(su_master_full, file.path(path, "su_master.rds"))