# This script takes the degrees from su_master, and adjusts for them
# on the basis of size.

library(data.table)

path <- file.path(EndowGitHub, "DerivedData")

figpath <- file.path(file.path(EndowDropbox, "Figures"))

node_dat <- readRDS(file.path(path, "su_master.rds"))
node_dat <- node_dat[node_dat$network == "sum", ]

# ------------------------------------------------------

node_dat$log_su_size <- log(node_dat$su_size)

node_dat$log_in_degree  <- ifelse(!is.na(node_dat$in_degree)  & node_dat$in_degree  > 0,
                                   log(node_dat$in_degree), NA_real_)
node_dat$log_out_degree <- ifelse(!is.na(node_dat$out_degree) & node_dat$out_degree > 0,
                                   log(node_dat$out_degree), NA_real_)

# Key variables:
# log_wealth, log_su_size, site

# ------------------------------------------------------

node_dat <- as.data.table(node_dat)

node_dat[, c("in_degree_residual", "expected_log_indegree_size_1") := {
  ok <- !is.na(log_in_degree) & !is.na(log_su_size)

  resid_vec <- rep(NA_real_, .N)
  pred_val  <- NA_real_

  if (sum(ok) >= 2) {
    dat_fit <- data.frame(
      log_in_degree = log_in_degree[ok],
      log_su_size   = log_su_size[ok]
    )
    model <- lm(log_in_degree ~ log_su_size, data = dat_fit)
    resid_vec[ok] <- residuals(model)
    pred_val <- predict(model, newdata = data.frame(log_su_size = 0))
  }

  list(resid_vec, rep(pred_val, .N))
}, by = site]

node_dat[, c("out_degree_residual", "expected_log_outdegree_size_1") := {
  ok <- !is.na(log_out_degree) & !is.na(log_su_size)

  resid_vec <- rep(NA_real_, .N)
  pred_val  <- NA_real_

  dat_fit <- data.frame(
    log_out_degree = log_out_degree[ok],
    log_su_size    = log_su_size[ok]
  )
  model <- lm(log_out_degree ~ log_su_size, data = dat_fit)
  resid_vec[ok] <- residuals(model)
  pred_val <- predict(model, newdata = data.frame(log_su_size = 0))

  list(resid_vec, rep(pred_val, .N))
}, by = site]

node_dat$size_adjusted_indegree <-
  exp(node_dat$expected_log_indegree_size_1 + node_dat$in_degree_residual)

node_dat$size_adjusted_outdegree <-
  exp(node_dat$expected_log_outdegree_size_1 + node_dat$out_degree_residual)

# ------------------------------------------------------

# Reimport the full su_master dataset. merge back on to it, and overwrite.

su_master_full <- readRDS(file.path(path, "su_master.rds"))
# su_master_full <- as.data.table(su_master_full)

merge_vars <-
  node_dat[, .(su_id,
               in_degree_residual,
               out_degree_residual,
               expected_log_indegree_size_1,
               expected_log_outdegree_size_1,
               size_adjusted_indegree,
               size_adjusted_outdegree)]

su_master_full <- merge(su_master_full, merge_vars, by = "su_id", all.x = TRUE)

message("Saving su_master with degree-adjustments")
saveRDS(su_master_full, file.path(path, "su_master.rds"))