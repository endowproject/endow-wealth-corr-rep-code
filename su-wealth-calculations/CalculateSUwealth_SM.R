# ============================================================
# CalculateSUwealth_SM.R
# ============================================================

site_name <- "SM"
get_data(site_name)

# --- Site-specific setup ---
final_item_list <- intersect(colnames(su), pc$item)
baseline_flat <- 500
exchange_rate_to_usd <- 0.2

# --- Checks ---
check_site_data(su, pc, final_item_list)

# --- Standard pipeline ---
su <- calculate_cash_value(su, final_item_list,
    baseline_flat = baseline_flat,
    exchange_rate_to_usd = exchange_rate_to_usd
)

item_summary <- make_item_summary(su, final_item_list, pc)

plot_item_values(su, final_item_list, pc, site_name, wealth_dir, exchange_rate_to_usd)

noise_list <- run_noise_trials(su, final_item_list,
    trials_for_noise, max_noise,
    baseline_flat = baseline_flat,
    exchange_rate_to_usd = exchange_rate_to_usd
)

report_and_save(su, noise_list, site_name, wealth_dir, item_summary = item_summary)
