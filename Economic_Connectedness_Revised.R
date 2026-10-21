## The original economic connectedness script just uses overall wealth
## to rank sharing units in the SES distribution.
## This could be problematic since larger sharing units may have both
## more wealth and more connections.

## So in this script, we want to rank on the basis of wealth per capita
## and per adultand also weight the distribution by size. So e.g.,
## if a sharing unitaccounts for half of the adults in society, and is
## rich, that will bethe ONLY sharing unit marked as having above-median wealth.

## Additionally, we want to break down EC by layer.

library(cNORM)
library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)

path <- file.path(EndowGitHub, "DerivedData")

figpath <- file.path(file.path(EndowDropbox, "Figures"))

node_dat <- readRDS(file.path(path, "su_master.rds"))
# node_dat <- as.data.frame(node_dat)
node_dat <- node_dat[node_dat$network == "sum", ]

edges <- readRDS(file.path(path, "igraphs_for_each_layer_list.rds"))

layer_has_self_loops <-
  vapply(edges, function(layer) {
    if (!is.list(layer)) {
      return(FALSE)
    }

    any(vapply(layer, function(graph) {
      !(missing(graph) || is.null(graph)) &&
        inherits(graph, "igraph") &&
        any(which_loop(graph))
    }, logical(1)))
  }, logical(1))

stopifnot(!any(layer_has_self_loops))

# dealing with cases of NAs or 0s that can occur when, e.g., no able-bodied SU members
safe_weighted_rank <- function(x, weights) {
  valid <- !is.na(x) & !is.na(weights) & weights > 0
  result <- rep(NA_real_, length(x))
  result[valid] <- weighted.rank(x[valid], weights = weights[valid]) / sum(valid)
  result
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

safe_sum <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# ==============================================================================

node_wealth_data <-
  list()

for (site in unique(node_dat$site)) {

  node_wealth_data[[site]] <-
    node_dat[node_dat$site == site,
             c("su_id",
               "site",
               "wealth",
               "wealth_per_adult",
               "adult_count",
               "wealth_per_able",
               "able_count",
               "wealth_per_capita",
               "su_size",
               "rank_wealth",
               "size_adjusted_wealth")]

  # node_wealth_data[[site]] <- as.data.frame(node_wealth_data[[site]])

  #node_wealth_data[[site]]$rank_size_adjusted_wealth <-
  #  percent_rank(node_wealth_data[[site]]$size_adjusted_wealth)

  # for consistency, using safe_weighted_rank with weights of 1, rather than percent_rank (should be equiv)
  node_wealth_data[[site]]$rank_size_adjusted_wealth <-
    safe_weighted_rank(node_wealth_data[[site]]$size_adjusted_wealth, rep(1, nrow(node_wealth_data[[site]])))

  # can comment this out with use of safe_weighted_rank
  #node_wealth_data[[site]] <-
  #  node_wealth_data[[site]][!is.na(node_wealth_data[[site]]$wealth_per_adult) &
  #                                   !is.na(node_wealth_data[[site]]$adult_count), ]

  node_wealth_data[[site]]$weighted_rank <-
    safe_weighted_rank(node_wealth_data[[site]]$wealth_per_adult,
                        node_wealth_data[[site]]$adult_count)

  node_wealth_data[[site]]$weighted_able_rank <-
    safe_weighted_rank(node_wealth_data[[site]]$wealth_per_able,
                        node_wealth_data[[site]]$able_count)

  node_wealth_data[[site]]$weighted_capita_rank <-
    safe_weighted_rank(node_wealth_data[[site]]$wealth_per_capita,
                        node_wealth_data[[site]]$su_size)

  #print(paste0("Weighted distribution calculated for site: ", site))

}

## For sites IH, AV, DJ, able count is missing.
## For site HE, only male household head asked, so not meaningful count
node_wealth_data[["HE"]]$weighted_able_rank <- NA

# ==============================================================================

## One thing to be aware of, we want to use the weights in each layer because
## we are reweighting the distribution by size, so if you have more individual
## connections to a sharing unit, that should get more weight.

# all_layers <-
#   c("union",
#     "loan",
#     "behav",
#     "exchange",
#     "male",
#     "female",
#     "info",
#     "exchange_plus_female",
#     "main_supp",
#     "layer_sum",
#     "layer_sum_gender",
#     "layer_sum_externals",
#     "main_supp",
#     "main_supp_layer")

# male_net <-
#   edges[["male"]]
#
# female_net <-
#   edges[["female"]]
#
# loan_net <-
#   edges[["loan"]]
#
# exchange_net <-
#   edges[["exchange"]]

layers_to_use <-
  c("loan",
    "behav",
    "exchange",
    "info",
    "male",
    "female",
    # "layer_sum"
    "sum",
    "collapse_sum",
    "main_supp",
    "primary_kin",
    "secondary_kin",
    "primary_connectedkin_mainsupp",
    "secondary_connectedkin_mainsupp",
    "non_primary_connectedkin_mainsupp",
    "non_secondary_connectedkin_mainsupp",
    "primary_connectedkin_sum",
    "secondary_connectedkin_sum",
    "non_primary_connectedkin_sum",
    "non_secondary_connectedkin_sum")

## Elly wants use to reverse these layers as well:

rev_layers_to_use <-
  paste0("rev_", layers_to_use)

for (i in 1:length(rev_layers_to_use)) {

  new_layer_name <- rev_layers_to_use[i]
  old_layer_to_reverse <- layers_to_use[i]
  edges[[new_layer_name]] <- list()

  for (s in names(edges[[old_layer_to_reverse]])) {

    edges[[new_layer_name]][[s]] <-
      reverse_edges(edges[[old_layer_to_reverse]][[s]])

  }

}

layers_to_use <-
  c(layers_to_use,
    rev_layers_to_use)

IEC <- list()
EC <- list()

for (site in unique(node_dat$site)) {
  IEC[[site]] <- list()
  EC[[site]] <- list()
}

for (l in layers_to_use) {

  ed <- edges[[l]]

  site_list <- names(ed)

  for (s in site_list) {

    eds <- ed[[s]]

    w_edgelist <-
      data.table(cbind(as_edgelist(eds), E(eds)$weight))

    names(w_edgelist) <-
      c("su_id", "alter_id", "weight")

    # ## keep only sampled alters, may be redundant
    #
    # w_edgelist <-
    #   w_edgelist[w_edgelist$alter_id %in% unique(w_edgelist$su_id), ]

    ## merge own SU wealth

    ## this is the wealth-per-adult variable

    edge_node_merge <-
      merge(w_edgelist,
            node_wealth_data[[s]],
            by = "su_id",
            all.x = FALSE,
            all.y = FALSE)

    ## merge alter SU wealth
    alter_data <-
      node_wealth_data[[s]][, c("su_id",
                                      "wealth",
                                      "rank_wealth",
                                      "weighted_rank",
                                      "weighted_able_rank",
                                      "weighted_capita_rank",
                                      "rank_size_adjusted_wealth")]

    names(alter_data) <-
      c("alter_id",
        "alter_wealth",
        "alter_absolute_rank",
        "alter_rank",
        "alter_able_rank",
        "alter_capita_rank",
        "alter_size_adjusted_rank")

    edge_node_merge <-
      merge(edge_node_merge,
            alter_data,
            by = "alter_id",
            all.x = FALSE,
            all.y = FALSE)

    ## Construct IEC

    edge_node_merge <-
      data.table(edge_node_merge)

    edge_node_merge$weight <-
      as.numeric(edge_node_merge$weight)

    ### strict inequality is correct here:
    ### e.g., weighted.rank(c(1,2,3,4), c(1,1,1,1)) / 4 and consider that
    ### person with wealth 2 is below median

    iec_data <-
      edge_node_merge[,
                      .(rank_wealth = first(rank_wealth),
                        IEC_absolute = 2 * wmean(alter_absolute_rank > 0.5, weight),
                        avg_frank_absolute = wmean(alter_absolute_rank, weight),
                        IEC_q1_absolute = 4 * wmean(alter_absolute_rank <= 0.25, weight),
                        IEC_q2_absolute = 4 * wmean(alter_absolute_rank > 0.25 & alter_absolute_rank <= 0.5, weight),
                        IEC_q3_absolute = 4 * wmean(alter_absolute_rank > 0.5 & alter_absolute_rank <= 0.75, weight),
                        IEC_q4_absolute = 4 * wmean(alter_absolute_rank > 0.75, weight),
                        IEC = 2 * wmean(alter_rank > 0.5, weight),
                        avg_frank = wmean(alter_rank, weight),
                        median_frank = median(alter_rank, na.rm = TRUE),
                        sum_frank = safe_sum(alter_rank), # don't think weights make sense here, or big households will be doubly advantaged.
                        median_fwealth = median(alter_wealth, na.rm = TRUE),
                        sum_fwealth = safe_sum(alter_wealth),
                        weighted_rank = first(weighted_rank),
                        adult_count = first(adult_count),
                        IEC_able = 2 * wmean(alter_able_rank > 0.5, weight),
                        avg_frank_able = wmean(alter_able_rank, weight),
                        weighted_able_rank = first(weighted_able_rank),
                        able_count = first(able_count),
                        IEC_q1 = 4 * wmean(alter_rank <= 0.25, weight),
                        IEC_q2 = 4 * wmean(alter_rank > 0.25 & alter_rank <= 0.5, weight),
                        IEC_q3 = 4 * wmean(alter_rank > 0.5 & alter_rank <= 0.75, weight),
                        IEC_q4 = 4 * wmean(alter_rank > 0.75, weight),
                        IEC_q1_able = 4 * wmean(alter_able_rank <= 0.25, weight),
                        IEC_q2_able = 4 * wmean(alter_able_rank > 0.25 & alter_able_rank <= 0.5, weight),
                        IEC_q3_able = 4 * wmean(alter_able_rank > 0.5 & alter_able_rank <= 0.75, weight),
                        IEC_q4_able = 4 * wmean(alter_able_rank > 0.75, weight),
                        IEC_capita = 2 * wmean(alter_capita_rank > 0.5, weight),
                        avg_frank_capita = wmean(alter_capita_rank, weight),
                        weighted_capita_rank = first(weighted_capita_rank),
                        su_size = first(su_size),
                        IEC_q1_capita = 4 * wmean(alter_capita_rank <= 0.25, weight),
                        IEC_q2_capita = 4 * wmean(alter_capita_rank > 0.25 & alter_capita_rank <= 0.5, weight),
                        IEC_q3_capita = 4 * wmean(alter_capita_rank > 0.5 & alter_capita_rank <= 0.75, weight),
                        IEC_q4_capita = 4 * wmean(alter_capita_rank > 0.75, weight),
                        rank_size_adjusted_wealth = first(rank_size_adjusted_wealth),
                        IEC_size_adjusted = 2 * wmean(alter_size_adjusted_rank > 0.5, weight),
                        avg_frank_size_adjusted = wmean(alter_size_adjusted_rank, weight),
                        IEC_q1_size_adjusted = 4 * wmean(alter_size_adjusted_rank <= 0.25, weight),
                        IEC_q2_size_adjusted = 4 * wmean(alter_size_adjusted_rank > 0.25 & alter_size_adjusted_rank <= 0.5, weight),
                        IEC_q3_size_adjusted = 4 * wmean(alter_size_adjusted_rank > 0.5 & alter_size_adjusted_rank <= 0.75, weight),
                        IEC_q4_size_adjusted = 4 * wmean(alter_size_adjusted_rank > 0.75, weight)),
                      by = .(su_id, site)]

    iec_data$IRC <- iec_data$IEC / wmean(iec_data$IEC, iec_data$adult_count)
    iec_data$IRC_absolute <- iec_data$IEC_absolute / mean(iec_data$IEC_absolute, na.rm = TRUE)
    iec_data$IRC_capita <- iec_data$IEC_capita / wmean(iec_data$IEC_capita, iec_data$su_size)

    iec_data$layer <- l

    IEC[[s]][[l]] <- iec_data

    ### weak inequality is correct here:
    ### e.g., weighted.rank(c(1,2,3,4), c(1,1,1,1)) / 4 and consider that
    ### person with wealth 2 is below median

    # mean EC of below median wealth SUs
    ec_data <-
      iec_data[weighted_rank <= 0.5,
               .(EC = wmean(IEC, adult_count)),
               by = .(site)]

    ec_data$layer <- l

    EC[[s]][[l]] <- ec_data

    # mean EC of above median wealth SUs
    ec_data_high <-
      iec_data[weighted_rank > 0.5,
               .(EC_high = wmean(IEC, adult_count)),
               by = .(site)]

    ec_data_high$layer <- l

    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_high, by = c("site", "layer"), all.x = TRUE)

    # global mean EC
    ec_data_all <-
      iec_data[weighted_rank > 0,
               .(EC_all = wmean(IEC, adult_count)),
               by = .(site)]

    ec_data_all$layer <- l

    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_all, by = c("site", "layer"), all.x = TRUE)

    # Absolute wealth

    # mean EC of below median wealth SUs
    ec_data_absolute <-
      iec_data[rank_wealth <= 0.5,
               .(EC_absolute = mean(IEC_absolute, na.rm = TRUE)),
               by = .(site)]

    ec_data_absolute$layer <- l

    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_absolute, by = c("site", "layer"), all.x = TRUE)

    # mean EC of above median wealth SUs
    ec_data_absolute_high <-
      iec_data[rank_wealth > 0.5,
               .(EC_absolute_high = mean(IEC_absolute, na.rm = TRUE)),
               by = .(site)]

    ec_data_absolute_high$layer <- l

    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_absolute_high, by = c("site", "layer"), all.x = TRUE)

    # global mean EC
    ec_data_absolute_all <-
      iec_data[rank_wealth > 0,
               .(EC_absolute_all = mean(IEC_absolute, na.rm = TRUE)),
               by = .(site)]

    ec_data_absolute_all$layer <- l

    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_absolute_all, by = c("site", "layer"), all.x = TRUE)

    ### merge on the quartiles data and the able and capita data:

    EC_data_able <-
      iec_data[weighted_able_rank <= 0.5,
               .(EC_able = wmean(IEC_able, able_count)),
               by = .(site)]

    EC_data_capita <-
      iec_data[weighted_capita_rank <= 0.5,
               .(EC_capita = wmean(IEC_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], EC_data_able, by = "site", all.x = TRUE) # all.x = TRUE required because site IH does not have able counts.

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], EC_data_capita, by = "site", all.x = TRUE)

    EC_data_capita_high <-
      iec_data[weighted_capita_rank > 0.5,
               .(EC_capita_high = wmean(IEC_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], EC_data_capita_high, by = "site", all.x = TRUE)

    EC_data_capita_all <-
      iec_data[weighted_capita_rank > 0,
               .(EC_capita_all = wmean(IEC_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], EC_data_capita_all, by = "site", all.x = TRUE)

    ec_data_q1 <-
      iec_data[weighted_rank <= 0.25,
               .(EC_q1_to_q1 = wmean(IEC_q1, adult_count),
                 EC_q1_to_q2 = wmean(IEC_q2, adult_count),
                 EC_q1_to_q3 = wmean(IEC_q3, adult_count),
                 EC_q1_to_q4 = wmean(IEC_q4, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q1, by = "site")

    ec_data_q2 <-
      iec_data[weighted_rank > 0.25 & weighted_rank <= 0.5,
               .(EC_q2_to_q1 = wmean(IEC_q1, adult_count),
                 EC_q2_to_q2 = wmean(IEC_q2, adult_count),
                 EC_q2_to_q3 = wmean(IEC_q3, adult_count),
                 EC_q2_to_q4 = wmean(IEC_q4, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q2, by = "site")

    ec_data_q3 <-
      iec_data[weighted_rank > 0.5 & weighted_rank <= 0.75,
               .(EC_q3_to_q1 = wmean(IEC_q1, adult_count),
                 EC_q3_to_q2 = wmean(IEC_q2, adult_count),
                 EC_q3_to_q3 = wmean(IEC_q3, adult_count),
                 EC_q3_to_q4 = wmean(IEC_q4, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q3, by = "site")

    ec_data_q4 <-
      iec_data[weighted_rank > 0.75,
               .(EC_q4_to_q1 = wmean(IEC_q1, adult_count),
                 EC_q4_to_q2 = wmean(IEC_q2, adult_count),
                 EC_q4_to_q3 = wmean(IEC_q3, adult_count),
                 EC_q4_to_q4 = wmean(IEC_q4, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q4, by = "site")

    ec_data_q1_able <-
      iec_data[weighted_able_rank <= 0.25,
               .(EC_q1_to_q1_able = wmean(IEC_q1_able, able_count),
                 EC_q1_to_q2_able = wmean(IEC_q2_able, able_count),
                 EC_q1_to_q3_able = wmean(IEC_q3_able, able_count),
                 EC_q1_to_q4_able = wmean(IEC_q4_able, able_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q1_able, by = "site", all.x = TRUE)

    ec_data_q2_able <-
      iec_data[weighted_able_rank > 0.25 & weighted_able_rank <= 0.5,
               .(EC_q2_to_q1_able = wmean(IEC_q1_able, able_count),
                 EC_q2_to_q2_able = wmean(IEC_q2_able, able_count),
                 EC_q2_to_q3_able = wmean(IEC_q3_able, able_count),
                 EC_q2_to_q4_able = wmean(IEC_q4_able, able_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q2_able, by = "site", all.x = TRUE)

    ec_data_q3_able <-
      iec_data[weighted_able_rank > 0.5 & weighted_able_rank <= 0.75,
               .(EC_q3_to_q1_able = wmean(IEC_q1_able, able_count),
                 EC_q3_to_q2_able = wmean(IEC_q2_able, able_count),
                 EC_q3_to_q3_able = wmean(IEC_q3_able, able_count),
                 EC_q3_to_q4_able = wmean(IEC_q4_able, able_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q3_able, by = "site", all.x = TRUE)

    ec_data_q4_able <-
      iec_data[weighted_able_rank > 0.75,
               .(EC_q4_to_q1_able = wmean(IEC_q1_able, able_count),
                 EC_q4_to_q2_able = wmean(IEC_q2_able, able_count),
                 EC_q4_to_q3_able = wmean(IEC_q3_able, able_count),
                 EC_q4_to_q4_able = wmean(IEC_q4_able, able_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q4_able, by = "site", all.x = TRUE)

    ec_data_q1_capita <-
      iec_data[weighted_capita_rank <= 0.25,
               .(EC_q1_to_q1_capita = wmean(IEC_q1_capita, su_size),
                 EC_q1_to_q2_capita = wmean(IEC_q2_capita, su_size),
                 EC_q1_to_q3_capita = wmean(IEC_q3_capita, su_size),
                 EC_q1_to_q4_capita = wmean(IEC_q4_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q1_capita, by = "site", all.x = TRUE)

    ec_data_q2_capita <-
      iec_data[weighted_capita_rank > 0.25 & weighted_capita_rank <= 0.5,
               .(EC_q2_to_q1_capita = wmean(IEC_q1_capita, su_size),
                 EC_q2_to_q2_capita = wmean(IEC_q2_capita, su_size),
                 EC_q2_to_q3_capita = wmean(IEC_q3_capita, su_size),
                 EC_q2_to_q4_capita = wmean(IEC_q4_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q2_capita, by = "site", all.x = TRUE)

    ec_data_q3_capita <-
      iec_data[weighted_capita_rank > 0.5 & weighted_capita_rank <= 0.75,
               .(EC_q3_to_q1_capita = wmean(IEC_q1_capita, su_size),
                 EC_q3_to_q2_capita = wmean(IEC_q2_capita, su_size),
                 EC_q3_to_q3_capita = wmean(IEC_q3_capita, su_size),
                 EC_q3_to_q4_capita = wmean(IEC_q4_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q3_capita, by = "site", all.x = TRUE)

    ec_data_q4_capita <-
      iec_data[weighted_capita_rank > 0.75,
               .(EC_q4_to_q1_capita = wmean(IEC_q1_capita, su_size),
                 EC_q4_to_q2_capita = wmean(IEC_q2_capita, su_size),
                 EC_q4_to_q3_capita = wmean(IEC_q3_capita, su_size),
                 EC_q4_to_q4_capita = wmean(IEC_q4_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q4_capita, by = "site", all.x = TRUE)

    ## Absolute metrics:

    ec_data_q1_absolute <-
      iec_data[rank_wealth <= 0.25,
               .(EC_q1_to_q1_absolute = mean(IEC_q1_absolute, na.rm = TRUE),
                 EC_q1_to_q2_absolute = mean(IEC_q2_absolute, na.rm = TRUE),
                 EC_q1_to_q3_absolute = mean(IEC_q3_absolute, na.rm = TRUE),
                 EC_q1_to_q4_absolute = mean(IEC_q4_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q1_absolute, by = "site", all.x = TRUE)

    ec_data_q2_absolute <-
      iec_data[rank_wealth > 0.25 & rank_wealth <= 0.5,
               .(EC_q2_to_q1_absolute = mean(IEC_q1_absolute, na.rm = TRUE),
                 EC_q2_to_q2_absolute = mean(IEC_q2_absolute, na.rm = TRUE),
                 EC_q2_to_q3_absolute = mean(IEC_q3_absolute, na.rm = TRUE),
                 EC_q2_to_q4_absolute = mean(IEC_q4_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q2_absolute, by = "site", all.x = TRUE)

    ec_data_q3_absolute <-
      iec_data[rank_wealth > 0.5 & rank_wealth <= 0.75,
               .(EC_q3_to_q1_absolute = mean(IEC_q1_absolute, na.rm = TRUE),
                 EC_q3_to_q2_absolute = mean(IEC_q2_absolute, na.rm = TRUE),
                 EC_q3_to_q3_absolute = mean(IEC_q3_absolute, na.rm = TRUE),
                 EC_q3_to_q4_absolute = mean(IEC_q4_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q3_absolute, by = "site", all.x = TRUE)

    ec_data_q4_absolute <-
      iec_data[rank_wealth > 0.75,
               .(EC_q4_to_q1_absolute = mean(IEC_q1_absolute, na.rm = TRUE),
                 EC_q4_to_q2_absolute = mean(IEC_q2_absolute, na.rm = TRUE),
                 EC_q4_to_q3_absolute = mean(IEC_q3_absolute, na.rm = TRUE),
                 EC_q4_to_q4_absolute = mean(IEC_q4_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_q4_absolute, by = "site", all.x = TRUE)


    # global means for relative connectedness
    ec_data_all_capita <-
      iec_data[weighted_capita_rank > 0,
               .(EC_all_to_q1_capita = wmean(IEC_q1_capita, su_size),
                 EC_all_to_q2_capita = wmean(IEC_q2_capita, su_size),
                 EC_all_to_q3_capita = wmean(IEC_q3_capita, su_size),
                 EC_all_to_q4_capita = wmean(IEC_q4_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_all_capita, by = "site", all.x = TRUE)

    ec_data_all_absolute <-
      iec_data[rank_wealth > 0,
               .(EC_all_to_q1_absolute = mean(IEC_q1_absolute, na.rm = TRUE),
                 EC_all_to_q2_absolute = mean(IEC_q2_absolute, na.rm = TRUE),
                 EC_all_to_q3_absolute = mean(IEC_q3_absolute, na.rm = TRUE),
                 EC_all_to_q4_absolute = mean(IEC_q4_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], ec_data_all_absolute, by = "site", all.x = TRUE)

    # average friend ranks

    frank_data_all_adult <-
      iec_data[weighted_rank > 0,
               .(avg_frank_all_adult = wmean(avg_frank, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_all_adult, by = "site", all.x = TRUE)

    frank_data_all_capita <-
      iec_data[weighted_capita_rank > 0,
               .(avg_frank_all_capita = wmean(avg_frank_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_all_capita, by = "site", all.x = TRUE)

    frank_data_all_absolute <-
      iec_data[rank_wealth > 0,
               .(avg_frank_all_absolute = mean(avg_frank_absolute, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_all_absolute, by = "site", all.x = TRUE)

    frank_data_low_adult <-
      iec_data[weighted_rank <= 0.5,
               .(avg_frank_low_adult = wmean(avg_frank, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_low_adult, by = "site", all.x = TRUE)

    frank_data_low_capita <-
      iec_data[weighted_capita_rank <= 0.5,
               .(avg_frank_low_capita = wmean(avg_frank_capita, su_size),
                 avg_sum_frank_low_capita = wmean(sum_frank, su_size),
                 avg_sum_fwealth_low_capita = wmean(sum_fwealth, su_size),
                 median_frank_low_capita = wmean(median_frank, su_size),
                 median_fwealth_low_capita = wmean(median_fwealth, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_low_capita, by = "site", all.x = TRUE)

    frank_data_low_absolute <-
      iec_data[rank_wealth <= 0.5,
               .(avg_frank_low_absolute = mean(avg_frank_absolute, na.rm = TRUE),
                 avg_sum_fwealth_low_absolute = mean(sum_fwealth, na.rm = TRUE),
                 median_frank_low_absolute = mean(median_frank, na.rm = TRUE),
                 median_fwealth_low_absolute = mean(median_fwealth, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_low_absolute, by = "site", all.x = TRUE)

    frank_data_high_adult <-
      iec_data[weighted_rank > 0.5,
               .(avg_frank_high_adult = wmean(avg_frank, adult_count)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_high_adult, by = "site", all.x = TRUE)

    frank_data_high_capita <-
      iec_data[weighted_capita_rank > 0.5,
               .(avg_frank_high_capita = wmean(avg_frank_capita, su_size),
                 avg_sum_frank_high_capita = wmean(sum_frank, su_size),
                 avg_sum_fwealth_high_capita = wmean(sum_fwealth, su_size),
                 median_frank_high_capita = wmean(median_frank, su_size),
                 median_fwealth_high_capita = wmean(median_fwealth, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_high_capita, by = "site", all.x = TRUE)

    frank_data_high_absolute <-
      iec_data[rank_wealth > 0.5,
               .(avg_frank_high_absolute = mean(avg_frank_absolute, na.rm = TRUE),
                 avg_sum_fwealth_high_absolute = mean(sum_fwealth, na.rm = TRUE),
                 median_frank_high_absolute = mean(median_frank, na.rm = TRUE),
                 median_fwealth_high_absolute = mean(median_fwealth, na.rm = TRUE)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_high_absolute, by = "site", all.x = TRUE)

    # Average friend rank by quartile for capita
    frank_data_q1_capita <-
      iec_data[weighted_capita_rank <= 0.25,
               .(avg_frank_q1_capita = wmean(avg_frank_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_q1_capita, by = "site", all.x = TRUE)

    frank_data_q2_capita <-
      iec_data[weighted_capita_rank > 0.25 & weighted_capita_rank <= 0.5,
               .(avg_frank_q2_capita = wmean(avg_frank_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_q2_capita, by = "site", all.x = TRUE)

    frank_data_q3_capita <-
      iec_data[weighted_capita_rank > 0.5 & weighted_capita_rank <= 0.75,
               .(avg_frank_q3_capita = wmean(avg_frank_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_q3_capita, by = "site", all.x = TRUE)

    frank_data_q4_capita <-
      iec_data[weighted_capita_rank > 0.75,
               .(avg_frank_q4_capita = wmean(avg_frank_capita, su_size)),
               by = .(site)]

    EC[[s]][[l]] <-
      merge(EC[[s]][[l]], frank_data_q4_capita, by = "site", all.x = TRUE)

    # Calculate EC with size-adjusted wealth

    # Mean EC of below median size-adjusted wealth SUs
    ec_data_size_adjusted <-
      iec_data[rank_size_adjusted_wealth <= 0.5,
               .(EC_size_adjusted = mean(IEC_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_size_adjusted$layer <- l

    # Mean EC of above median size-adjusted wealth SUs
    ec_data_size_adjusted_high <-
      iec_data[rank_size_adjusted_wealth > 0.5,
               .(EC_size_adjusted_high = mean(IEC_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_size_adjusted_high$layer <- l

    # Global mean EC for size-adjusted wealth
    ec_data_size_adjusted_all <-
      iec_data[rank_size_adjusted_wealth > 0,
               .(EC_size_adjusted_all = mean(IEC_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_size_adjusted_all$layer <- l

    # Quartile-specific EC calculations for size-adjusted wealth
    ec_data_q1_size_adjusted <-
      iec_data[rank_size_adjusted_wealth <= 0.25,
               .(EC_q1_to_q1_size_adjusted = mean(IEC_q1_size_adjusted, na.rm = TRUE),
                 EC_q1_to_q2_size_adjusted = mean(IEC_q2_size_adjusted, na.rm = TRUE),
                 EC_q1_to_q3_size_adjusted = mean(IEC_q3_size_adjusted, na.rm = TRUE),
                 EC_q1_to_q4_size_adjusted = mean(IEC_q4_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_q2_size_adjusted <-
      iec_data[rank_size_adjusted_wealth > 0.25 & rank_size_adjusted_wealth <= 0.5,
               .(EC_q2_to_q1_size_adjusted = mean(IEC_q1_size_adjusted, na.rm = TRUE),
                 EC_q2_to_q2_size_adjusted = mean(IEC_q2_size_adjusted, na.rm = TRUE),
                 EC_q2_to_q3_size_adjusted = mean(IEC_q3_size_adjusted, na.rm = TRUE),
                 EC_q2_to_q4_size_adjusted = mean(IEC_q4_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_q3_size_adjusted <-
      iec_data[rank_size_adjusted_wealth > 0.5 & rank_size_adjusted_wealth <= 0.75,
               .(EC_q3_to_q1_size_adjusted = mean(IEC_q1_size_adjusted, na.rm = TRUE),
                 EC_q3_to_q2_size_adjusted = mean(IEC_q2_size_adjusted, na.rm = TRUE),
                 EC_q3_to_q3_size_adjusted = mean(IEC_q3_size_adjusted, na.rm = TRUE),
                 EC_q3_to_q4_size_adjusted = mean(IEC_q4_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    ec_data_q4_size_adjusted <-
      iec_data[rank_size_adjusted_wealth > 0.75,
               .(EC_q4_to_q1_size_adjusted = mean(IEC_q1_size_adjusted, na.rm = TRUE),
                 EC_q4_to_q2_size_adjusted = mean(IEC_q2_size_adjusted, na.rm = TRUE),
                 EC_q4_to_q3_size_adjusted = mean(IEC_q3_size_adjusted, na.rm = TRUE),
                 EC_q4_to_q4_size_adjusted = mean(IEC_q4_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    # Average friend rank for size-adjusted wealth
    frank_data_all_size_adjusted <-
      iec_data[rank_size_adjusted_wealth > 0,
               .(avg_frank_all_size_adjusted = mean(avg_frank_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    frank_data_low_size_adjusted <-
      iec_data[rank_size_adjusted_wealth <= 0.5,
               .(avg_frank_low_size_adjusted = mean(avg_frank_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    frank_data_high_size_adjusted <-
      iec_data[rank_size_adjusted_wealth > 0.5,
               .(avg_frank_high_size_adjusted = mean(avg_frank_size_adjusted, na.rm = TRUE)),
               by = .(site)]

    # Merge all size-adjusted EC data
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_size_adjusted, by = c("site", "layer"), all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_size_adjusted_high, by = c("site", "layer"), all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_size_adjusted_all, by = c("site", "layer"), all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_q1_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_q2_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_q3_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], ec_data_q4_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], frank_data_all_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], frank_data_low_size_adjusted, by = "site", all.x = TRUE)
    EC[[s]][[l]] <- merge(EC[[s]][[l]], frank_data_high_size_adjusted, by = "site", all.x = TRUE)

  }

}

# ==============================================================================

## Roll up the EC and IEC lists.

list_to_bind <- list()
iec_list_to_bind <- list()

for (s in unique(node_dat$site)) {

  list_to_bind[[s]] <-
    bind_rows(EC[[s]])

  iec_list_to_bind[[s]] <-
    bind_rows(IEC[[s]])

}

EC_dataset <- bind_rows(list_to_bind)
IEC_dataset <- bind_rows(iec_list_to_bind)

IEC_dataset$network <-
  IEC_dataset$layer

write.csv(EC_dataset, file.path(path, "EC_Revised.csv"), row.names = FALSE)
write.csv(IEC_dataset, file.path(path, "IEC_Revised.csv"), row.names = FALSE)

EC_reshape <-
  reshape(EC_dataset,
          idvar = "site",
          timevar = "layer",
          direction = "wide")

write.csv(EC_reshape, file.path(path, "EC_Revised_Reshape.csv"), row.names = FALSE)

message(paste0("EC and IEC Revised datasets written to ", path))

# ==============================================================================

## Plot EC by site and layer.

## Need to update with the reverse layers as well

EC_dataset$layer_factor <-
  factor(EC_dataset$layer,
         levels = c("loan",
                    "behav",
                    "exchange",
                    "info",
                    "male",
                    "female",
                    "sum",
                    "collapse_sum",
                    "main_supp"))

EC_dataset <-
  EC_dataset[!is.na(EC_dataset$layer_factor), ]

plot <-
  ggplot(EC_dataset, aes(x = layer_factor, y = EC, fill = layer_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ site) +      # Create separate facets for each site
  labs(title = "Economic connectedness by network layer and site",
       y = "Economic Connectedness",
       x = " ") +
  theme_minimal() + # Apply a clean theme
  theme(axis.text.x = element_blank(),   # Remove x-axis text
        axis.ticks.x = element_blank()) + # Remove x-axis ticks
  expand_limits(y = c(0, 2)) +
  scale_fill_discrete(
    name = "Network",
    labels = c("behav" = "Work",
               "exchange" = "Exchange",
               "loan" = "Loan",
               "info" = "Information",
               "female" = "Female",
               "male" = "Male",
               "sum" = "Sum",
               "collapse_sum" = "Collapsed Sum",
               "main_supp" = "Main Supp")
  )

ggsave(file.path(figpath, "EC_By_Network_And_Site.pdf"),
       plot,
       width = 10,
       height = 10,
       units = "in")

plot2 <-
  ggplot(EC_dataset, aes(x = layer_factor, y = EC_able, fill = layer_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ site) +      # Create separate facets for each site
  labs(title = "Economic connectedness by network layer and site",
       y = "Economic Connectedness",
       x = " ") +
  theme_minimal() +         # Apply a clean theme
  theme(axis.text.x = element_blank(),   # Remove x-axis text
        axis.ticks.x = element_blank()) + # Remove x-axis ticks
  expand_limits(y = c(0, 2)) +
  scale_fill_discrete(
    name = "Network",
    labels = c("behav" = "Work",
               "exchange" = "Exchange",
               "loan" = "Loan",
               "info" = "Information",
               "female" = "Female",
               "male" = "Male",
               "sum" = "Sum",
               "collapse_sum" = "Collapsed Sum",
               "main_supp" = "Main Supp")
  )

ggsave(file.path(figpath, "EC_able_By_Network_And_Site.pdf"),
       plot2,
       width = 10,
       height = 10,
       units = "in")

plot2 <-
  ggplot(EC_dataset, aes(x = layer_factor, y = EC_capita, fill = layer_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ site) +      # Create separate facets for each site
  labs(title = "Economic connectedness by network layer and site",
       y = "Economic Connectedness",
       x = " ") +
  theme_minimal() +         # Apply a clean theme
  theme(axis.text.x = element_blank(),   # Remove x-axis text
        axis.ticks.x = element_blank()) + # Remove x-axis ticks
  expand_limits(y = c(0, 2)) +
  scale_fill_discrete(
    name = "Network",
    labels = c("behav" = "Work",
               "exchange" = "Exchange",
               "loan" = "Loan",
               "info" = "Information",
               "female" = "Female",
               "male" = "Male",
               "sum" = "Sum",
               "collapse_sum" = "Collapsed Sum",
               "main_supp" = "Main Supp")
  )

ggsave(file.path(figpath, "EC_capita_By_Network_And_Site.pdf"),
       plot2,
       width = 10,
       height = 10,
       units = "in")

cor(EC_dataset$EC, EC_dataset$EC_capita)

# Scatter plot of EC vs EC_capita

ggplot(EC_dataset, aes(x = EC, y = EC_capita)) +
  geom_text(aes(label = site), color = "blue", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(
    title = "Scatter Plot of EC_adult vs EC_capita",
    x = "EC_adult",
    y = "EC_capita"
  ) +
  theme_minimal()

# cor(EC_dataset[EC_dataset$layer == "union", ]$EC,
#     EC_dataset[EC_dataset$layer == "union", ]$EC_capita)

ggplot(EC_dataset[EC_dataset$layer == "union", ], aes(x = EC, y = EC_capita)) +
  geom_text(aes(label = site), color = "blue", size = 4, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = F) +
  labs(
    title = "Scatter Plot of EC_adult vs EC_capita",
    x = "EC_adult",
    y = "EC_capita"
  ) +
  coord_cartesian(xlim = c(0.6, 1.5), ylim = c(0.6, 1.5)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()

# =======================================================

# Reversed

EC_dataset <- read.csv(file.path(path, "EC_Revised.csv"))

EC_dataset$layer_factor <-
  factor(EC_dataset$layer,
         levels = c("sum",
                    "collapse_sum",
                    "main_supp",
                    "rev_sum",
                    "rev_collapse_sum",
                    "rev_main_supp"))

EC_dataset <-
  EC_dataset[!is.na(EC_dataset$layer_factor), ]

plot <-
  ggplot(EC_dataset, aes(x = layer_factor, y = EC_capita, fill = layer_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ site) +      # Create separate facets for each site
  labs(title = "Economic connectedness by network layer and site",
       y = "Economic Connectedness",
       x = " ") +
  theme_minimal() + # Apply a clean theme
  theme(axis.text.x = element_blank(),   # Remove x-axis text
        axis.ticks.x = element_blank()) + # Remove x-axis ticks
  expand_limits(y = c(0, 2)) +
  scale_fill_discrete(
    name = "Network",
    labels = c("sum" = "Ego-Adjusted Sum",
               "collapse_sum" = "Ego-Adjusted Collapsed Sum",
               "main_supp" = "Unadjusted Sum",
               "rev_sum" = "Reverse: Ego-Adjusted Sum",
               "rev_collapse_sum" = "Reverse: Ego-Adjusted Collapsed Sum",
               "rev_main_supp" = "Reverse: Unadjusted Sum")
  )

ggsave(file.path(figpath, "EC_capita_By_Network_And_Site_Reverse.pdf"),
       plot,
       width = 10,
       height = 10,
       units = "in")