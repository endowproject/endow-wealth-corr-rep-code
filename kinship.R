## Kinship Network Data Generation
##
## Creates sitelevel_kinship.csv

library(dplyr)
library(tidyr)
library(DescTools)
library(igraph)
library(stargazer)
library(plm)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ineq)
library(data.table)

# ==============================================================================
# Setup
# ==============================================================================

path <- file.path(EndowGitHub, "DerivedData")
load(file.path(path, "su_nets_expanded.rdata"))

EC <- read.csv(file.path(path, "EC_Revised.csv"))
iec <- read.csv(file.path(path, "IEC_Revised.csv"))
gini <- read.csv(file.path(path, "gini_site_data.csv"))

######### Create dataset #################

probkin_foo <- function(site_name, network_layer = "main_supp", kinship_layer = "secondary_kin") {
  site_data <- su_nets_expanded[[site_name]]

  kin_network <- site_data[[kinship_layer]]
  connection_network <- site_data[[network_layer]]

  # make sure that the kin network has two edges both directions (each and mutual should give same results)
  kin_directed <- as_directed(kin_network,
                              mode = 'mutual')
  # remove self loops
  kin_directed <- delete_edges(kin_directed,  E(kin_directed)[which_loop(kin_directed)])
  connection_network <- delete_edges(connection_network, E(connection_network)[which_loop(connection_network)])

  # intersect the two and get the degree for each node as the union
  kin_links <- intersection(kin_directed, connection_network)
  kin_links_degree <- degree(kin_links, mode = "out")

  # denominator : total kins for each sharing unit
  kin_directed_degree <- degree(kin_directed, mode = "out")

  connection_network_degree <- degree(connection_network, mode="out")
  #### non-kin denom
  nonkin_degree <- connection_network_degree - kin_links_degree
  N <- length(V(connection_network))
  total_nonkin <- (N- 1) - kin_directed_degree
  total <- N - 1

  # sharing unit size
  su_size <- V(connection_network)$susize
  su_wealth <- V(connection_network)$su_wealth

  df <- data.frame(
    su_id = names(kin_links_degree),
    site = site_name,
    kin_links = kin_links_degree,  #numerator
    kins = kin_directed_degree,#denominator
    nonkin_degree = nonkin_degree, # num 2
    total_nonkin = total_nonkin, #denom 2
    connection_network_degree = connection_network_degree,
    total = total,
    N = N,
    su_size = su_size,
    su_wealth = su_wealth,
    su_wpc = su_wealth / su_size,
    kin_pc = kin_directed_degree / su_size,
    kin_links_pc = kin_links_degree / su_size,
    stringsAsFactors = FALSE
  )

  df <- df %>%
    mutate(
      prob_kin = kin_links / kins,
      prob_nonkin = nonkin_degree / total_nonkin,
      prop_kin = kins / (N - 1),
      prop_kin_pc = kin_pc / (N - 1),
      prop_connected_kin = kin_links / connection_network_degree,
      prop_connected_kin_pc = kin_links_pc / connection_network_degree,
      ratio = prob_kin / prob_nonkin
    )


  return(df)

}



prob_kin_df <- data.frame()
for (site_name in names(su_nets_expanded)) {
  site_df <- probkin_foo(site_name,
                         network_layer = "main_supp",
                         kinship_layer = "primary_kin")
  prob_kin_df <- rbind(prob_kin_df, site_df)
  #print(site_name)
}

network_layers <- c("collapse_sum", "loan_req", "loan_giv", "exchange_req", "exchange_giv", "f_behav", "m_behav", "behav", "f_info", "m_info", "info", "govt", "external")
kinship_layers <- c("primary_kin")#, "secondary_kin")

prob_kin_all <- purrr::map_dfr(kinship_layers, function(kin_layer) {
  purrr::map_dfr(network_layers, function(net_layer) {
    purrr::map_dfr(names(su_nets_expanded), function(site_name) {
      tryCatch(
        {
          probkin_foo(
            site_name,
            network_layer = net_layer,
            kinship_layer = kin_layer
          ) %>%
            mutate(network = net_layer, kinship = kin_layer, .after = site)
        },
        error = function(e) {
          message(sprintf(
            "Skipped %s / %s / %s: %s",
            site_name,
            net_layer,
            kin_layer,
            e$message
          ))
          tibble()
        }
      )
    })
  })
})

#### su-level csv
write.csv(prob_kin_all, file.path(EndowGitHub, "DerivedData", "su_kin_support.csv"), row.names = FALSE)


#### site level csv - prop kin connections #####
sitelevel_kinship <- data.frame()

for (sitename in names(su_nets_expanded)) {
  site_graphs <- su_nets_expanded[[sitename]]

  ## --- DIRECTED ---
  g_pck <- site_graphs[["primary_connectedkin_sum"]]
  g_sum <- site_graphs[["sum"]]

  # unweighted edges (gsize)
  prop_directed_gsize <- gsize(g_pck) / gsize(g_sum)

  # weighted edges (sum of edge weights)
  prop_directed_weighted_edges <- sum(E(g_pck)$weight, na.rm = TRUE) /
    sum(E(g_sum)$weight, na.rm = TRUE)

  # degree sums (unweighted)
  prop_directed_deg <- sum(degree(g_pck)) / sum(degree(g_sum))

  # weighted strength sums
  prop_directed_str <- sum(strength(g_pck, weights = E(g_pck)$weight)) /
    sum(strength(g_sum, weights = E(g_sum)$weight))

  ## --- UNDIRECTED (collapse) ---
  und_pck <- as_undirected(g_pck, mode = "collapse")
  und_sum <- as_undirected(g_sum, mode = "collapse")

  # unweighted edges (gsize)
  prop_undirected_gsize <- gsize(und_pck) / gsize(und_sum)

  # weighted edges (sum of edge weights)
  prop_undirected_weighted_edges <- sum(E(und_pck)$weight, na.rm = TRUE) /
    sum(E(und_sum)$weight, na.rm = TRUE)

  # degree sums (unweighted)
  prop_undirected_deg <- sum(degree(und_pck)) / sum(degree(und_sum))

  # weighted strength sums
  prop_undirected_str <- sum(strength(und_pck, weights = E(und_pck)$weight)) /
    sum(strength(und_sum, weights = E(und_sum)$weight))

  ## --- bind into df ---
  sitelevel_kinship <- rbind(
    sitelevel_kinship,
    data.frame(
      site = sitename,
      prop_primaryconnected_directed_gsize          = prop_directed_gsize,
      prop_primaryconnected_directed_weighted_edges = prop_directed_weighted_edges,
      prop_primaryconnected_directed_deg            = prop_directed_deg,
      prop_primaryconnected_directed_str            = prop_directed_str,
      prop_primaryconnected_undirected_gsize          = prop_undirected_gsize,
      prop_primaryconnected_undirected_weighted_edges = prop_undirected_weighted_edges,
      prop_primaryconnected_undirected_deg            = prop_undirected_deg,
      prop_primaryconnected_undirected_str            = prop_undirected_str
    )
  )
}

outpath <- file.path(EndowGitHub, "DerivedData", "sitelevel_kinship.csv")

write.csv(sitelevel_kinship, outpath, row.names = FALSE)