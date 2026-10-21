########################################.
#
#   Create Networks
#   Elly Power
#
########################################.

output_path <- file.path(EndowGitHub, "DerivedData")

# Load libraries (install them if you don't have them)
library(igraph)
library(tidyverse)

normalize <- function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}

remove_self_loops <- function(graph) {
  if (missing(graph) || is.null(graph)) {
    return(NULL)
  }

  delete_edges(graph, E(graph)[which_loop(graph)])
}

remove_layer_self_loops <- function(layer) {
  if (!is.list(layer)) {
    return(layer)
  }

  lapply(layer, remove_self_loops)
}



########################################.
#
#   Load primary data
#
########################################.

## These files derive from process_data.R.
## su_nets is a series of loaded igraph objects, one for each network prompt
## su_nets_recipient_collapse is the same as su_nets, but the edge weights are collapsed at the respondent level :
# so if in su_nets two people from SU1 named two from SU2 : weight = 4.
# But in su_nets_recipient_collapse : it would be 2

## su_meta is associated Sharing Unit metadata. The order of SUs should be equivalent across all networks and su_meta within the site.
## su_alters is metadata on alters named by members of each sharing unit for each prompt. It lists all alters named for that prompt and then some summary statistics about them (e.g., their count, the # of residents (and so of non-residents), the # of high status individuals, etc.)

## The edge weights for the network prompts represent the number of nominations from SU_i to SU_j. So, if there were multiple reporters in the SU, this could obviously be more than 1.


load(file.path(EndowDerivedData, "su_nets.rdata"))
load(file.path(EndowDerivedData, "su_meta.rdata"))
load(file.path(EndowDerivedData, "su_alters.rdata"))
load(file.path(EndowDerivedData, "su_nets_recipient_collapse.rdata"))

sites <-
  names(su_nets)

## variables here that are crossed out are crossed out bc they are not standard across sites.
su_meta <- lapply(su_meta, function(x) {

  select(
    x, ## retaining what should be the standardized variables
    "su_id",
    #"smaller_meals",
    #"fewer_meals",
    #"no_food",
    #"sleep_hungry",
    #"without_eating",
    "malehead",
    "femalehead",
    # "malehhfatheredu",
    # "malehhfathernoeticother",
    # "malehhmotheredu",
    # "femalehhmothernoeticother",
    # "femalehhfatheredu",
    # "femalehhfathernoeticother",
    # "femalehhmotheredu",
    # "femalehhmothernoeticother",
    "su_size",
    "adult_count",
    "able_count",
    "su_wealth",
    "su_sampled",
    "num_surv",
    "num_surv_male_q",
    "num_surv_female_q",
    "primary_kin",
    "secondary_kin",
    "age_av",
    "age_max",
    "edu_max",
    "other_noetic_any",
    "other_noetic_count",
    "status_any",
    "status_count",
    "hofh",
    "able_count_hh",
    "status_any_hh",
    "status_count_hh",
    "edu_max_hh",
    "other_noetic_any_hh",
    "other_noetic_count_hh"
  )
})


## Selecting particular networks (not considering the external ones, as here considering only ties to other sharing units!)

loan_req <- lapply(su_nets, function(x) x$e1)
loan_giv <- lapply(su_nets, function(x) x$e2)
exchange_req <- lapply(su_nets, function(x) x$e3)
exchange_giv <- lapply(su_nets, function(x) x$e4)
f_behav <- lapply(su_nets, function(x) x$e5)
m_behav <- lapply(su_nets, function(x) x$e6)
f_info <- lapply(su_nets, function(x) x$e7)
f_info$MK <- NULL # this is picking up what's there called e78, because e7 is an abbreviation for e78 as read by R
m_info <- lapply(su_nets, function(x) x$e8)
govt <- lapply(su_nets, function(x) x$e9)
external <- lapply(su_nets, function(x) x$e10)

max_rel <- lapply(su_nets, function(x) x$maxrel)

## e.g., 0.25 is grandchildren to grandparents, 0.5 is parents to kids or full siblings.
primary_kin <- lapply(sites, function(site) {
  kin <- delete_edges(max_rel[[site]], which(E(max_rel[[site]])$weight < 0.5))
  V(kin)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(kin)$susize <- V(exchange_req[[site]])$susize
  return(kin)
})
names(primary_kin) <- sites

secondary_kin <- lapply(sites, function(site) {
  kin <- delete_edges(max_rel[[site]], which(E(max_rel[[site]])$weight < 0.25))
  V(kin)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(kin)$susize <- V(exchange_req[[site]])$susize
  return(kin)
})
names(secondary_kin) <- sites


## CREATING *UNION* NETWORKS. For loan + exchange, that's combining "double sampled" prompts; for behav + info, that's combining gender-specific prompts

### EXCEPTIONS :
# IH, PC, WH, WL some questions were not elicited - only request not give for example
# in union  : it will use loan_req exchange_req
# MK : male and female weren't seperated
###

## Be careful running anything here line by line because within the function it would overwrite
## the network name with matrices for the particular site if you give it the opportunity to
## write to the global environment.

## Elly's direction

loan <- lapply(setdiff(sites, c("IH","PC", "WH", "WL")), function(site) {
  req <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight"))
  give <- as.matrix(as_adjacency_matrix(loan_giv[[site]], attr = "weight"))
  adj <- req + t(give)
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
  V(graph)$susize <- V(loan_req[[site]])$susize
  return(graph)
})
names(loan) <- setdiff(sites, c("IH","PC", "WH", "WL"))

## matt's direction

loan_matt <- lapply(setdiff(sites, c("IH","PC", "WH", "WL")), function(site) {
  req <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight"))
  give <- as.matrix(as_adjacency_matrix(loan_giv[[site]], attr = "weight"))
  adj <- t(req) + give
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
  V(graph)$susize <- V(loan_req[[site]])$susize
  return(graph)
})
names(loan_matt) <- setdiff(sites, c("IH","PC", "WH", "WL"))

## elly's direction

exchange <- lapply(sites, function(site) {
  req <- as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight"))
  give <- as.matrix(as_adjacency_matrix(exchange_giv[[site]], attr = "weight"))
  adj <- req + t(give)
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(exchange) <- sites

## matt's direction

exchange_matt <- lapply(sites, function(site) {
  req <- as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight"))
  give <- as.matrix(as_adjacency_matrix(exchange_giv[[site]], attr = "weight"))
  adj <- t(req) + give
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(exchange_matt) <- sites

behav <- lapply(sites, function(site) {
  if(site == "MK") return(su_nets[[site]]$e56a) else
    female <- as.matrix(as_adjacency_matrix(f_behav[[site]], attr = "weight"))
    male <- as.matrix(as_adjacency_matrix(m_behav[[site]], attr = "weight"))
    adj <- female + male
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(f_behav[[site]])$su_wealth
    V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(behav) <- sites

info <- lapply(sites, function(site) {
  if(site == "MK") return(su_nets[[site]]$e78) else
  female <- as.matrix(as_adjacency_matrix(f_info[[site]], attr = "weight"))
  male <- as.matrix(as_adjacency_matrix(m_info[[site]], attr = "weight"))
  adj <- female + male
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(f_info[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(info) <- sites

#### main_supp :
## 1.  no double sampled loan and exchange - so only loan_req and exchange_req
## 2. no govt, externals

main_supp <- lapply(sites, function(site){
  if (site == "MK") {
    adj <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = "weight")) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = "weight"))
      graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
      V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
      V(graph)$susize <- V(loan_req[[site]])$susize
      return(graph)
  } else {
    adj <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(behav[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(info[[site]], attr = "weight"))
      graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
      V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
      V(graph)$susize <- V(loan_req[[site]])$susize
      return(graph)
  }
})
names(main_supp) <- sites

main_supp_matt <- lapply(sites, function(site){
  if (site == "MK") {
    adj <- t(as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight"))) +
      t(as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight"))) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = "weight")) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = "weight"))
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
    V(graph)$susize <- V(loan_req[[site]])$susize
    return(graph)
  } else {
    adj <- t(as.matrix(as_adjacency_matrix(loan_req[[site]], attr = "weight"))) +
      t(as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = "weight"))) +
      as.matrix(as_adjacency_matrix(behav[[site]], attr = "weight")) +
      as.matrix(as_adjacency_matrix(info[[site]], attr = "weight"))
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
    V(graph)$susize <- V(loan_req[[site]])$susize
    return(graph)
  }
})
names(main_supp_matt) <- sites


male <- lapply(setdiff(sites, "MK"), function(site) {
  info <- as.matrix(as_adjacency_matrix(m_info[[site]], attr = "weight"))
  behav <- as.matrix(as_adjacency_matrix(m_behav[[site]], attr = "weight"))
  adj <- info + behav
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(male) <- setdiff(sites, "MK")

female <- lapply(setdiff(sites, "MK"), function(site) {
  info <- as.matrix(as_adjacency_matrix(f_info[[site]], attr = "weight"))
  behav <- as.matrix(as_adjacency_matrix(f_behav[[site]], attr = "weight"))
  adj <- info + behav
  graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize
  return(graph)
})
names(female) <- setdiff(sites, "MK")

exchange_plus_female <-
  lapply(setdiff(sites, "MK"), function(site) {
    female <- as.matrix(as_adjacency_matrix(female[[site]], attr = "weight"))
    exchange <- as.matrix(as_adjacency_matrix(exchange[[site]], attr = "weight"))
    adj <- female + exchange
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
    V(graph)$susize <- V(exchange_req[[site]])$susize
    return(graph)
  })
names(exchange_plus_female) <- setdiff(sites, "MK")

# ## potentially, we should include e9/e10, e11 on can be ignored due to comparability.
# ## For the double-sampled prompts, after discussing with Elly, we are not adding additional
# ## weight just because your response about borrowing concords with someone else's response
# ## about lending.
# union <-
#   lapply(sites, function(site) {
#     if (site %in% c("IH","PC", "WH", "WL")) {
#       loan <- as.matrix(as_adjacency_matrix(loan_req[[site]]))
#       exchange <- as.matrix(as_adjacency_matrix(exchange[[site]]))
#       behav <- as.matrix(as_adjacency_matrix(behav[[site]], attr = "weight"))
#       info <- as.matrix(as_adjacency_matrix(info[[site]], attr = "weight"))
#       adj <- loan + exchange + behav + info
#       graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
#       V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#       V(graph)$susize <- V(exchange_req[[site]])$susize
#
#       return(graph)
#     } else {
#       loan <- as.matrix(as_adjacency_matrix(loan[[site]]))
#       exchange <- as.matrix(as_adjacency_matrix(exchange[[site]]))
#       behav <- as.matrix(as_adjacency_matrix(behav[[site]], attr = "weight"))
#       info <- as.matrix(as_adjacency_matrix(info[[site]], attr = "weight"))
#       adj <- loan + exchange + behav + info
#       graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
#       V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#       V(graph)$susize <- V(exchange_req[[site]])$susize
#
#       return(graph)
#     }
#
#   })
# names(union) <- sites


# #### layer sum: 1. layer sum  2. layer sum with externals and govt 3. mainsupp layer weight version 4. layersum with gender combined ####
#
# # layer sum : note that loan and exchange are transposed and counted as weight = 1 (MK exception)
# layer_sum <-
#   lapply(sites, function(site) {
#
#     if (site %in% c("IH","PC", "WH", "WL")) {
#       # loan req only
#       loan <- as.matrix(as_adjacency_matrix(loan_req[[site]]), attr = NULL)
#     } else{
#       # loan req and giv treated as one
#       loan <- as.matrix(as_adjacency_matrix(loan[[site]]), attr = NULL)
#     }
#     # exchange req and giv treated as one
#     exchange <- as.matrix(as_adjacency_matrix(exchange[[site]]), attr = NULL)
#
#
#     # behav : gender separate and MK exception :  notice it will have one layer less for behav
#     if(site == "MK") {
#       f_behav <- as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = NULL))
#     }
#     else {
#     f_behav <- as.matrix(as_adjacency_matrix(f_behav[[site]], attr = NULL))
#     m_behav <- as.matrix(as_adjacency_matrix(m_behav[[site]], attr = NULL))
#     }
#     # info : gender separate and MK exception : notice it will have one layer less for info
#     if(site == "MK"){
#       f_info <- as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = NULL))
#     }  else {
#       f_info <- as.matrix(as_adjacency_matrix(f_info[[site]], attr = NULL))
#       m_info <- as.matrix(as_adjacency_matrix(m_info[[site]], attr = NULL))
#
#     }
#
#
#     # max layer weight : 6 (MK : 4)
#     if(site == "MK"){
#       adj <- loan + exchange + f_behav +  f_info
#     }else{
#       adj <- loan + exchange + f_behav + m_behav + f_info + m_info
#     }
#
#     graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#     V(graph)$susize <- V(exchange_req[[site]])$susize
#     return(graph)
#   })
# names(layer_sum) <- sites
#
# # layer sum gender : same as layer sum - except we don't treat male and female as separate
# layer_sum_gender <-
#   lapply(sites, function(site) {
#
#     if (site %in% c("IH","PC", "WH", "WL")) {
#       # loan req only
#       loan <- as.matrix(as_adjacency_matrix(loan_req[[site]]), attr = NULL)
#     } else{
#       # loan req and giv treated as one
#       loan <- as.matrix(as_adjacency_matrix(loan[[site]]), attr = NULL)
#     }
#
#       # exchange req and giv treated as one
#       exchange <- as.matrix(as_adjacency_matrix(exchange[[site]]), attr = NULL)
#       # info f and m treated as one
#       info <- as.matrix(as_adjacency_matrix(info[[site]]), attr = NULL)
#       # behav f and m treated as one
#       behav <- as.matrix(as_adjacency_matrix(behav[[site]]), attr = NULL)
#       # max layer weight : 4
#       adj <- loan + exchange + behav + info
#       graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
#       V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#       V(graph)$susize <- V(exchange_req[[site]])$susize
#       return(graph)
#   })
# names(layer_sum_gender) <- sites
#
# # layer sum with externals and govt (gender combined)
# layer_sum_externals <-
#   lapply(sites, function(site) {
#
#     if (site %in% c("IH","PC", "WH", "WL")) {
#       # loan req only
#       loan <- as.matrix(as_adjacency_matrix(loan_req[[site]]), attr = NULL)
#     } else{
#       # loan req and giv treated as one
#       loan <- as.matrix(as_adjacency_matrix(loan[[site]]), attr = NULL)
#     }
#
#     # exchange req and giv treated as one
#     exchange <- as.matrix(as_adjacency_matrix(exchange[[site]]), attr = NULL)
#     # info f and m treated as one
#     info <- as.matrix(as_adjacency_matrix(info[[site]]), attr = NULL)
#     # behav f and m treated as one
#     behav <- as.matrix(as_adjacency_matrix(behav[[site]]), attr = NULL)
#
#
#     if (site %in% c("IH","PC", "WH", "WL","MK")) { #these have null external and govt
#       adj <- loan + exchange + behav + info # max layerweight = 4
#     } else{
#       # externals
#       external <- as.matrix(as_adjacency_matrix(external[[site]]), attr = NULL)
#       # govt
#       govt <- as.matrix(as_adjacency_matrix(govt[[site]]), attr = NULL)
#       adj <- loan + exchange + behav + info + govt + external # max layerweight = 6
#     }
#     graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#     V(graph)$susize <- V(exchange_req[[site]])$susize
#     return(graph)
#   })
# names(layer_sum_externals) <- sites

# main support with layer weights and combined gender
main_supp_layer <- lapply(sites, function(site){
  if (site == "MK") {
    # MK : info and behav have only one gender
    adj <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = NULL)) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = NULL))
      graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
      V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
      V(graph)$susize <- V(loan_req[[site]])$susize
      return(graph)
  } else {
    adj <- as.matrix(as_adjacency_matrix(loan_req[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(behav[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(info[[site]], attr = NULL))
      graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
      V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
      V(graph)$susize <- V(loan_req[[site]])$susize
      return(graph)
  }
})
names(main_supp_layer) <- sites

# main support with layer weights and combined gender
main_supp_layer_matt <- lapply(sites, function(site){
  if (site == "MK") {
    # MK : info and behav have only one gender
    adj <- t(as.matrix(as_adjacency_matrix(loan_req[[site]], attr = NULL))) +
      t(as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = NULL))) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = NULL)) +
      as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = NULL))
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
    V(graph)$susize <- V(loan_req[[site]])$susize
    return(graph)
  } else {
    adj <- t(as.matrix(as_adjacency_matrix(loan_req[[site]], attr = NULL))) +
      t(as.matrix(as_adjacency_matrix(exchange_req[[site]], attr = NULL))) +
      as.matrix(as_adjacency_matrix(behav[[site]], attr = NULL)) +
      as.matrix(as_adjacency_matrix(info[[site]], attr = NULL))
    graph <- graph_from_adjacency_matrix(as.matrix(adj), mode = "directed", weighted = TRUE, diag = TRUE)
    V(graph)$su_wealth <- V(loan_req[[site]])$su_wealth
    V(graph)$susize <- V(loan_req[[site]])$susize
    return(graph)
  }
})
names(main_supp_layer_matt) <- sites


#### sum network (A) ####

# same as main supp, but including the weights for each layer and dividing by the number of respondents / surveyed
# Function to adjust weights by dividing by num_surv
adjust_weights_by_num_surv <- function(graph,site) {
  # Setting vertex attribute
  vertex_ids <- V(graph)$name
  vertex_data <- su_meta[[site]]
  vertex_data <- vertex_data[match(vertex_ids, vertex_data$su_id), ]
  V(graph)$num_surv <- vertex_data$num_surv

  # Get the num_surv attribute for source nodes of each edge
  source_nodes <- ends(graph, E(graph))[, 1]
  num_surv_values <- V(graph)[source_nodes]$num_surv

  # Adjust weights, handling NA or 0 values in num_surv
  E(graph)$weight <- ifelse(is.na(num_surv_values) | num_surv_values == 0,
                            NA,
                            E(graph)$weight / num_surv_values)
  return(graph)
}

adjust_weights_by_num_surv_male_q <- function(graph,site) {

  # Setting vertex attribute
  vertex_ids <- V(graph)$name
  vertex_data <- su_meta[[site]]
  vertex_data <- vertex_data[match(vertex_ids, vertex_data$su_id), ]
  V(graph)$num_surv_male_q <- vertex_data$num_surv_male_q

  # Get the num_surv attribute for source nodes of each edge
  source_nodes <- ends(graph, E(graph))[, 1]
  num_surv_values <- V(graph)[source_nodes]$num_surv_male_q

  # Adjust weights, handling NA or 0 values in num_surv
  # Note, if we have a 0 or NA for num_surv, then we should have a 0 for the edge.
  # We should check this with a stopifnot at some point.
  E(graph)$weight <- ifelse(is.na(num_surv_values) | num_surv_values == 0,
                            NA,
                            E(graph)$weight / num_surv_values)
  return(graph)
}

adjust_weights_by_num_surv_female_q <- function(graph,site) {
  # Setting vertex attribute
  vertex_ids <- V(graph)$name
  vertex_data <- su_meta[[site]]
  vertex_data <- vertex_data[match(vertex_ids, vertex_data$su_id), ]
  V(graph)$num_surv_female_q <- vertex_data$num_surv_female_q

  # Get the num_surv attribute for source nodes of each edge
  source_nodes <- ends(graph, E(graph))[, 1]
  num_surv_values <- V(graph)[source_nodes]$num_surv_female_q

  # Adjust weights, handling NA or 0 values in num_surv
  E(graph)$weight <- ifelse(is.na(num_surv_values) | num_surv_values == 0,
                            NA,
                            E(graph)$weight / num_surv_values)
  return(graph)
}

adjust_weights_by_population <- function(graph,site) {

  # Get the num_surv attribute for source nodes of each edge
  source_nodes <- ends(graph, E(graph))[, 1]
  susize_values <- V(graph)[source_nodes]$susize

  # Adjust weights, handling NA or 0 values in susize (should never happen)
  E(graph)$weight <- ifelse(is.na(susize_values) | susize_values == 0,
                            NA,
                            E(graph)$weight / susize_values)
  return(graph)
}


sum <- lapply(sites, function(site){
  # loan layer is just loan request - ie. no transposes of give layer (like main supp)
  loan_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(loan_req[[site]])), mode = "directed", weighted = TRUE, diag = TRUE)
  V(loan_graph)$num_surv <- V(loan_req[[site]])$num_surv
  loan_graph <- adjust_weights_by_num_surv(loan_graph, site)
  loan <- as_adjacency_matrix(loan_graph, attr = "weight")

  # exchange layer is just exchange request - ie. no transpose of give layer (like main supp)
  exchange_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(exchange_req[[site]])), mode = "directed", weighted = TRUE, diag = TRUE)
  V(exchange_graph)$num_surv <- V(exchange_req[[site]])$num_surv
  exchange_graph <- adjust_weights_by_num_surv(exchange_graph,site)
  exchange <- as_adjacency_matrix(exchange_graph, attr = "weight")

  # Behavior layers (MK site exception)
  if (site == "MK") {
    f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a,  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_behav_graph)$num_surv <- V(su_nets[[site]]$e56a)$num_surv
    f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
    f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
  } else {
    f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_behav[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    m_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_behav[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_behav_graph)$num_surv <- V(f_behav[[site]])$num_surv
    V(m_behav_graph)$num_surv <- V(m_behav[[site]])$num_surv
    f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
    m_behav_graph <- adjust_weights_by_num_surv_male_q(m_behav_graph,site)
    f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
    m_behav <- as_adjacency_matrix(m_behav_graph, attr = "weight")
  }

  # Information layers (MK site exception)
  if (site == "MK") {
    f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets[[site]]$e78,  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_info_graph)$num_surv <- V(su_nets[[site]]$e78)$num_surv
    f_info_graph <- adjust_weights_by_num_surv(f_info_graph,site)
    f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
  } else {
    f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_info[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    m_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_info[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_info_graph)$num_surv <- V(f_info[[site]])$num_surv
    V(m_info_graph)$num_surv <- V(m_info[[site]])$num_surv
    f_info_graph <- adjust_weights_by_num_surv_female_q(f_info_graph,site)
    m_info_graph <- adjust_weights_by_num_surv_male_q(m_info_graph,site)
    f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
    m_info <- as_adjacency_matrix(m_info_graph, attr = "weight")
  }

  # Sum adjusted adjacency matrices based on site specifications
  if (site == "MK") {
    adj <- loan + exchange + f_behav + f_info
  } else {
    adj <- loan + exchange + f_behav + m_behav + f_info + m_info
  }

  # Create final graph and add node attributes
  graph <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req[[site]])$susize


  # Get the mean weight
  mean_weight <- mean(E(graph)$weight, na.rm = TRUE)

  # Print the results
  #cat(site, mean_weight, "\n")

  return(graph)
})

names(sum) <- sites

sum_column_stochastic <- lapply(sites, function(site){

  if ("weight" %in% edge_attr_names(sum[[site]])) {
    adj <- as.matrix(as_adjacency_matrix(sum[[site]], attr = "weight"))
  } else {
    adj <- as.matrix(as_adjacency_matrix(sum[[site]]))
  }

  ## if the column sum is zero, we can't normalize by dividing by zero,
  ## but since the column sum is zero we can divide by any arbitrary constant.
  normalizing_vector <- ifelse(rowSums(t(adj)) > 0, rowSums(t(adj)), 1)

  column_stochastic_adj <- t(t(adj) / normalizing_vector)

  graph <- graph_from_adjacency_matrix(column_stochastic_adj, mode = "directed", weighted = TRUE, diag = TRUE)

  return(graph)

})

names(sum_column_stochastic) <- sites

# sum_matt <- lapply(sites, function(site){
#   # transpose to get matt direction
#   loan_graph <- graph_from_adjacency_matrix(t(as.matrix(as_adjacency_matrix(loan_req[[site]]))), mode = "directed", weighted = TRUE, diag = TRUE)
#   V(loan_graph)$num_surv <- V(loan_req[[site]])$num_surv
#   loan_graph <- adjust_weights_by_num_surv(loan_graph, site)
#   loan <- as_adjacency_matrix(loan_graph, attr = "weight")
#
#   # exchange layer is just exchange request - ie. no transpose of give layer (like main supp)
#   exchange_graph <- graph_from_adjacency_matrix(t(as.matrix(as_adjacency_matrix(exchange_req[[site]]))), mode = "directed", weighted = TRUE, diag = TRUE)
#   V(exchange_graph)$num_surv <- V(exchange_req[[site]])$num_surv
#   exchange_graph <- adjust_weights_by_num_surv(exchange_graph,site)
#   exchange <- as_adjacency_matrix(exchange_graph, attr = "weight")
#
#   # Behavior layers (MK site exception)
#   if (site == "MK") {
#     f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets[[site]]$e56a, attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_behav_graph)$num_surv <- V(su_nets[[site]]$e56a)$num_surv
#     f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
#     f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
#   } else {
#     f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_behav[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     m_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_behav[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_behav_graph)$num_surv <- V(f_behav[[site]])$num_surv
#     V(m_behav_graph)$num_surv <- V(m_behav[[site]])$num_surv
#     f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
#     m_behav_graph <- adjust_weights_by_num_surv_male_q(m_behav_graph,site)
#     f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
#     m_behav <- as_adjacency_matrix(m_behav_graph, attr = "weight")
#   }
#
#   # Information layers (MK site exception)
#   if (site == "MK") {
#     f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets[[site]]$e78, attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_info_graph)$num_surv <- V(su_nets[[site]]$e78)$num_surv
#     f_info_graph <- adjust_weights_by_num_surv(f_info_graph,site)
#     f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
#   } else {
#     f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_info[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     m_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_info[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_info_graph)$num_surv <- V(f_info[[site]])$num_surv
#     V(m_info_graph)$num_surv <- V(m_info[[site]])$num_surv
#     f_info_graph <- adjust_weights_by_num_surv_female_q(f_info_graph,site)
#     m_info_graph <- adjust_weights_by_num_surv_male_q(m_info_graph,site)
#     f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
#     m_info <- as_adjacency_matrix(m_info_graph, attr = "weight")
#   }
#
#   # Sum adjusted adjacency matrices based on site specifications
#   if (site == "MK") {
#     adj <- loan + exchange + f_behav + f_info
#   } else {
#     adj <- loan + exchange + f_behav + m_behav + f_info + m_info
#   }
#
#   # Create final graph and add node attributes
#   graph <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE)
#   V(graph)$su_wealth <- V(exchange_req[[site]])$su_wealth
#   V(graph)$susize <- V(exchange_req[[site]])$susize
#
#
#   # Get the mean weight
#   mean_weight <- mean(E(graph)$weight, na.rm = TRUE)
#
#   # Print the results
#   cat(site, mean_weight, "\n")
#
#   return(graph)
# })
#
# names(sum_matt) <- sites

sum_pop_downweighted <- lapply(sites, function(site){

  graph <- adjust_weights_by_population(sum[[site]], site)
  return(graph)

})

names(sum_pop_downweighted) <- sites

external <- lapply(sites, function(site) {
  #print(site)
  if (is.null(external[[site]])) {
    return(NULL)
  }

  external_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(external[[site]])), mode = "directed", weighted = TRUE, diag = TRUE)
  V(external_graph)$num_surv <- V(external[[site]])$num_surv
  external_graph <- adjust_weights_by_num_surv(external_graph, site)
  adj <- as_adjacency_matrix(external_graph, attr = "weight")
  graph <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$num_surv <- V(external[[site]])$num_surv
  V(graph)$su_wealth <- V(external[[site]])$su_wealth
  V(graph)$susize <- V(external[[site]])$susize

  return(graph)

})

names(external) <- sites


#### COLLAPSE sum network (C) ####

### same as sum network, only difference is that the weights in the numerator is "collapsed" to the respondent level
### we use su_nets_recipient_collapse edge weights which make this correction
### NOTE : TO DO PE : doesn't show up in the su_nets_recipient_collapse object - need to fix in process_data.r

loan_req_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e1)
loan_giv_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e2)
exchange_req_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e3)
exchange_giv_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e4)
f_behav_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e5)
m_behav_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e6)
f_info_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e7)
f_info_collapse$MK <- NULL # this is picking up what's there called e78, because e7 is an abbreviation for e78 as read by R
m_info_collapse <- lapply(su_nets_recipient_collapse, function(x) x$e8)

#print ("****************************************")
collapse_sum_sites <- sites
#collapse_sum_sites <- sites
collapse_sum <- lapply(collapse_sum_sites, function(site){
  # loan layer is just loan request - ie. no transposes of give layer (like main supp)
  loan_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(loan_req_collapse[[site]])), mode = "directed", weighted = TRUE, diag = TRUE)
  V(loan_graph)$num_surv <- V(loan_req_collapse[[site]])$num_surv
  loan_graph <- adjust_weights_by_num_surv(loan_graph, site)
  loan <- as_adjacency_matrix(loan_graph, attr = "weight")

  # exchange layer is just exchange request - ie. no transpose of give layer (like main supp)
  exchange_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(exchange_req_collapse[[site]])), mode = "directed", weighted = TRUE, diag = TRUE)
  V(exchange_graph)$num_surv <- V(exchange_req_collapse[[site]])$num_surv
  exchange_graph <- adjust_weights_by_num_surv(exchange_graph,site)
  exchange <- as_adjacency_matrix(exchange_graph, attr = "weight")

  # Behavior layers (MK site exception)
  if (site == "MK") {
    f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets_recipient_collapse[[site]]$e56a,  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_behav_graph)$num_surv <- V(su_nets_recipient_collapse[[site]]$e56a)$num_surv
    f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
    f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
  } else {
    f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_behav_collapse[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    m_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_behav_collapse[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_behav_graph)$num_surv <- V(f_behav_collapse[[site]])$num_surv
    V(m_behav_graph)$num_surv <- V(m_behav_collapse[[site]])$num_surv
    f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
    m_behav_graph <- adjust_weights_by_num_surv_male_q(m_behav_graph,site)
    f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
    m_behav <- as_adjacency_matrix(m_behav_graph, attr = "weight")
  }

  # Information layers (MK site exception)
  if (site == "MK") {
    f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets_recipient_collapse[[site]]$e78,  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_info_graph)$num_surv <- V(su_nets_recipient_collapse[[site]]$e78)$num_surv
    f_info_graph <- adjust_weights_by_num_surv(f_info_graph,site)
    f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
  } else {
    f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_info_collapse[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    m_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_info_collapse[[site]],  attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
    V(f_info_graph)$num_surv <- V(f_info_collapse[[site]])$num_surv
    V(m_info_graph)$num_surv <- V(m_info_collapse[[site]])$num_surv
    f_info_graph <- adjust_weights_by_num_surv_female_q(f_info_graph,site)
    m_info_graph <- adjust_weights_by_num_surv_male_q(m_info_graph,site)
    f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
    m_info <- as_adjacency_matrix(m_info_graph, attr = "weight")
  }

  # Sum adjusted adjacency matrices based on site specifications
  if (site == "MK") {
    adj <- loan + exchange + f_behav + f_info
  } else {
    adj <- loan + exchange + f_behav + m_behav + f_info + m_info
  }

  # Create final graph and add node attributes
  graph <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE)
  V(graph)$su_wealth <- V(exchange_req_collapse[[site]])$su_wealth
  V(graph)$susize <- V(exchange_req_collapse[[site]])$susize


  # Get the mean weight
  mean_weight <- mean(E(graph)$weight, na.rm = TRUE)

  # Print the results
  #cat(site, mean_weight, "\n")

  return(graph)
})

names(collapse_sum) <- collapse_sum_sites

# collapse_sum_matt <- lapply(collapse_sum_sites, function(site){
#   # loan layer is just loan request - ie. no transposes of give layer (like main supp)
#   loan_graph <- graph_from_adjacency_matrix(t(as.matrix(as_adjacency_matrix(loan_req_collapse[[site]]))), mode = "directed", weighted = TRUE, diag = TRUE)
#   V(loan_graph)$num_surv <- V(loan_req_collapse[[site]])$num_surv
#   loan_graph <- adjust_weights_by_num_surv(loan_graph, site)
#   loan <- as_adjacency_matrix(loan_graph, attr = "weight")
#
#   # exchange layer is just exchange request - ie. no transpose of give layer (like main supp)
#   exchange_graph <- graph_from_adjacency_matrix(t(as.matrix(as_adjacency_matrix(exchange_req_collapse[[site]]))), mode = "directed", weighted = TRUE, diag = TRUE)
#   V(exchange_graph)$num_surv <- V(exchange_req_collapse[[site]])$num_surv
#   exchange_graph <- adjust_weights_by_num_surv(exchange_graph,site)
#   exchange <- as_adjacency_matrix(exchange_graph, attr = "weight")
#
#   # Behavior layers (MK site exception)
#   if (site == "MK") {
#     f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets_recipient_collapse[[site]]$e56a, attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_behav_graph)$num_surv <- V(su_nets_recipient_collapse[[site]]$e56a)$num_surv
#     f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
#     f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
#   } else {
#     f_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_behav_collapse[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     m_behav_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_behav_collapse[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_behav_graph)$num_surv <- V(f_behav_collapse[[site]])$num_surv
#     V(m_behav_graph)$num_surv <- V(m_behav_collapse[[site]])$num_surv
#     f_behav_graph <- adjust_weights_by_num_surv_female_q(f_behav_graph,site)
#     m_behav_graph <- adjust_weights_by_num_surv_male_q(m_behav_graph,site)
#     f_behav <- as_adjacency_matrix(f_behav_graph, attr = "weight")
#     m_behav <- as_adjacency_matrix(m_behav_graph, attr = "weight")
#   }
#
#   # Information layers (MK site exception)
#   if (site == "MK") {
#     f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(su_nets_recipient_collapse[[site]]$e78, attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_info_graph)$num_surv <- V(su_nets_recipient_collapse[[site]]$e78)$num_surv
#     f_info_graph <- adjust_weights_by_num_surv(f_info_graph,site)
#     f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
#   } else {
#     f_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(f_info_collapse[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     m_info_graph <- graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix(m_info_collapse[[site]], attr = "weight")), mode = "directed", weighted = TRUE, diag = TRUE)
#     V(f_info_graph)$num_surv <- V(f_info_collapse[[site]])$num_surv
#     V(m_info_graph)$num_surv <- V(m_info_collapse[[site]])$num_surv
#     f_info_graph <- adjust_weights_by_num_surv_female_q(f_info_graph,site)
#     m_info_graph <- adjust_weights_by_num_surv_male_q(m_info_graph,site)
#     f_info <- as_adjacency_matrix(f_info_graph, attr = "weight")
#     m_info <- as_adjacency_matrix(m_info_graph, attr = "weight")
#   }
#
#   # Sum adjusted adjacency matrices based on site specifications
#   if (site == "MK") {
#     adj <- loan + exchange + f_behav + f_info
#   } else {
#     adj <- loan + exchange + f_behav + m_behav + f_info + m_info
#   }
#
#   # Create final graph and add node attributes
#   graph <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE)
#   V(graph)$su_wealth <- V(exchange_req_collapse[[site]])$su_wealth
#   V(graph)$susize <- V(exchange_req_collapse[[site]])$susize
#
#
#   # Get the mean weight
#   mean_weight <- mean(E(graph)$weight, na.rm = TRUE)
#
#   # Print the resultsa
#   cat(site, mean_weight, "\n")
#
#   return(graph)
# })
#
# names(collapse_sum_matt) <- collapse_sum_sites

## We also want to make a version of the network where we divide each edge weight
## by the number of people in the sharing units.


### kinship networks (connected on sum)

keep_connection_weights <- function(graph) {
  connection_weights <- edge_attr(graph, "weight_2")
  if (is.null(connection_weights)) {
    connection_weights <- numeric(ecount(graph))
  }

  graph <- set_edge_attr(graph, "weight", value = connection_weights)
  if ("weight_1" %in% edge_attr_names(graph)) {
    graph <- delete_edge_attr(graph, "weight_1")
  }
  if ("weight_2" %in% edge_attr_names(graph)) {
    graph <- delete_edge_attr(graph, "weight_2")
  }

  graph
}

primary_connectedkin_sum <- lapply(sites, function(site) {
  kin_graph <- primary_kin[[site]]
  connection_graph <- sum[[site]]

  # make the kinship graph directed : ie. if i connected j now, we have edges going both ways
  kin_undirected <- as_undirected(kin_graph, mode = "collapse") #so here kin has an edge from i to j if either i -> j or j -> i or both
  kin_bidirected <- as_directed(kin_undirected, mode = "mutual") # now every i-j has both i->j and j->i versions

  # intersect to get connected kin
  kin_links <- intersection(kin_bidirected, connection_graph, byname = TRUE)

  #remove isolates : it doesn't make sense to have them here even though they exist in sum because these are structural 0s here
  #kin_links <- delete_vertices(kin_links, V(kin_links)[degree(kin_links, mode = "all") == 0])

  # remove self loops
  #kin_links <- delete_edges(kin_links, E(kin_links)[which_loop(kin_links)])

  # this will have two weights - one from kin and one from the network so we need to delete
  kin_links <- keep_connection_weights(kin_links)

  #node attributes
  V(kin_links)$su_wealth <- V(connection_graph)$su_wealth[V(kin_links)$name]
  V(kin_links)$susize <- V(connection_graph)$susize[V(kin_links)$name]


  return(kin_links)
})
names(primary_connectedkin_sum) <- sites


secondary_connectedkin_sum <- lapply(sites, function(site) {
  kin_graph <- secondary_kin[[site]]
  connection_graph <- sum[[site]]

  # make the kinship graph directed : ie. if i connected j now, we have edges going both ways
  kin_undirected <- as_undirected(kin_graph, mode = "collapse") #so here kin has an edge from i to j if either i -> j or j -> i or both
  kin_bidirected <- as_directed(kin_undirected, mode = "mutual") # now every i-j has both i->j and j->i versions

  # intersect to get connected kin
  kin_links <- intersection(kin_bidirected, connection_graph, byname = TRUE)
  #remove isolates : it doesn't make sense to have them here even though they exist in sum because these are structural 0s here
  #kin_links <- delete_vertices(kin_links, V(kin_links)[degree(kin_links, mode = "all") == 0])
  # remove self loops
  #kin_links <- delete_edges(kin_links, E(kin_links)[which_loop(kin_links)])

  # this will have two weights - one from kin and one from the network so we need to delete so that only the sum network weights are retained
  kin_links <- keep_connection_weights(kin_links)


  # node attributes
  V(kin_links)$su_wealth <- V(connection_graph)$su_wealth[V(kin_links)$name]
  V(kin_links)$susize <- V(connection_graph)$susize[V(kin_links)$name]

  return(kin_links)
})
names(secondary_connectedkin_sum) <- sites




non_primary_connectedkin_sum <- lapply(sites, function(site) {
  kin_graph <- primary_connectedkin_sum[[site]]
  connection_graph <- sum[[site]]


  # sum network / primary connected kin sum
  nonkin_links <- difference(
    connection_graph,
    kin_graph,
    byname = TRUE
  )

  # remove self loops
  #nonkin_links <- delete_edges(nonkin_links, E(nonkin_links)[which_loop(nonkin_links)])
  #remove isolates : it doesn't make sense to have them here even though they exist in sum because these are structural 0s here
  #nonkin_links <- delete_vertices(nonkin_links, V(nonkin_links)[degree(nonkin_links, mode = "all") == 0])

  # node attributes
  V(nonkin_links)$su_wealth <- V(connection_graph)$su_wealth[V(nonkin_links)$name]
  V(nonkin_links)$susize <- V(connection_graph)$susize[V(nonkin_links)$name]


  return(nonkin_links)
})
names(non_primary_connectedkin_sum) <- sites



non_secondary_connectedkin_sum <- lapply(sites, function(site) {
  kin_graph <- secondary_connectedkin_sum[[site]]
  connection_graph <- sum[[site]]

  # Remove secondary kin edges from sum
  nonkin_links <- difference(
    connection_graph,
    kin_graph,
    byname = TRUE
  )

  # remove self loops
  #nonkin_links <- delete_edges(nonkin_links, E(nonkin_links)[which_loop(nonkin_links)])
  #remove isolates : it doesn't make sense to have them here even though they exist in sum because these are structural 0s here
  #nonkin_links <- delete_vertices(nonkin_links, V(nonkin_links)[degree(nonkin_links, mode = "all") == 0])

  # node attributes
  V(nonkin_links)$su_wealth <- V(connection_graph)$su_wealth[V(nonkin_links)$name]
  V(nonkin_links)$susize <- V(connection_graph)$susize[V(nonkin_links)$name]

  return(nonkin_links)
})
names(non_secondary_connectedkin_sum) <- sites


## Goal: Move from these igraph objects to a potential-edge-level
## (i.e., a row for every potential edge, not just present edges in a network)
## which has a numeric indicator for how many of those edges exists in a particular
## layer (layers are columns). This is going to be used in the PCA.
layers <-
  list(loan_req,
       loan_giv,
       exchange_req,
       exchange_giv,
       f_behav,
       m_behav,
       f_info,
       m_info,
       govt,
       external,
       primary_kin,
       secondary_kin,
       primary_connectedkin_sum,
       secondary_connectedkin_sum,
       non_primary_connectedkin_sum,
       non_secondary_connectedkin_sum)

names(layers) <-
  c("loan_req",
    "loan_giv",
    "exchange_req",
    "exchange_giv",
    "f_behav",
    "m_behav",
    "f_info",
    "m_info",
    "govt",
    "external",
    "primary_kin",
    "secondary_kin",
    "primary_connectedkin_sum",
    "secondary_connectedkin_sum",
    "non_primary_connectedkin_sum",
    "non_secondary_connectedkin_sum")


## NOTE TO TOM: we should probably revise this to deal with MK & IH, WH, WL, PC ?
sites_to_use <- setdiff(sites, c("IH","PC", "WH", "WL", "MK"))

site_dfs <- list()
for (site in sites_to_use) {
  layer_columns <- list()
  for (layer_index in 1:length(layers)) {
    net <- layers[[layer_index]][[site]]
    if (is.null(net) || vcount(net) == 0) {
      site_layer_df <- data.frame(id1 = character(), id2 = character())
      site_layer_df[[names(layers)[layer_index]]] <- numeric()
    } else {
      ## with as_adjacency_matrix we do not specify attribute = "weight" so
      ## we get a binary adjacency matrix.
      ## but if we wanted to do primary kin, we would have to use maxrel for weights.
      ## additionally, not as_adjacency_matrix is old igraph, and has perhaps
      ## been replaced with as_adjacency_matrix.
      site_layer_df <- as.data.frame(as.table(as.matrix(as_adjacency_matrix(net))))
      names(site_layer_df) <- c("id1", "id2", names(layers)[layer_index])
    }
    layer_columns[[layer_index]] <- site_layer_df
  }
  site_dfs[[site]] <-
    Reduce(function(df1, df2) merge(df1, df2, by = c("id1", "id2"), all.x = T, all.y = T), layer_columns)
  site_dfs[[site]]$site <- site
}
dataframe <- bind_rows(site_dfs)

saveRDS(dataframe, file.path(output_path, "su_pca_nets.rds"))

#### Adding in a fuller set of networks, with the constructed ones

combined_nets <-
  list(loan,
       exchange,
       behav,
       info,
       male,
       female,
       exchange_plus_female,
       union,
       # layer_sum,
       # layer_sum_gender,
       # layer_sum_externals,
       main_supp,
       main_supp_layer,
       sum,
       collapse_sum,
       main_supp_matt,
       main_supp_layer_matt,
       # sum_matt,
       # collapse_sum_matt,
       sum_pop_downweighted,
       sum_column_stochastic)

names(combined_nets) <-
  c("loan",
    "exchange",
    "behav",
    "info",
    "male",
    "female",
    "exchange_plus_female",
    "union",
    # "layer_sum",
    # "layer_sum_gender",
    # "layer_sum_externals",
    "main_supp",
    "main_supp_layer",
    "sum",
    "collapse_sum",
    "main_supp_matt",
    "main_supp_layer_matt",
    # "sum_matt",
    # "collapse_sum_matt",
    "sum_pop_downweighted",
    "sum_column_stochastic")

all_withloops <- c(layers, combined_nets)
all <- lapply(all_withloops, remove_layer_self_loops)

saveRDS(all, file = file.path(output_path, "igraphs_for_each_layer_list.rds"))

su_nets_expanded <- list()
su_nets_expanded_withloops <- list()

## restructure all to be oriented around site, rather than layer
for (type in names(all)) {
  for (site in names(all[[type]])) {
    # Create the nested structure in the reorganized list
    if (!is.list(su_nets_expanded[[site]])) {
      su_nets_expanded[[site]] <- list()
      su_nets_expanded_withloops[[site]] <- list()
    }
    su_nets_expanded[[site]][[type]] <- all[[type]][[site]]
    su_nets_expanded_withloops[[site]][[type]] <- all_withloops[[type]][[site]]
  }
}

## add in avrel, maxrel, and distance
## distance is the distance in meters between sharing units
## NOTE: will NOT want to use distance for centrality calculations

for (site in sites) {
  su_nets_expanded[[site]]$avrel <- remove_self_loops(su_nets[[site]]$avrel)
  su_nets_expanded[[site]]$maxrel <- remove_self_loops(su_nets[[site]]$maxrel)
  su_nets_expanded[[site]]$distance <- remove_self_loops(su_nets[[site]]$distance)
}

## add transposed networks for directed cases
for (site in sites) {
  for (type in setdiff(names(su_nets_expanded[[site]]), c("primary_kin","secondary_kin",
                                                          "primary_connectedkin_sum",
                                                          "secondary_connectedkin_sum",
                                                          "avrel", "maxrel", "distance"))) {
    net <- reverse_edges((su_nets_expanded[[site]][[type]]))
    name <- paste0("transp_", type)
    su_nets_expanded[[site]][[name]] <- net
  }

}

save(su_nets_expanded, file = file.path(output_path, "su_nets_expanded.rdata"))
save(su_nets_expanded_withloops, file = file.path(output_path, "su_nets_expanded_withloops.rdata"))






####### SITE AND SU DESCRIPTIVES : code errors out here, so for now skipping ############

site_descriptives <- list()

for( i in 1:length(su_nets_expanded)) {
  #print(names(su_nets_expanded)[i])
  site_descriptives[[i]] <- data.frame(net = NA,
  n_su = 0,
  n_ties = 0,
  density = 0,
  reciprocity = 0,
  transitivity = 0,
  avpathlength = 0,
  outdeg_centralization = 0,
  indeg_centralization = 0,
  bet_centralization = 0,
  mean_degree = 0,
  range_outdegree = 0,
  range_indegree = 0)

  for( net in names(su_nets_expanded[[i]])) {


    #network <- delete_vertices(su_nets_expanded[[i]][[net]], !V(su_nets_expanded[[i]][[1]])$sampled) ## removing non-sampled SUs

    ref_sampled <- setNames(V(su_nets_expanded[[i]][[1]])$sampled, V(su_nets_expanded[[i]][[1]])$name)
    network <- delete_vertices(su_nets_expanded[[i]][[net]], V(su_nets_expanded[[i]][[net]])[!ref_sampled[name]])


  site_descriptives[[i]][match(net, names(su_nets_expanded[[i]])),] <- c(
    net = net,
    n_su = vcount(network),
    n_ties = ecount(network),
    density = round(edge_density(network), 3),
    reciprocity = round(reciprocity(network), 3),
    transitivity = round(transitivity(network), 3),
    avpathlength = round(mean_distance(network), 3),
    outdeg_centralization = round((centr_degree(network, mode = "out")$centralization), 3),
    indeg_centralization = round((centr_degree(network, mode = "in")$centralization), 3),
    bet_centralization = round(centr_betw(network)$centralization, 3),
    mean_degree = round(mean(degree(network, mode = "out")), 3),
    range_outdegree = paste(min(degree(network, mode = "out")), max(degree(network, mode = "out")), sep = " - "),
    range_indegree = paste(min(degree(network, mode = "in")), max(degree(network, mode = "in")), sep = " - ")
  )
  }
}
names(site_descriptives) <- names(su_nets_expanded)

## Above creates a list object, the line below unlists that to one long df
site_descriptives_df <- imap_dfr(site_descriptives, ~ mutate(.x, site = .y))

site_descriptives_by_net <- split(site_descriptives_df, site_descriptives_df$net)

write.csv(
  site_descriptives_df,
  file = file.path(output_path, "site_net_descriptives.csv"),
  row.names = FALSE
)

## ratio of internal to external alters
externals_df <- data.frame(
    site = rep(NA_character_, length(su_alters)),
    req_alts = 0,
    req_exts = 0,
    tot_alts = 0,
    tot_exts = 0
)

for (i in 1:length(su_alters)) {
    site <- names(su_alters)[i]
    req_df <- subset(su_alters[[site]], su_alters[[site]]$tie == "req")
    all_df <- subset(su_alters[[site]], su_alters[[site]]$tie == "all")
    externals_df$site[i] <- site
    externals_df$req_alts[i] <- sum(req_df$alter_count, na.rm = TRUE)
    externals_df$req_exts[i] <- sum(req_df$externals_count, na.rm = TRUE)
    externals_df$tot_alts[i] <- sum(all_df$alter_count, na.rm = TRUE)
    externals_df$tot_exts[i] <- sum(all_df$externals_count, na.rm = TRUE)
}
externals_df$prop_ext_req <- externals_df$req_exts/externals_df$req_alts
externals_df$prop_ext_all <- externals_df$tot_exts/externals_df$tot_alts


write.csv(
  externals_df,
  file = file.path(output_path, "site_externals_descriptives.csv"),
  row.names = FALSE
)



#### SU DESCRIPTIVES

# for (site in names(su_nets)) {
#   su_nets_expanded[[site]]$distance <- NULL
# }

# calc_su_descrip <- function(net) {
#   undir_net <- as_undirected(net, mode = "collapse")

#   su_id <- V(net)$name
#   deg <- degree(net, mode = "all")
#   ideg <- degree(net, mode = "in")
#   odeg <- degree(net, mode = "out")
#   deg_norm <- normalize(degree(net, mode = "all"))
#   ideg_norm <- normalize(degree(net, mode = "in"))
#   odeg_norm <- normalize(degree(net, mode = "out"))
#   str <- strength(net, mode = "all", weights = E(net)$weight)
#   istr <- strength(net, mode = "in", weights = E(net)$weight)
#   ostr <- strength(net, mode = "out", weights = E(net)$weight)
#   bet <- betweenness(net)
#   clo <- closeness(net)
#   iclo <- closeness(net, mode = "in")
#   oclo <- closeness(net, mode = "out")
#   hub <- hub_score(net)$vector
#   aut <- authority_score(net)$vector
#   eig <- eigen_centrality(undir_net)$vector ## NOTE HERE USING UNDIR_NET! DON'T WANT SELF-LOOPS
#   pr <- page_rank(net)$vector
#   katz <- alpha_centrality(net, alpha = 0.99)
#   katz2 <- alpha_centrality(net, alpha = 0.2)
#   clu <- transitivity(net, type = "local")
#   df <- data.frame(su_id, deg, ideg, odeg, deg_norm, ideg_norm, odeg_norm, str, istr, ostr, bet, clo, iclo, oclo, hub, aut, eig, pr, katz, katz2, clu)
#   return(df)
# }

# # Other possible measures, mostly presuming weighted network
# #  str <- strength(net, mode = "all", weights = E(net)$weight)
# #  istr <- strength(net, mode = "in", weights = E(net)$weight)
# #  ostr <- strength(net, mode = "out", weights = E(net)$weight)
# #  wbet <- betweenness(net, weights = 1/E(net)$weight)
# #  wclo <- closeness(net, weights = 1/E(net)$weight)
# #  wiclo <- closeness(net, mode = "in", weights = 1/E(net)$weight)
# #  woclo <- closeness(net, mode = "out", weights = 1/E(net)$weight)
# #  whub <- hub_score(net, weights = E(net)$weight)$vector
# #  waut <- authority_score(net, weights = E(net)$weight)$vector
# #  weig <- eigen_centrality(net, weights = E(net)$weight)$vector
# #  wpr <- page_rank(net, weights = E(net)$weight)$vector

# su_descriptives <- lapply(su_nets_expanded$AH, function(x) {
#       df <- calc_su_descrip(x)
#       return(df)
#     })



# loan_su_descriptives <- lapply(loan, function(x) {
#   df <- calc_su_descrip(x)
#   names(df)[-1] <- paste0("loan_", names(df)[-1])
#   return(df)
# })

# exchange_su_descriptives <- lapply(exchange, function(x) {
#   df <- calc_su_descrip(x)
#   names(df)[-1] <- paste0("exchange_", names(df)[-1])
#   return(df)
# })

# behav_su_descriptives <- lapply(behav, function(x) {
#   df <- calc_su_descrip(x)
#   names(df)[-1] <- paste0("behav_", names(df)[-1])
#   return(df)
# })

# info_su_descriptives <- lapply(info, function(x) {
#   df <- calc_su_descrip(x)
#   names(df)[-1] <- paste0("info_", names(df)[-1])
#   return(df)
# })

# main_supp_su_descriptives <- lapply(main_supp, function(x) {
#   df <- calc_su_descrip(x)
#   names(df)[-1] <- paste0("main_supp_", names(df)[-1])
#   return(df)
# })

# kin_su_descriptives <- lapply(primary_kin, function(net) {
#   su_id <- V(net)$name
#   deg <- degree(net, mode = "all")
#   deg_norm <- normalize(degree(net, mode = "all"))
#   bet <- normalize(betweenness(net))
#   clo <- normalize(closeness(net))
#   hub <- normalize(hub_score(net)$vector)
#   aut <- normalize(authority_score(net)$vector)
#   eig <- normalize(eigen_centrality(net)$vector)
#   pr <- normalize(page_rank(net)$vector)
#   clu <- transitivity(net, type = "local")
#   all <- data.frame(su_id, deg, deg_norm, bet, clo, hub, aut, eig, pr, clu)
#   names(all)[-1] <- paste0("kin_", names(all)[-1])
#   return(all)
# })



# ## Summing the material wealth of each household's immediate neighbors (in different networks)
# ## Could have this decay with path length (at some point, I think would approach Katz centrality with the wealth being the beta_i)
# ## Something like alpha_centrality(net, exo = V(net)$su_wealth, alpha = 1/eigen_centrality(net)$value) ??
# ## V(net)$val %*% solve(diag(1, vcount(net)) - as_adjacency_matrix(net) * 0.5) - V(net)$val *should* work, but get negative values! This has "decay" of 0.5
# ## see with: net <- make_directed_graph(c(1, 2, 1, 3, 1, 4, 1, 5, 6, 2, 7, 1))
# ## see with: V(net)$val <- c(1, 2, 3, 4, 5, 6, 7)
# ## Presumably related to same issues as alpha parameter with alpha/Katz centrality
# ## Leaving this for now.
# ## Yes, it's ugly and slow, but it runs! [That could be said throughout, I appreciate]
# for( i in 1:length(types)) {
#   for( site in names(types[[i]])) {
#     for(j in 1:vcount(types[[i]][[site]])){
#       if(i == 1) loan_su_descriptives[[site]]$loan_alter_wealth_out[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(loan[[site]], mode = "out")[[j]])])
#       if(i == 1) loan_su_descriptives[[site]]$loan_alter_wealth_in[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(loan[[site]], mode = "in")[[j]])])
#       if(i == 1) loan_su_descriptives[[site]]$loan_alter_wealth_all[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(loan[[site]], mode = "all")[[j]])])
#       if(i == 2) exchange_su_descriptives[[site]]$exchange_alter_wealth_out[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(exchange[[site]], mode = "out")[[j]])])
#       if(i == 2) exchange_su_descriptives[[site]]$exchange_alter_wealth_in[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(exchange[[site]], mode = "in")[[j]])])
#       if(i == 2) exchange_su_descriptives[[site]]$exchange_alter_wealth_all[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(exchange[[site]], mode = "all")[[j]])])
#       if(i == 3) behav_su_descriptives[[site]]$behav_alter_wealth_out[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(behav[[site]], mode = "out")[[j]])])
#       if(i == 3) behav_su_descriptives[[site]]$behav_alter_wealth_in[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(behav[[site]], mode = "in")[[j]])])
#       if(i == 3) behav_su_descriptives[[site]]$behav_alter_wealth_all[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(behav[[site]], mode = "all")[[j]])])
#       if(i == 4) info_su_descriptives[[site]]$info_alter_wealth_out[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(info[[site]], mode = "out")[[j]])])
#       if(i == 4) info_su_descriptives[[site]]$info_alter_wealth_in[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(info[[site]], mode = "in")[[j]])])
#       if(i == 4) info_su_descriptives[[site]]$info_alter_wealth_all[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(info[[site]], mode = "all")[[j]])])
#       if (i == 5) main_supp_su_descriptives[[site]]$main_supp_alter_wealth_out[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(main_supp[[site]], mode = "out")[[j]])])
#       if (i == 5) main_supp_su_descriptives[[site]]$main_supp_alter_wealth_in[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(main_supp[[site]], mode = "in")[[j]])])
#       if (i == 5) main_supp_su_descriptives[[site]]$main_supp_alter_wealth_all[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(main_supp[[site]], mode = "all")[[j]])])
#       if(i == 6) kin_su_descriptives[[site]]$kin_alter_wealth[j] <- sum(su_meta[[site]]$su_wealth[su_meta[[site]]$su_id %in% names(ego(primary_kin[[site]], mode = "all")[[j]])])
#     }
#   }
# }




# su_descriptives <- list()

# for (site in names(su_meta)){
#  if (!site %in% c("IH", "MK", "PC", "WH", "WL")) su_descriptives[[site]] <- merge(merge(merge(merge(merge(merge(su_meta[[site]], loan_su_descriptives[[site]], by = "su_id"), exchange_su_descriptives[[site]], by = "su_id"), behav_su_descriptives[[site]], by = "su_id"), info_su_descriptives[[site]], by = "su_id"), main_supp_su_descriptives[[site]], by = "su_id"), kin_su_descriptives[[site]], by = "su_id")
#  if (site %in% c("IH", "PC", "WH", "WL")) su_descriptives[[site]] <- merge(merge(merge(merge(merge(su_meta[[site]], exchange_su_descriptives[[site]], by = "su_id"), behav_su_descriptives[[site]], by = "su_id"), info_su_descriptives[[site]], by = "su_id"), main_supp_su_descriptives[[site]], by = "su_id"), kin_su_descriptives[[site]], by = "su_id")
#  if (site %in% c("IH","PC","WH","WL")) su_descriptives[[site]][,colnames(loan_su_descriptives[[1]][-1])] <- NA # adding blank columns
#  if (site == "MK") su_descriptives[[site]] <- merge(merge(merge(merge(merge(su_meta[[site]], loan_su_descriptives[[site]], by = "su_id"), exchange_su_descriptives[[site]], by = "su_id"), behav_su_descriptives[[site]], by = "su_id"), main_supp_su_descriptives[[site]], by = "su_id"), kin_su_descriptives[[site]], by = "su_id")
#  if (site == "MK") su_descriptives[[site]][, colnames(info_su_descriptives[[1]][-1])] <- NA # adding blank columns
# }

# ## reordering so they're aligned
# su_descriptives <- lapply(su_descriptives, function(x) x[, colnames(su_descriptives$AH)])

# ## TO TROUBLESHOOT: Some duplicate entries of SUs are appearing here (e.g., EK, KM) -- duplicate IDs in networks! seems to stem from res_su object?

# ## need to make ordinal max edu values numeric to be able to combine
# for (site in names(su_descriptives)) {
#   su_descriptives[[site]]$edu_max <- as.numeric(su_descriptives[[site]]$edu_max)
#   su_descriptives[[site]]$edu_max_hh <- as.numeric(su_descriptives[[site]]$edu_max_hh)
#   su_descriptives[[site]]$edu_max_norm <- as.numeric(su_descriptives[[site]]$edu_max_norm)
#   su_descriptives[[site]]$edu_max_hh_norm <- as.numeric(su_descriptives[[site]]$edu_max_hh_norm)
# }


# su_df <- bind_rows(lapply(seq_along(su_descriptives), function(x) cbind(su_descriptives[[x]], site = names(su_descriptives)[x])))

# ## removing all SUs (and sites!) for which we don't have material wealth
# su_df1 <- su_df[!is.na(su_df$su_wealth),]

# su_df2 <- subset(su_df1, su_df1$edu_max_hh != -Inf)

# require(lme4)

# minfo <- lmer(su_wealth_log_norm ~ (1 | site) + su_size_norm + age_av_norm + primary_kin_norm + edu_max_hh_norm + info_pr + info_clu + info_bet, data = su_df2)
# mbehav <- lmer(su_wealth_log_norm ~ (1 | site) + su_size_norm + age_av_norm + primary_kin_norm + edu_max_hh_norm + behav_pr + behav_clu + behav_bet, data = su_df2)
# mexchange <- lmer(su_wealth_log_norm ~ (1 | site) + su_size_norm + age_av_norm + primary_kin_norm + edu_max_hh_norm + exchange_pr + exchange_clu + exchange_bet, data = su_df2)
# mloan <- lmer(su_wealth_log_norm ~ (1 | site) + su_size_norm + age_av_norm + primary_kin_norm + edu_max_hh_norm + loan_pr + loan_clu + loan_bet, data = su_df2)

# texreg::screenreg(list(mloan, mexchange, mbehav, minfo))

# mpr <- lmer(su_wealth_log_norm ~ (1 | site) + su_size_norm + age_av_norm + primary_kin_norm + edu_max_hh_norm + loan_pr + exchange_pr + behav_pr + info_pr, data = su_df2)


# plot <- ggplot(su_df1, aes(x=exchange_pr, y=su_wealth, group = site, colour = site)) +
#   geom_smooth(aes(linetype = site), method = "lm", se = FALSE) +
#   scale_y_continuous(trans='log10', labels = c(0,10,100,1000,10000,100000)) +
#   coord_trans(y = "log10") +
#   scale_linetype_manual(values = rep(c("solid", "dashed", "dotted"), length.out = length(unique(su_df1$site)))) +
#   xlab("exchange_pr") +
#   ylab("Material Wealth (USD)") +
#   #geom_text(aes(label=gsub(site)),hjust=0, vjust=0) +
#   theme_bw() +
#   theme(plot.title = element_text(size=20, face="bold"), axis.text=element_text(size=12),
#         axis.title=element_text(size=20,face="bold"),  legend.key.width = unit(1.3,"cm"))
# plot




# #### PLOTS

# # Set the dimensions of the plots
# plot_width <- 5 # inches
# plot_height <- 4 # inches

# # Set the margins around each plot
# top_margin <- 1 # inches
# bottom_margin <- 1 # inches
# left_margin <- 1 # inches
# right_margin <- 1 # inches

# # Set the spacing between each plot
# horizontal_spacing <- 0.5 # inches
# vertical_spacing <- 0.5 # inches

# # Calculate the total width and height of each plot
# total_width <- plot_width + left_margin + right_margin
# total_height <- plot_height + top_margin + bottom_margin

# # Calculate the total width and height of the grid of plots
# grid_width <- 8 * total_width + 4 * horizontal_spacing
# grid_height <- 4 * total_height + 5 * vertical_spacing


# pdf("plots/Exchange.pdf", width = grid_width, height = grid_height)

# # Set the plot parameters for all plots
# par(mar = c(top_margin, left_margin, bottom_margin, right_margin),
#     mfcol = c(4, 8), # set the grid of plots
#     oma = c(0, 0, 0, 0), # set the outer margins to zero
#     cex = 0.8) # adjust the font size to fit more text in each plot


# for (site in names(exchange)){
# plot(exchange[[site]],
#      main = site,
#      edge.width = E(exchange[[site]])$weight,
#      edge.color = adjustcolor("Grey", alpha.f = 0.5),
#      edge.curved = TRUE,
#      edge.arrow.size = 0.5,
#      vertex.label = NA,
#      vertex.size = ifelse(is.na(V(su_nets[[site]]$e4)$su_wealth), 0, scales::rescale(V(su_nets[[site]]$e4)$su_wealth, to = c(3,10)))
#      )
# }
# dev.off()

# pdf("plots/BehavAssist.pdf", width = grid_width, height = grid_height)

# # Set the plot parameters for all plots
# par(mar = c(top_margin, left_margin, bottom_margin, right_margin),
#     mfcol = c(4, 8), # set the grid of plots
#     oma = c(0, 0, 0, 0), # set the outer margins to zero
#     cex = 0.8) # adjust the font size to fit more text in each plot

# for (site in names(behav)){
#   plot(behav[[site]],
#        main = site,
#        edge.width = E(behav[[site]])$weight,
#        edge.color = adjustcolor("Grey", alpha.f = 0.5),
#        edge.curved = TRUE,
#        edge.arrow.size = 0.5,
#        vertex.label = NA,
#        vertex.size = ifelse(is.na(V(su_nets[[site]]$e4)$su_wealth), 0, scales::rescale(V(su_nets[[site]]$e4)$su_wealth, to = c(3,10)))
#   )
# }
# dev.off()

# pdf("plots/InfoShare.pdf", width = grid_width, height = grid_height)

# # Set the plot parameters for all plots
# par(mar = c(top_margin, left_margin, bottom_margin, right_margin),
#     mfcol = c(4, 8), # set the grid of plots
#     oma = c(0, 0, 0, 0), # set the outer margins to zero
#     cex = 0.8) # adjust the font size to fit more text in each plot

# for (site in names(info)){
#   plot(info[[site]],
#        main = site,
#        edge.width = E(info[[site]])$weight,
#        edge.color = adjustcolor("Grey", alpha.f = 0.5),
#        edge.curved = TRUE,
#        edge.arrow.size = 0.5,
#        vertex.label = NA,
#        vertex.size = ifelse(is.na(V(su_nets[[site]]$e4)$su_wealth), 0, scales::rescale(V(su_nets[[site]]$e4)$su_wealth, to = c(3,10)))
#   )
# }
# dev.off()

# pdf("plots/Loan.pdf", width = grid_width, height = grid_height)

# # Set the plot parameters for all plots
# par(mar = c(top_margin, left_margin, bottom_margin, right_margin),
#     mfcol = c(4, 8), # set the grid of plots
#     oma = c(0, 0, 0, 0), # set the outer margins to zero
#     cex = 0.8) # adjust the font size to fit more text in each plot

# for (site in names(loan)){
#   plot(loan[[site]],
#        main = site,
#        edge.width = E(loan[[site]])$weight,
#        edge.color = adjustcolor("Grey", alpha.f = 0.5),
#        edge.curved = TRUE,
#        edge.arrow.size = 0.5,
#        vertex.label = NA,
#        vertex.size = ifelse(is.na(V(su_nets[[site]]$e4)$su_wealth), 0, scales::rescale(V(su_nets[[site]]$e4)$su_wealth, to = c(3,10)))
#   )
# }
# dev.off()
