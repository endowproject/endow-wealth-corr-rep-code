# Modularity analysis

start.time <- Sys.time()

library(tidyverse)
library(igraph)

path <- file.path(EndowGitHub, "DerivedData")

load(file.path(path, "su_nets_expanded.rdata"))

node_dat <- readRDS(file.path(path, "su_master.rds"))
node_dat <- node_dat[node_dat$network == "sum", ]
#node level data for the union graph


#MODULARITY FUNCTIONS

modularity <- function(edge_list, wealth_cuts){

  edge_list_community <- edge_list %>%
    mutate(su_community = length(wealth_cuts)) %>%
    mutate(alter_community = length(wealth_cuts))

  #list of possible wealth splits to calculate modularity over
  wealth_cuts <- c(min(edge_list$su_wealth)-1, wealth_cuts, max(edge_list$su_wealth))

  for(i in 1:(length(wealth_cuts) - 1)){

    #different communities by each of the wealth cuts -
    #in the applications below, there are just 2
    edge_list_community <- edge_list_community %>%
      mutate(su_community = if_else(wealth_cuts[i] <= su_wealth &
                                      su_wealth <= wealth_cuts[i+1], i, su_community) ) %>%
      mutate(alter_community = if_else(wealth_cuts[i] <= alter_wealth & alter_wealth <= wealth_cuts[i+1], i,
                                       alter_community) )

  }

  #attach the ego and alter degrees do the dataset for the modularity calculation
  su_degree <- edge_list_community %>%
    dplyr::group_by(su) %>%
    dplyr::summarise(su_degree = sum(edges))
  alter_degree <- edge_list_community %>%
    dplyr::group_by(alter) %>%
    dplyr::summarise(alter_degree = sum(edges))
  full_data <- edge_list_community %>%
    left_join(su_degree, by = "su") %>%
    left_join(alter_degree, by = "alter")

  #full number of edges for modularity calculation
  total_edges <- sum(full_data$edges)

  # calculate modularity
  modularity_data <- full_data %>%
    mutate(modularity_contribution = (edges - su_degree * alter_degree / (total_edges)) * (su_community == alter_community)) %>%
    dplyr::summarise(modularity = (1 / (2 * total_edges)) * sum(modularity_contribution))
  #calculation doesn't change with multiplicative constant applied to all edges

  return(modularity_data$modularity)

}

# generate all the cutpoints in the wealth distribution
generate_wealth_cuts <- function(wealth) {
  wealth_cuts <- numeric(length(wealth) - 1)
  for (i in 1:(length(wealth) - 1)) {
    wealth_cuts[i] <- (wealth[i] + wealth[i + 1]) / 2
  }
  return(c(min(wealth) - 1, wealth_cuts, max(wealth) + 1))
}

#calculate modularity for each wealth cut - m
compare_modularity <- function(edge_list_func, type){

  wealth_df <- edge_list_func %>%
    dplyr::select(su, su_wealth) %>%
    distinct()

  wealth <- wealth_df$su_wealth
  sorted_wealth <- sort(wealth)
  wealth_cuts <- generate_wealth_cuts(sorted_wealth)
  compare_modularity <- numeric(length(wealth_cuts))
  wealth_cut_matrix <- NA

  if(type == 1){
    #make one wealth cut
    for(i in 1:length(wealth_cuts) ){
      wealth_cut <- wealth_cuts[i]
      compare_modularity[i] <- modularity(edge_list_func, c(wealth_cut))
    }
    compare_modularity_matrix <- NA
    n_wealth_cuts <- length(wealth_cuts)
  }

  if(type == 2){
    #make two wealth cuts at the same time
    wealth_cut_matrix <- expand.grid(wealth_cuts, wealth_cuts) %>%
      mutate(min_cut =pmin(Var1, Var2), max_cut = pmax(Var1, Var2)) %>%
      dplyr::select(min_cut, max_cut) %>%
      filter(min_cut != max_cut) %>%
      distinct() %>%
      arrange(min_cut, max_cut)

    n_wealth_cuts <- nrow(wealth_cut_matrix)

    for(i in 1:nrow(wealth_cut_matrix) ){
      wealth_cuts_i <- unlist(wealth_cut_matrix[i,])
      compare_modularity[i] <- modularity(edge_list_func, wealth_cuts_i)
    }
    compare_modularity_matrix <- cbind(wealth_cut_matrix, compare_modularity)
  }

  # return the compare_modularity_matrix matrix for two cuts and the compare_modularity vector for one cut
  return(list(
    compare_modularity_matrix = compare_modularity_matrix,
    compare_modularity = compare_modularity,
    wealth_cut_matrix = wealth_cut_matrix,
    order_wealth = order(wealth),
    wealth_cuts = wealth_cuts,
    n_wealth_cuts = n_wealth_cuts
  ))

}

#null distribution function for wealth modularity
generate_random_graph_wealth <- function(edge_list_func){

  #permute wealth
  su_individual_index <- edge_list_func %>% dplyr::select(su, su_wealth) %>%
    distinct() %>%
    mutate(new_su_wealth = sample(su_wealth))
  alter_individual_index <- su_individual_index %>%
    dplyr::rename(alter = su) %>%
    dplyr::rename(new_alter_wealth = new_su_wealth)

  #attach permuted wealth edge list
  network_edge_list <- edge_list_func %>%
    left_join(su_individual_index, by = "su") %>%
    left_join(alter_individual_index, by = "alter") %>%
    mutate(su_wealth = new_su_wealth, alter_wealth = new_alter_wealth) %>%
    dplyr::select(su, alter, su_wealth, alter_wealth, edges)

  return(network_edge_list)

}

#create null distribution using generate_random_graph_wealth function
null_distribution <- function(edge_list_func, type, sims, n_wealth_cuts){

  null_dist_matrix <- matrix(NA, nrow = sims, ncol = n_wealth_cuts)

  for(i in 1:sims){

    sim_graph <- generate_random_graph_wealth(edge_list_func)

    null_mod_obj <- compare_modularity(sim_graph, type)

    null_dist_matrix[i,] <- null_mod_obj$compare_modularity

  }

  return(null_dist_matrix)

}

general_modularity <- function(edge_df){

  #in and out degrees
  su_degree <- edge_df %>%
    dplyr::group_by(su) %>%
    dplyr::summarise(su_degree = sum(edges))

  alter_degree <- edge_df %>%
    dplyr::group_by(alter) %>%
    dplyr::summarise(alter_degree = sum(edges))

  total_edges <- sum(edge_df$edges)

  pop_size <- nrow(su_degree)

  adjacency_matrix <- matrix(data = 0, nrow = pop_size, ncol = pop_size)

  su_id_df <- su_degree %>% dplyr::select(su) %>% arrange(su) %>% mutate(su_id = 1:pop_size)

  alter_id_df <- alter_degree %>% dplyr::select(alter) %>% arrange(alter) %>% mutate(alter_id = 1:pop_size)

  su_degree <- su_degree %>%
    left_join(su_id_df, by = "su") %>%
    arrange(su_id)

  alter_degree <- alter_degree %>%
    left_join(alter_id_df, by = "alter") %>%
    arrange(alter_id)

  edge_list <- edge_df %>%
    left_join(su_id_df, by = "su") %>%
    left_join(alter_id_df, by = "alter") %>%
    dplyr::select(su_id, alter_id, edges)

  for(i in 1:nrow(edge_list)){
    edge <- edge_list[i,]
    adjacency_matrix[edge$su_id, edge$alter_id] <- edge$edges
  }

  degree_matrix <- outer(su_degree$su_degree, alter_degree$alter_degree)

  #B matrix as specified in Newman
  modularity_matrix <- adjacency_matrix - degree_matrix/(total_edges)

  mod_eigen <- eigen(modularity_matrix)

  #membership from the first eigenvector
  gen_membership <- 1 + -2*as.numeric(Re(mod_eigen$vectors[,1]) < 0)

  first_mod_calc <- (1/(2*total_edges)) * t(gen_membership) %*% modularity_matrix %*% gen_membership

  su_membership <- data.frame(su_id = 1:pop_size, su_community = gen_membership) %>%
    left_join(su_id_df, by = "su_id")

  alter_membership <- data.frame(alter_id = 1:pop_size, alter_community = gen_membership) %>%
    left_join(alter_id_df, by = "alter_id")

  full_data <- edge_df %>%
    left_join(su_degree, by = "su") %>%
    left_join(alter_degree, by = "alter") %>%
    left_join(su_membership, by = "su") %>%
    left_join(alter_membership, by = "alter")

  #calculate modularity
  modularity_data <- full_data %>%
    mutate(modularity_contribution = (edges - su_degree*alter_degree/(total_edges))* (su_community*alter_community)) %>%
    dplyr::summarise(modularity = (1/(2*total_edges)) * sum(modularity_contribution))

  #dispersion of wealth (in case this is useful)
  community_var <- full_data %>% dplyr::select(su, su_wealth) %>%
    distinct() %>%
    arrange(su) %>%
    left_join(su_membership, by = "su") %>%
    dplyr::group_by(su_community) %>%
    dplyr::summarise(w_d = sd(su_wealth))

  wealth_dispersion <- sum(community_var$w_d)

  return(list(
    modularity = modularity_data$modularity,
    membership = gen_membership,
    wealth_dispersion = wealth_dispersion,
    community_var = community_var
  ))

}

#END OF MODULARITY FUNCTIONS


wealth_vars <- c("wealth_per_capita", "wealth")

for (wealth_var in wealth_vars) {

  # derive the corresponding gini column name
  gini_col <- paste0("gini_", wealth_var)


  #EXTRACT NODE LEVEL CHARACTERISTICS

  su_node_wealth_data <- list()
  alter_node_wealth_data <- list()

  for (site in unique(node_dat$site)) {

    node_site_data <-
      node_dat[node_dat$site == site,
               c("su_id",
                 "site",
                 wealth_var#,
                 #"adult_count"
                 )]


    node_site_data <- node_site_data %>%
      filter(!is.na(.data[[wealth_var]])) # & !is.na(adult_count))

    su_node_wealth_data[[site]] <- node_site_data %>%
      dplyr::rename(su = su_id,
             su_site = site,
             #su_adult_count = adult_count,
             su_wealth = !!sym(wealth_var))

    alter_node_wealth_data[[site]] <- node_site_data %>%
      dplyr::rename(alter = su_id,
             alter_site = site,
             #alter_adult_count = adult_count,
             alter_wealth = !!sym(wealth_var))

  }


  #EXTRACT GRAPH DATA

  nets_data <- su_nets_expanded

  ggplot_list <- list()

  sites <- names(nets_data)
  final_sites <- c()

  data_list <- list()

  #all of the types of graphs
  total_graph_names <- names(nets_data[[1]])

  #we are analyzing the union graph
  graph_names <- c("sum")

  for(i in 1:length(nets_data)){

    graphs <- list()

    #formation of master edgelist
    for (j in 1:length(graph_names)){

      graph <- as_directed(nets_data[[i]][[graph_names[j]]])

      #solution from Analysis of weighted networks by Newman https://arxiv.org/pdf/cond-mat/0407503
      #modularity results extend to non-integer weights

      #create edge list
      graph_edge_list <- data.frame(cbind(as_edgelist(graph),
                                          weight = E(graph)$weight))%>%
        mutate(weight = as.double(weight))
      names(graph_edge_list) <- c("su", "alter", "weight")
      graph_edge_list <- graph_edge_list %>%
        filter(su != alter)

      #add a node level effects to edgelist
      data <- graph_edge_list %>%
        as_tibble() %>%
        complete(su = V(graph)$name, alter = V(graph)$name, fill = list(weight = 0)) %>%
        #filter(su != alter) %>% - include, but set to zero
        dplyr::select(su, alter, weight) %>%
        #add the node characteristics
        left_join(alter_node_wealth_data[[sites[i]]], by = "alter") %>%
        left_join(su_node_wealth_data[[sites[i]]], by = "su") %>%
        mutate(wealth_dif = abs(su_wealth - alter_wealth)) %>%
        filter(!is.na(wealth_dif)) %>%
        mutate(binary_weight = as.double(weight > 0)) %>%
        mutate(site = sites[i])

      graphs[[graph_names[j]]] <- data

    }

    final_sites <- c(final_sites, sites[i])
    data_list[[sites[i]]] <- graphs

  }

  #put all of the node level data together
  master_node_data <- do.call("rbind", su_node_wealth_data) %>%
    dplyr::select(su, su_site, su_wealth) %>%
    mutate(site_num = as.numeric(as.factor(su_site))) %>%
    arrange(site_num)

  #gini information for sites
  gini <- node_dat %>%
    dplyr::select(site, !!sym(gini_col)) %>%
    mutate(site_num = as.numeric(as.factor(site))) %>%
    arrange(site_num) %>%
    unique()


  #MODULARITY CALCULATION FOR SITES

  site_compare <- list()
  site_null_distribution_list <- list()
  site_divides <- list()
  site_divides_wealth <- list()
  site_max_mod <- list()
  site_wealth_cuts <- list()
  null_sims <- 500
  type <- 1

  site_wealth_cuts <- list()

  graphs <- list()

  for(i in 1:length(final_sites)){
    message(paste("Processing modularity for site", final_sites[i], "for wealth variable:", wealth_var))

    graph_compare <- list()
    graph_null_distribution_list <- list()
    graph_divides <- numeric(length(graph_names))
    graph_wealth_cuts <- list()

    if (type == 2){
      graph_divides_wealth <- matrix(ncol = 2, nrow = length(graph_names))
    } else{
      graph_divides_wealth <- numeric(length(graph_names))
    }

    site_graph <- data_list[[i]][[1]]

    #relevant parts of edgelist
    data_df <- site_graph %>%
      dplyr::select(su, alter, su_wealth, alter_wealth, weight) %>%
      dplyr::mutate(edges = weight) %>%
      dplyr::select(su, alter, su_wealth, alter_wealth, edges)

    #compare modularity
    mod_obj <- compare_modularity(data_df, type = type)

    #pick out maximum wealth cut by modularity
    if(type == 2){
      site_divides_wealth[[i]]  <- unlist(mod_obj$wealth_cut_matrix[which.max(mod_obj$compare_modularity), ])
    }else {
      site_divides_wealth[[i]]  <- mod_obj$wealth_cuts[which.max(mod_obj$compare_modularity)]
    }

    #number of wealth cuts
    n_wealth_cuts <- mod_obj$n_wealth_cuts

    #generate null distribution
    final_null_matrix <- null_distribution(data_df, type, null_sims, n_wealth_cuts)

    #all of the modularity compared
    site_compare[[i]] <- mod_obj

    #store the null distribution
    site_null_distribution_list[[i]] <- final_null_matrix

    #index of maximum modularity
    site_divides[[i]] <- which.max(mod_obj$compare_modularity)

    #max modularity
    site_max_mod[[i]] <- max(mod_obj$compare_modularity)

  }


  #SAVE MODULARITY FIGURE

  null_sims <- 500

  divide_vec <- numeric(length = length(final_sites))
  quantile_divide_vec <- numeric(length = length(final_sites))

  #assess which modularities are signficant
  sig_vec <- numeric(length = length(final_sites))
  quant_vec <- numeric(length = length(final_sites))

  for(i in 1:length(final_sites)){
    max_modularity <- max(site_compare[[i]]$compare_modularity)
    max_null_vec <- numeric(null_sims)
    for(j in 1:null_sims){
      max_null_vec[j] <- max(site_null_distribution_list[[i]][j,])
    }

    quant_vec[i] <-  quantile(max_null_vec, 0.95)
    sig_vec[i] <- max_modularity > quantile(max_null_vec, 0.95)
    message(paste0("Max modularity for ", final_sites[i], ": ", max_modularity), ". Significant: ", sig_vec[i])
  }

  final_df <- data.frame()
  j <- 1
  #store modularity related data in dataframe
  for(i in 1:length(final_sites)){
    site_df <- data.frame(
      wealth_cut_abs = site_compare[[i]]$wealth_cuts,
      modularity = site_compare[[i]]$compare_modularity,
      normed_mod = site_compare[[i]]$compare_modularity / max(site_compare[[i]]$compare_modularity),
      wealth_cut = 1:length(site_compare[[i]]$compare_modularity),
      site = final_sites[i],
      significant = sig_vec[i],
      quantile = quant_vec[i],
      norm = max(site_compare[[i]]$compare_modularity) - quant_vec[i]
    ) %>% mutate(first_plot = 0)
    if(sig_vec[i] == 0){
      if(j %% 2 == 0){
        site_df <- site_df %>% mutate(first_plot = 1)
      }
      j <- j + 1
    }
    final_df <- rbind(final_df, site_df)
  }

  final_df <- final_df %>%
    dplyr::select(wealth_cut_abs, modularity, wealth_cut, site, significant, quantile, norm)

  names(final_df) <- c("wealth_cut", "modularity", "n_wealth_cut", "site", "significant", "null_quantile", "max_normed_modularity")

  out_path <- file.path(path, paste0("modularity_data_set_", wealth_var, ".rds"))
  message(paste0("Saving modularity data set to ", out_path))
  saveRDS(final_df, out_path)


  #GENERATE PLOT
  sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)

  gini <- node_dat %>%
    dplyr::select(site, !!sym(gini_col)) %>%
    mutate(site_num = as.numeric(as.factor(site))) %>%
    arrange(site_num) %>%
    unique()

  plot_final_df <- final_df %>% left_join(gini, by = "site")
  plot_modularity <- plot_final_df %>%
    dplyr::select(site, max_normed_modularity, !!sym(gini_col)) %>%
    distinct() %>%
    ggplot(aes(x = max_normed_modularity, y = .data[[gini_col]])) +
    #geom_point() +
    geom_smooth(method = "lm") +
    #ggrepel::geom_label_repel(aes(label = site)) +
    geom_label(
      aes(label = site),
      family = "roboto",
      size = 1.5,
      alpha = 0.5,
      linewidth = 0.25,
      label.padding = unit(0.3, "lines"),
      label.r = unit(0.6, "lines")
    ) +
    labs(
      x = "Normed Modularity",
      y = paste0("Gini (", gsub("_", " ", wealth_var), ")")
    ) +
    theme_classic(base_size = 15)

  ggsave(file.path(EndowDropbox, "Figures", "Modularity",
                   paste0("mod_gini_cor_", wealth_var, ".pdf")),
         plot_modularity, width = 8, height = 6)

}

end.time <- Sys.time()
time.taken <- round(difftime(end.time, start.time, units = "hours"), 2)
message(paste0("Modularity analysis completed in ", time.taken, " hours"))