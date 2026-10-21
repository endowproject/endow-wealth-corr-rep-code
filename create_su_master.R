#### THIS SCRIPT CREATES SU_MASTER FILE WHICH IS USED IN REGRESSIONS WITH WEALTH

# packages
library(dplyr)
library(plyr)
library(tidyr)
library(DescTools) ## for Gini() function
library(igraph)
library(stargazer)
library(plm)
library(stringr)
library(ggrepel)


# load data
path <- file.path(EndowGitHub, "DerivedData")
load(file.path(path, "su_meta.rdata"))
load(file.path(path,"su_nets_expanded.rdata")) #list of sites and each site has network layer as igraph object
load(file.path(path, "su_alters.rdata"))

# NOTE: some sharing units were NOT sampled for the social support network questions
# recorded as "su_sampled" in su_meta and "sampled" vertex attribute in su_nets_expanded
# It only makes sense to retain their in-degree measures and turn all others to NA

# Dealing with the prospect of dividing by 0
safe_div <- function(numerator, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

# Select specific columns from each data frame (each df is a site) and compute dependent variables for reg
su_meta_filtered <- lapply(su_meta, function(df) {
  df %>%
    mutate( su_id = su_id,
            su_size = su_size,
            rank_size = percent_rank(su_size),
            adult_count = adult_count,
            num_surveyed = num_surv,
            able_count = able_count,
            able_count_pa = safe_div(able_count, adult_count),
            log_able_count_pa = log(able_count_pa),
            std_able_count_pa = (able_count_pa - mean(able_count_pa, na.rm = TRUE)) / sd(able_count_pa, na.rm = TRUE),
            std_log_able_count_pa = (log_able_count_pa - mean(log_able_count_pa, na.rm = TRUE)) / sd(log_able_count_pa, na.rm = TRUE),
            rank_able_count_pa = percent_rank(able_count_pa),
            rank_adult_count = percent_rank(adult_count),
            primary_kin = primary_kin,
            secondary_kin = secondary_kin,
            avg_age = age_av,
            max_edu = as.double(edu_max),
            wealth_per_capita = su_wealth / su_size,
            wealth_per_adult = safe_div(su_wealth, adult_count),
            wealth_per_able = safe_div(su_wealth, able_count),
            wealth = su_wealth,
            log_wealth = log(wealth),
            std_wealth = (su_wealth - mean(su_wealth,na.rm = TRUE)) / sd(su_wealth, na.rm = TRUE),
            std_log_wealth = (log_wealth - mean(log_wealth,na.rm = TRUE)) / sd(log_wealth, na.rm = TRUE),
            rank_wealth = percent_rank(wealth), #na's are kept with rank NA
            log_wpa = log(wealth_per_adult),
            log_wpable = log(wealth_per_able),
            rank_wpa = percent_rank(wealth_per_adult),
            rank_wpable = percent_rank(wealth_per_able),
            rank_wpc = percent_rank(wealth_per_capita),
            std_wpa = (wealth_per_adult - mean(wealth_per_adult,na.rm = TRUE)) / sd(wealth_per_adult, na.rm = TRUE),
            std_log_wpa = (log_wpa - mean(log_wpa,na.rm = TRUE)) / sd(log_wpa, na.rm = TRUE)
            ) %>%
    select(su_id,su_size, rank_size,adult_count,able_count,able_count_pa,num_surveyed,
           rank_able_count_pa, rank_adult_count, primary_kin,secondary_kin, avg_age, max_edu,
           wealth_per_capita, wealth_per_adult, wealth, log_wealth, rank_wealth,log_wpa,rank_wpa,
           std_wealth, std_wpa, std_able_count_pa,
           std_log_able_count_pa,std_log_wealth,std_log_wpa,
           wealth_per_able, log_wpable, rank_wpable, rank_wpc)
})

wealth_df <- lapply(names(su_meta_filtered), function(name) {
  df <- su_meta_filtered[[name]]
  df$site <- name
  return(df)
})

# Convert list of data frames with selected columns to a single data frame -
# this is a df where each row is a household
wealth_df <- bind_rows(su_meta_filtered, .id = "site")

#there are 13 duplicate su_ids all in CR, which i remove
dupes <- wealth_df[duplicated(wealth_df[c("site", "su_id")]), ]
wealth_df <- wealth_df[!duplicated(wealth_df[c("site", "su_id")]), ]


############## EDGES #######################

##### Function to calculate centralities #####
calculate_centralities <- function(df, site_name, network_name) {

  # Getting the site and network igraph object
  # Note : self-loops are retained
  directed_graph <- df[[site_name]][[network_name]]

  # undirected graph
  # collapse - one undirected edge where there exists atleast one directed
  # ex : if there is an edge from ahsu007 to ahsu004 and ahsu004 to ahsu007 this is just counted as one
  undirected_graph <- as_undirected(directed_graph, mode = "collapse")

  # centrality measures : note that all are unweighted
  out_degree <- degree(directed_graph, mode = "out")
  in_degree <- degree(directed_graph, mode = "in")
  undirected_degree <- degree(undirected_graph)
  #eigen_centrality <- eigen_centrality(undirected_graph)$vector
  #pagerank <- page_rank(undirected_graph)$vector
  #betweenness <- betweenness(undirected_graph, directed = FALSE) # for now errors out

  #katz <- alpha_centrality(undirected_graph, alpha = 0.99)
  #katz2 <- alpha_centrality(undirected_graph, alpha = 0.2)

  # setup df
  result_df <- data.frame(
    su_id = names(out_degree),
    out_degree = out_degree,
    in_degree = in_degree,
    degree = undirected_degree
    #eigen = eigen_centrality,
    #pagerank = pagerank
    #betweenness = betweenness
    #katz = katz,
    #katz2 = katz2
  )


  # weighted version
  # using existing network edge weights : ie. weights are number of person links
  # for union of multiple layers :
  # layer_weighted : if each layer is unweighted in the union - ie. max weight is number of layers
  # weighted : if each layer is both with layer weights and person weights
  # layer_sum, layer_sum_gender, layer_sum_externals, main_supp_layers : all layer weighted only (ie. no person weights)
  # sum :  # person links / num_surv and with gender fix for layers with gendered questions

  # num_surv as 0 when su listed as alter but not surveyed (but only in in-graph)
  temp_weights <- E(directed_graph)$weight
  temp_weights[is.na(temp_weights)] <- 0
  in_degree_weighted <- strength(directed_graph, mode = "in", weights = temp_weights)
  # in out graph we keep num_surv as NA because
  out_degree_weighted <- strength(directed_graph, mode = "out") # num_surv as NA when su listed as alter but not surveyed
  undirected_degree_weighted <- strength(undirected_graph) # num_surv as NA when su listed as alter but not surveyed

  #eigen_weighted <- eigen_centrality(undirected_graph, weights = E(undirected_graph)$weight)$vector
  #eigen_weighted_directed <- eigen_centrality(directed_graph, weights = E(directed_graph)$weight, directed = TRUE)$vector
  #pagerank_weighted <- page_rank(undirected_graph,weights = E(undirected_graph)$weight)$vector
  #pagerank_weighted_directed <- page_rank(directed_graph,weights = E(directed_graph)$weight, directed = TRUE)$vector
  #
  # inv_weights_uw <- 1 / E(undirected_graph)$weight
  # betweenness_weighted <- betweenness(undirected_graph, directed = FALSE, weight = inv_weights_uw)
  # inv_weights_w <- 1 / E(directed_graph)$weight
  # betweenness_weighted_directed <- betweenness(directed_graph, directed = TRUE, weights = inv_weights_w)
  #




  # Combine the results into the dataframe for weighted networks
  result_df <- result_df %>%
    mutate(
      out_degree_weighted = out_degree_weighted,
      in_degree_weighted = in_degree_weighted,
      degree_weighted = undirected_degree_weighted,
      #eigen_weighted = eigen_weighted,
      #eigen_weighted_directed = eigen_weighted_directed,
      #pagerank_weighted = pagerank_weighted,
      #pagerank_weighted_directed = pagerank_weighted_directed
      #betweenness_weighted = betweenness_weighted,
      #betweenness_weighted_directed = betweenness_weighted_directed
    )

  # consider SU sampling: if NOT sampled, only in-degree measures retained
  # always pull from the loan_req network, as not there as attribute in some aggregate nets
  sampled_df <- data.frame(
  su_id = V(su_nets_expanded[[site_name]][["loan_req"]])$name,
  sampled = V(su_nets_expanded[[site_name]][["loan_req"]])$sampled
  )
  result_df <- merge(result_df, sampled_df, by = "su_id", all.x = TRUE)

  result_df <- result_df %>%
    mutate(
      out_degree = ifelse(sampled, out_degree, NA_real_),
      out_degree_weighted = ifelse(sampled, out_degree_weighted, NA_real_),
      degree = ifelse(sampled, degree, NA_real_),
      degree_weighted = ifelse(sampled, degree_weighted, NA_real_)
      # in_degree / in_degree_weighted left as is
    )



  # merge with wealth_df to get 'adult_count'
  result_df <- merge(result_df, wealth_df[c('su_id', 'adult_count', 'su_size')], by = 'su_id', all.x = TRUE)

  # get percent ranks
  result_df <- result_df %>%
    mutate(

      ##### rank eigen, page rank and katz

      # undirected unweighted
      #rank_eigen = percent_rank(eigen_centrality),
      #rank_pagerank = percent_rank(pagerank),
      #rank_betweenness = percent_rank(betweenness),
      #rank_katz = percent_rank(katz),

      # undirected weighted
      #rank_eigen_weighted = percent_rank(eigen_weighted),
      #rank_pagerank_weighted = percent_rank(pagerank_weighted),
      #rank_betweenness_weighted = percent_rank(betweenness_weighted),

      # directed weighted
      #rank_eigen_weighted_directed = percent_rank(eigen_weighted_directed),
      #rank_pagerank_weighted_directed = percent_rank(pagerank_weighted_directed),
      #rank_betweenness_weighted_directed = percent_rank(betweenness_weighted_directed),

      #####  degree

      # rank degree, undirected, unweighted
      rank_degree = percent_rank(degree),
      degree_per_adult = degree / adult_count,
      rank_degree_peradult = percent_rank(degree_per_adult),
      degree_per_capita = degree / su_size,
      rank_degree_percap = percent_rank(degree_per_capita),



      # rank degree, undirected, weighted
      rank_degree_weighted = ifelse(!is.na(degree_weighted), percent_rank(degree_weighted), NA),
      degree_per_adult_weighted = ifelse(!is.na(degree_weighted), degree_weighted / adult_count, NA),
      rank_degree_peradult_weighted = ifelse(!is.na(degree_per_adult_weighted), percent_rank(degree_per_adult_weighted), NA),
      degree_percap_weighted = ifelse(!is.na(degree_weighted), degree_weighted / su_size, NA),
      rank_degree_percap_weighted = ifelse(!is.na(degree_percap_weighted), percent_rank(degree_percap_weighted), NA),


      #####  in-degree

      # rank in-degree, directed, unweighted
      rank_in_degree= percent_rank(in_degree),
      in_degree_peradult = in_degree / adult_count,
      rank_in_degree_peradult = percent_rank(in_degree_peradult),
      in_degree_percap = in_degree / su_size,
      rank_in_degree_percap = percent_rank(in_degree_percap),


      # rank in-degree, directed, weighted
      rank_in_degree_weighted = ifelse(!is.na(in_degree_weighted), percent_rank(in_degree_weighted), NA),
      in_degree_peradult_weighted = ifelse(!is.na(in_degree_weighted), in_degree_weighted / adult_count, NA),
      rank_in_degree_peradult_weighted =  percent_rank(in_degree_peradult_weighted),
      in_degree_percap_weighted = ifelse(!is.na(in_degree_weighted), in_degree_weighted / su_size, NA),
      rank_in_degree_percap_weighted =  percent_rank(in_degree_percap_weighted),



      #####  out -degree

      # rank out-degree, directed, unweighted
      rank_out_degree= percent_rank(out_degree),
      out_degree_peradult = out_degree / adult_count,
      rank_out_degree_peradult = percent_rank(out_degree_peradult),
      out_degree_percap = out_degree / su_size,
      rank_out_degree_percap = percent_rank(out_degree_percap),


      # rank out-degree, directed, weighted
      rank_out_degree_weighted = ifelse(!is.na(out_degree_weighted), percent_rank(out_degree_weighted), NA),
      out_degree_peradult_weighted = ifelse(!is.na(out_degree_weighted), out_degree_weighted / adult_count, NA),
      rank_out_degree_peradult_weighted =  percent_rank(out_degree_peradult_weighted),
      out_degree_percap_weighted = ifelse(!is.na(out_degree_weighted), out_degree_weighted / su_size, NA),
      rank_out_degree_percap_weighted =  percent_rank(out_degree_percap_weighted),

      ##### other
      log_degree = log(degree),
      log_degree_pa = log(degree_per_adult),
      #log_eigen = log(eigen_centrality),
      std_degree = (degree - mean(degree, na.rm = TRUE)) / sd(degree, na.rm = TRUE),
      #std_eigen = (eigen_centrality - mean(eigen_centrality, na.rm = TRUE)) / sd(eigen_centrality, na.rm = TRUE),
      std_degree_pa = (degree_per_adult - mean(degree_per_adult, na.rm = TRUE)) / sd(degree_per_adult, na.rm = TRUE),
      std_log_degree = (log_degree - mean(log_degree, na.rm = TRUE)) / sd(log_degree, na.rm = TRUE),
      #std_log_eigen = (log_eigen - mean(log_eigen, na.rm = TRUE)) / sd(log_eigen, na.rm = TRUE),
      std_log_degree_pa = (log_degree_pa - mean(log_degree_pa, na.rm = TRUE)) / sd(log_degree_pa, na.rm = TRUE)

    )

  result_df <- result_df %>% select(c(-adult_count, -su_size))
  return(result_df)
}


##### Function to calculate centralities #####
calculate_centralities_extended <- function(df, site_name, network_name) {

  # Getting the site and network igraph object
  # Note : self-loops are retained
  directed_graph <- df[[site_name]][[network_name]]

  # undirected graph
  # collapse - one undirected edge where there exists atleast one directed
  # ex : if there is an edge from ahsu007 to ahsu004 and ahsu004 to ahsu007 this is just counted as one
  undirected_graph <- as_undirected(directed_graph, mode = "collapse",
                                    edge.attr.comb = "sum")

  # centrality measures : note that all are unweighted
  out_degree <- degree(directed_graph, mode = "out")
  in_degree <- degree(directed_graph, mode = "in")
  undirected_degree <- degree(undirected_graph)
  eigen_centrality <- eigen_centrality(undirected_graph)$vector
  pagerank <- page_rank(undirected_graph)$vector
  betweenness <- betweenness(undirected_graph, directed = FALSE)
  katz_0.9 <- NA
  katz_0.2 <- NA
  katz_0.9_weighted <- NA
  katz_0.2_weighted <- NA

  katz_0.9 <- tryCatch(
    alpha_centrality(undirected_graph, loops = TRUE, alpha = 0.9, weight = NA),
    error = function(e) {
      message("Error in katz_0.9: ", e$message)
      return(rep(NA, vcount(undirected_graph)))
    }
  )

  katz_0.2 <- tryCatch(
    alpha_centrality(undirected_graph, loops = TRUE, alpha = 0.2, weight = NA),
    error = function(e) {
      message("Error in katz_0.2: ", e$message)
      return(rep(NA, vcount(undirected_graph)))
    }
  )

  katz_0.9_weighted <- tryCatch(
    alpha_centrality(undirected_graph, loops = TRUE, alpha = 0.9, weight = E(undirected_graph)$weight),
    error = function(e) {
      message("Error in katz_0.9_weighted: ", e$message)
      return(rep(NA, vcount(undirected_graph)))
    }
  )

  katz_0.2_weighted <- tryCatch(
    alpha_centrality(undirected_graph, loops = TRUE, alpha = 0.2, weight = E(undirected_graph)$weight),
    error = function(e) {
      message("Error in katz_0.2_weighted: ", e$message)
      return(rep(NA, vcount(undirected_graph)))
    }
  )


  # setup df
  result_df <- data.frame(
    su_id = names(out_degree),
    out_degree = out_degree,
    in_degree = in_degree,
    degree = undirected_degree,
    eigen = eigen_centrality,
    pagerank = pagerank,
    betweenness = betweenness,
    katz_0.9 = katz_0.9,
    katz_0.2 = katz_0.2,
    katz_0.9_weighted = katz_0.9_weighted,
    katz_0.2_weighted = katz_0.2_weighted
  )


  # weighted version
  # using existing network edge weights : ie. weights are number of person links
  # for union of multiple layers :
    # layer_weighted : if each layer is unweighted in the union - ie. max weight is number of layers
    # weighted : if each layer is both with layer weights and person weights
  # layer_sum, layer_sum_gender, layer_sum_externals, main_supp_layers : all layer weighted only (ie. no person weights)
  # sum :  # person links / num_surv and with gender fix for layers with gendered questions
  # collapse sum : sum weights collapsed to the respondent level

    # num_surv as 0 when su listed as alter but not surveyed (but only in in-graph)
    temp_weights <- E(directed_graph)$weight
    temp_weights[is.na(temp_weights)] <- 0
    in_degree_weighted <- strength(directed_graph, mode = "in", weights = temp_weights)
    # in out graph we keep num_surv as NA because
    out_degree_weighted <- strength(directed_graph, mode = "out") # num_surv as NA when su listed as alter but not surveyed
    undirected_degree_weighted <- strength(undirected_graph) # num_surv as NA when su listed as alter but not surveyed

    # Initialize centrality variables with NA

    katz_0.2_directed <- NA
    katz_0.2_directed_weighted <- NA
    katz_0.9_directed <- NA
    katz_0.9_directed_weighted <- NA

    # compute Katz  with error handling
    katz_0.2_directed <- tryCatch(
      alpha_centrality(directed_graph, loops = TRUE, alpha = 0.2, weights = NA),
      error = function(e) {
        message("Error in katz_0.2_directed: ", e$message)
        return(rep(NA, vcount(directed_graph)))
      }
    )

    katz_0.2_directed_weighted <- tryCatch(
      alpha_centrality(directed_graph, loops = TRUE, alpha = 0.2, weights = E(directed_graph)$weight),
      error = function(e) {
        message("Error in katz_0.2_directed_weighted: ", e$message)
        return(rep(NA, vcount(directed_graph)))
      }
    )

    katz_0.9_directed <- tryCatch(
      alpha_centrality(directed_graph, loops = TRUE, alpha = 0.9, weights = NA),
      error = function(e) {
        message("Error in katz_0.9_directed: ", e$message)
        return(rep(NA, vcount(directed_graph)))
      }
    )

    katz_0.9_directed_weighted <- tryCatch(
      alpha_centrality(directed_graph, loops = TRUE, alpha = 0.9, weights = E(directed_graph)$weight),
      error = function(e) {
        message("Error in katz_0.9_directed_weighted: ", e$message)
        return(rep(NA, vcount(directed_graph)))
      }
    )
    eigen_weighted <- eigen_centrality(undirected_graph, weights = E(undirected_graph)$weight)$vector
    eigen_weighted_directed <- eigen_centrality(directed_graph, weights = E(directed_graph)$weight, directed = TRUE)$vector
    pagerank_weighted <- page_rank(undirected_graph,weights = E(undirected_graph)$weight)$vector
    pagerank_weighted_directed <- page_rank(directed_graph,weights = E(directed_graph)$weight, directed = TRUE)$vector
    #
    inv_weights_uw <- 1 / E(undirected_graph)$weight
    betweenness_weighted <- betweenness(undirected_graph, directed = FALSE, weight = inv_weights_uw)
    inv_weights_w <- 1 / E(directed_graph)$weight
    betweenness_weighted_directed <- betweenness(directed_graph, directed = TRUE, weights = inv_weights_w)
    #




    # Combine the results into the dataframe for weighted networks
    result_df <- result_df %>%
      mutate(
        out_degree_weighted = out_degree_weighted,
        in_degree_weighted = in_degree_weighted,
        degree_weighted = undirected_degree_weighted,
        katz_0.2_directed = katz_0.2_directed,
        katz_0.2_directed_weighted = katz_0.2_directed_weighted,
        katz_0.9_directed = katz_0.9_directed,
        katz_0.9_directed_weighted = katz_0.9_directed_weighted,
        eigen_weighted = eigen_weighted,
        eigen_weighted_directed = eigen_weighted_directed,
        pagerank_weighted = pagerank_weighted,
        pagerank_weighted_directed = pagerank_weighted_directed,
        betweenness_weighted = betweenness_weighted,
        betweenness_weighted_directed = betweenness_weighted_directed
      )


  # consider SU sampling: if NOT sampled, only in-degree measures retained
  sampled_df <- data.frame(
  su_id = V(su_nets_expanded[[site_name]][["loan_req"]])$name,
  sampled = V(su_nets_expanded[[site_name]][["loan_req"]])$sampled
  )
  result_df <- merge(result_df, sampled_df, by = "su_id", all.x = TRUE)

  result_df <- result_df %>%
    mutate(
      out_degree = ifelse(sampled, out_degree, NA_real_),
      out_degree_weighted = ifelse(sampled, out_degree_weighted, NA_real_),
      degree = ifelse(sampled, degree, NA_real_),
      degree_weighted = ifelse(sampled, degree_weighted, NA_real_),
      eigen = ifelse(sampled, eigen, NA_real_),
      pagerank = ifelse(sampled, pagerank, NA_real_),
      betweenness = ifelse(sampled, betweenness, NA_real_),
      katz_0.9 = ifelse(sampled, katz_0.9, NA_real_),
      katz_0.2 = ifelse(sampled, katz_0.2, NA_real_),
      katz_0.9_weighted = ifelse(sampled, katz_0.9_weighted, NA_real_),
      katz_0.2_weighted = ifelse(sampled, katz_0.2_weighted, NA_real_),
      eigen_weighted = ifelse(sampled, eigen_weighted, NA_real_),
      eigen_weighted_directed = ifelse(sampled, eigen_weighted_directed, NA_real_),
      pagerank_weighted = ifelse(sampled, pagerank_weighted, NA_real_),
      pagerank_weighted_directed = ifelse(sampled, pagerank_weighted_directed, NA_real_),
      betweenness_weighted = ifelse(sampled, betweenness_weighted, NA_real_),
      betweenness_weighted_directed = ifelse(sampled, betweenness_weighted_directed, NA_real_),
      katz_0.2_directed = ifelse(sampled, katz_0.2_directed, NA_real_),
      katz_0.2_directed_weighted = ifelse(sampled, katz_0.2_directed_weighted, NA_real_),
      katz_0.9_directed = ifelse(sampled, katz_0.9_directed, NA_real_),
      katz_0.9_directed_weighted = ifelse(sampled, katz_0.9_directed_weighted, NA_real_)
      # in_degree / in_degree_weighted left as is
    )


  # merge with wealth_df to get 'adult_count'
  result_df <- merge(result_df, wealth_df[c('su_id', 'adult_count', 'su_size')], by = 'su_id', all.x = TRUE)

  # get percent ranks
  result_df <- result_df %>%
    mutate(

      ##### rank eigen, page rank and katz

      # undirected unweighted
      rank_eigen = percent_rank(eigen_centrality),
      rank_pagerank = percent_rank(pagerank),
      rank_betweenness = percent_rank(betweenness),

      # undirected weighted
      rank_eigen_weighted = percent_rank(eigen_weighted),
      rank_pagerank_weighted = percent_rank(pagerank_weighted),
      rank_betweenness_weighted = percent_rank(betweenness_weighted),

      # directed weighted
      rank_eigen_weighted_directed = percent_rank(eigen_weighted_directed),
      rank_pagerank_weighted_directed = percent_rank(pagerank_weighted_directed),
      rank_betweenness_weighted_directed = percent_rank(betweenness_weighted_directed),

      #####  degree

      # rank degree, undirected, unweighted
      rank_degree = percent_rank(degree),
      degree_per_adult = degree / adult_count,
      rank_degree_peradult = percent_rank(degree_per_adult),
      degree_per_capita = degree / su_size,
      rank_degree_percap = percent_rank(degree_per_capita),



      # rank degree, undirected, weighted
      rank_degree_weighted = ifelse(!is.na(degree_weighted), percent_rank(degree_weighted), NA),
      degree_per_adult_weighted = ifelse(!is.na(degree_weighted), degree_weighted / adult_count, NA),
      rank_degree_peradult_weighted = ifelse(!is.na(degree_per_adult_weighted), percent_rank(degree_per_adult_weighted), NA),
      degree_percap_weighted = ifelse(!is.na(degree_weighted), degree_weighted / su_size, NA),
      rank_degree_percap_weighted = ifelse(!is.na(degree_percap_weighted), percent_rank(degree_percap_weighted), NA),


      #####  in-degree

      # rank in-degree, directed, unweighted
      rank_in_degree = percent_rank(in_degree),
      in_degree_peradult = in_degree / adult_count,
      rank_in_degree_peradult = percent_rank(in_degree_peradult),
      in_degree_percap = in_degree / su_size,
      rank_in_degree_percap = percent_rank(in_degree_percap),


      # rank in-degree, directed, weighted
      rank_in_degree_weighted = ifelse(!is.na(in_degree_weighted), percent_rank(in_degree_weighted), NA),
      in_degree_peradult_weighted = ifelse(!is.na(in_degree_weighted), in_degree_weighted / adult_count, NA),
      rank_in_degree_peradult_weighted =  percent_rank(in_degree_peradult_weighted),
      in_degree_percap_weighted = ifelse(!is.na(in_degree_weighted), in_degree_weighted / su_size, NA),
      rank_in_degree_percap_weighted =  percent_rank(in_degree_percap_weighted),



      #####  out -degree

      # rank out-degree, directed, unweighted
      rank_out_degree = percent_rank(out_degree),
      out_degree_peradult = out_degree / adult_count,
      rank_out_degree_peradult = percent_rank(out_degree_peradult),
      out_degree_percap = out_degree / su_size,
      rank_out_degree_percap = percent_rank(out_degree_percap),


      # rank out-degree, directed, weighted
      rank_out_degree_weighted = ifelse(!is.na(out_degree_weighted), percent_rank(out_degree_weighted), NA),
      out_degree_peradult_weighted = ifelse(!is.na(out_degree_weighted), out_degree_weighted / adult_count, NA),
      rank_out_degree_peradult_weighted =  percent_rank(out_degree_peradult_weighted),
      out_degree_percap_weighted = ifelse(!is.na(out_degree_weighted), out_degree_weighted / su_size, NA),
      rank_out_degree_percap_weighted =  percent_rank(out_degree_percap_weighted),

      #####  katz
      rank_katz_0.9 = ifelse(!is.na(katz_0.9), percent_rank(katz_0.9), NA),
      rank_katz_0.2 = ifelse(!is.na(katz_0.2), percent_rank(katz_0.2), NA),
      rank_katz_0.9_weighted = ifelse(!is.na(katz_0.9_weighted), percent_rank(katz_0.9_weighted), NA),
      rank_katz_0.2_weighted = ifelse(!is.na(katz_0.2_weighted), percent_rank(katz_0.2_weighted), NA),
      rank_katz_0.2_directed = ifelse(!is.na(katz_0.2_directed), percent_rank(katz_0.2_directed), NA),
      rank_katz_0.2_directed_weighted = ifelse(!is.na(katz_0.2_directed_weighted), percent_rank(katz_0.2_directed_weighted), NA),
      rank_katz_0.9_directed = ifelse(!is.na(katz_0.9_directed), percent_rank(katz_0.9_directed), NA),
      rank_katz_0.9_directed_weighted = ifelse(!is.na(katz_0.9_directed_weighted), percent_rank(katz_0.9_directed_weighted), NA),


      ##### other
      log_degree = log(degree),
      log_degree_pa = log(degree_per_adult),
      #log_eigen = log(eigen_centrality),
      std_degree = (degree - mean(degree, na.rm = TRUE)) / sd(degree, na.rm = TRUE),
      #std_eigen = (eigen_centrality - mean(eigen_centrality, na.rm = TRUE)) / sd(eigen_centrality, na.rm = TRUE),
      std_degree_pa = (degree_per_adult - mean(degree_per_adult, na.rm = TRUE)) / sd(degree_per_adult, na.rm = TRUE),
      std_log_degree = (log_degree - mean(log_degree, na.rm = TRUE)) / sd(log_degree, na.rm = TRUE),
      #std_log_eigen = (log_eigen - mean(log_eigen, na.rm = TRUE)) / sd(log_eigen, na.rm = TRUE),
      std_log_degree_pa = (log_degree_pa - mean(log_degree_pa, na.rm = TRUE)) / sd(log_degree_pa, na.rm = TRUE)

    )

  result_df <- result_df %>% select(c(-adult_count, -su_size))
  return(result_df)
}

##### Calculate Centralities for Each Site and Network Layer#####
network_columns <- names(su_nets_expanded[[1]])
####### !!!! su_master is too big for GitHub! For now, let's chop it down
#######      by getting rid of the networks we don't really use.
network_columns <- network_columns[! grepl("transp", network_columns)]
network_columns <- network_columns[! grepl("matt", network_columns)]
site_names <- names(su_nets_expanded)
result_list <- list()

## Doing this restriction means it works always for alpha = 0.9.

## We should check at some point that these graphs are strongly connected.

# network_columns_centrality <-
#   c("sum",
#     "collapse_sum",
#     "main_supp",
#     "sum_pop_downweighted",
#     "transp_sum",
#     "transp_collapse_sum",
#     "transp_main_supp",
#     "transp_sum_pop_downweighted")

network_columns_centrality <-
  c("sum",
    "collapse_sum",
    "main_supp",
    "sum_pop_downweighted",
    "sum_column_stochastic",
    "external")

for (site_name in site_names) {
  for (network_name in network_columns) {
    if (is.null(su_nets_expanded[[site_name]][[network_name]]) == FALSE) {
      if (network_name %in% network_columns_centrality) {
        # stopifnot(is_connected(su_nets_expanded[[site_name]][[network_name]], mode = "strong"))\
        # lapply(site_names, function(site){is_connected(su_nets_expanded[[site]][["sum"]], mode = "strong")})
        # So we shouldn't use eigenvector centrality on the directed graphs,
        # because eigenvector centrality is not well-defined on directed graphs.
        result_df <- calculate_centralities_extended(su_nets_expanded, site_name, network_name)
      } else {
        result_df <- calculate_centralities(su_nets_expanded, site_name, network_name)
      }
      if (nrow(result_df) != 0) {
        #print(site_name)
        #print(network_name)

        result_df$site <- site_name
        result_df$network <- network_name
        result_list <- append(result_list, list(result_df))
      }
    }
  }
}

## Add NAs for the columns we haven't done for some networks, to enable them to rbind. (this is what bind_rows does)

# centrality_df <- do.call(rbind, result_list)
centrality_df <- do.call(bind_rows, result_list)

# combining centrality network measures with wealth per capita
master_df <- merge(centrality_df, wealth_df, by = c("su_id", "site"))


## add site-level gini :
sitegini <- master_df %>%
  filter(network == "sum") %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    gini_wealth = Gini(
      wealth[!(wealth == 0 | is.na(wealth) | is.nan(wealth)) | is.nan(wealth) | is.infinite(wealth)]
    ),
    gini_wealth_per_adult = Gini(
      wealth_per_adult[!(wealth_per_adult == 0 | is.na(wealth_per_adult) | is.nan(wealth_per_adult) | is.infinite(wealth_per_adult))]
    ),
    gini_wealth_per_capita = Gini(
      wealth_per_capita[!(wealth_per_capita == 0 | is.na(wealth_per_capita) | is.nan(wealth_per_capita) | is.infinite(wealth_per_capita))]
    )
  )

master_df <- master_df %>% left_join(sitegini, by = "site")


### EXTERNAL TIES

master_df_filtered <- master_df %>% filter(network == "sum") # so that there aren't duplicate su_ids
## su_alters has records of all (ext + int) alters for different ties,
## so limiting to just req, i.e., 1, 3, 5, 6, 7, 8, 9, 10
## note only has sampled SUs
su_alters_req <- lapply(su_alters, function(x) {
  x <- subset(x, x$tie == "req")
  x <- merge(x, master_df_filtered[c("su_id", "adult_count", "num_surveyed","su_size")],
    by.x = "sui", by.y = "su_id",
    all.x = TRUE
  )
  ## NAs in externals_count are if trying to count null list;


  ## Can be turned to 0
  x$externals_count[is.na(x$externals_count)] <- 0
  x$alter_count[is.na(x$alter_count)] <- 0
  x$alter_res_count[is.na(x$alter_res_count)] <- 0
  x$rank_ext_count <- percent_rank(x$externals_count)
  x$rank_alt_count <- percent_rank(x$alter_count)
  x$rank_res_count <- percent_rank(x$alter_res_count)
  x$ext_count_per_adult <- x$externals_count / x$adult_count
  x$rank_ext_count_peradult <- percent_rank(x$ext_count_per_adult)
  x$ext_count_per_surv <- x$externals_count / x$num_surveyed
  x$rank_ext_count_persurv <- percent_rank(x$ext_count_per_surv)
  x$alt_count_per_surv <- x$alter_count / x$num_surveyed
  x$res_count_per_surv <- x$alter_res_count / x$num_surveyed
  x$rank_alt_count_persurv <- percent_rank(x$alt_count_per_surv)
  x$rank_res_count_persurv <- percent_rank(x$res_count_per_surv)
  #x$ext_count_per_capita <- x$externals_count / x$su_size
  #x$rank_ext_count_percapita <- percent_rank(x$ext_count_percapita)
  x$prop_external <- x$externals_count / x$alter_count
  x$rank_prop_ext <- percent_rank(x$prop_external)

  x %>% dplyr::select(
    sui,
    num_surveyed,
    adult_count,
    su_size,
    alter_count,
    alter_res_count,
    rank_res_count,
    externals_count,
    externals_status_count,
    externals_wealth_count,
    rank_ext_count,
    rank_alt_count,
    ext_count_per_adult,
    rank_ext_count_peradult,
    ext_count_per_surv,
    rank_ext_count_persurv,
    alt_count_per_surv,
    res_count_per_surv,
    rank_alt_count_persurv,
    rank_res_count_persurv,
    #ext_count_per_capita,
    #rank_ext_count_percapita,
    prop_external,
    rank_prop_ext
  )
})

su_alters_req_df <- bind_rows(
  lapply(names(su_alters_req), function(name) {
    df <- su_alters_req[[name]]
    df$site <- name  # Add a new column with the name
    return(df)
  })
)

# combining su_alters_req with full wealth per capita data
su_alters_req_df <- merge(master_df_filtered, su_alters_req_df,
  by.x = c("su_id", "site", "adult_count", "num_surveyed","su_size"),
  by.y = c("sui", "site", "adult_count", "num_surveyed","su_size"),
  suffixes = c("", "")
)

## adding gini
su_alters_req_df <- su_alters_req_df %>%
                    left_join(sitegini, by = "site",
                              suffix = c("",""))


n_sites = length(unique(master_df$site))
n_su = length(unique(master_df$su_id))
message(paste0("number of unique sites:", n_sites))
message(paste0("number of sharing units:",n_su))


### Save this master_df dataset
message("Saving master_df")
saveRDS(master_df, file.path(path, "su_master.rds"))
# write.csv(master_df, file.path(EndowDropbox, "Data", "su_master.csv"), row.names = FALSE)
write.csv(su_alters_req_df, file.path(path, "su_externals_df.csv"), row.names = FALSE)


#
# ### summary stats on masterdf
#
# summstat <- master_df %>%
#   dplyr::group_by(site, network) %>%
#   dplyr::summarise(
#     in_degree_mean = mean(in_degree, na.rm = TRUE),
#     in_degree_var = var(in_degree, na.rm = TRUE),
#     out_degree_mean = mean(out_degree, na.rm = TRUE),
#     out_degree_var = var(out_degree, na.rm = TRUE),
#     degree_mean = mean(degree, na.rm = TRUE),
#     degree_var = var(degree, na.rm = TRUE),
#     wealth_var =  var(wealth, na.rm = TRUE)
#     #eigen_centrality_mean = mean(eigen, na.rm = TRUE),
#     #eigen_centrality_var = var(eigen, na.rm = TRUE),
#     #page_rank_mean = mean(pagerank, na.rm = TRUE),
#     #page_rank_var = var(pagerank, na.rm = TRUE)
#   )
#
#
# ## Variance - site-level
#
# vardf <- summstat %>%
#           filter(network == "union")  %>%
#           select(site, degree_var, wealth_var)
#
# varplot <- ggplot(vardf, aes(x = degree_var, y = wealth_var)) +
#   geom_point() +
#   geom_text_repel(aes(label = site), size = 3, max.overlaps = Inf) +
#   labs(
#     x = "Variance of Degree",
#     y = "Variance of Wealth",
#     title = "Variance of Wealth vs Variance of Degree for Each Site"
#   )
#
#
# ggsave(file = file.path(EndowDropbox, "Figures/var-var.png") , varplot)
#
#
# vardf_noBM <- vardf[vardf$site != "BM", ]
# varplot2 <- ggplot(vardf_noBM, aes(x = degree_var, y = wealth_var)) +
#   geom_point() +
#   geom_text_repel(aes(label = site), size = 3, max.overlaps = Inf) + # Use geom_text_repel to avoid overlaps
#   labs(
#     x = "Variance of Degree",
#     y = "Variance of Wealth",
#     title = "Variance of Wealth vs Variance of Degree for Each Site"
#   )
# ggsave(file = file.path(EndowDropbox, "Figures/var-var_noBM.png") , varplot2)
