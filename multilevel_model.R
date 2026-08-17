library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(tidyr)
library(DescTools) ## for Gini() function
library(stargazer)
library(plm)
library(stringr)
library(reshape)
library(xtable)
library(Hmisc)
library(tidyverse)
require(gridExtra)
require(rstan)
require(latex2exp)
library(cNORM)
library(cmdstanr)
library(posterior)

mod <- cmdstan_model(
  file.path(EndowGitHub, "multilevel_model_mcmc.stan"),
  cpp_options = list(stan_threads = TRUE)
)

extract_cmdstanr <- function(fit) {
  draws <- fit$draws(format = "draws_df")
  vars <- setdiff(names(draws), c(".chain", ".iteration", ".draw"))
  out <- list()
  for (v in unique(sub("\\[.*\\]", "", vars))) {
    matched <- vars[sub("\\[.*\\]", "", vars) == v]
    if (length(matched) == 1 && matched == v) {
      out[[v]] <- draws[[v]]
    } else {
      out[[v]] <- as.matrix(draws[, matched])
    }
  }
  out
}

path <- file.path(EndowGitHub, "DerivedData")
node_dat <- readRDS(file.path(path, "su_master.rds"))
load(file.path(path, "su_nets_expanded.rdata"))
node_dat <- node_dat[node_dat$network == "main_supp", ]
sites <- names(su_nets_expanded)

dir.create(file.path(path, "multilevel_model_output"), showWarnings = FALSE, recursive = TRUE)

out_folder <- file.path(path, "multilevel_model_output")

gini <- read.csv(file.path(path, "gini_site_data.csv")) %>%
  dplyr::mutate(site_num = as.numeric(as.factor(site))) %>%
  arrange(site_num)

summarize <- dplyr::summarise
rename <- dplyr::rename

node_dat <- node_dat %>% dplyr::mutate(num_surveyed_clean = ifelse(is.na(num_surveyed), 1, num_surveyed))

wealth_vars <- c("wealth", "wealth_per_capita")

for (wealth_var in wealth_vars) {

  gini_col <- paste0("gini_", wealth_var)

  su_node_wealth_data <- list()

  alter_node_wealth_data <- list()

  for (site in unique(node_dat$site)) {

    node_site_data <-
      node_dat[node_dat$site == site,
               c("su_id",
                 "site",
                 wealth_var,
                 "su_size",
                 "num_surveyed")]

    node_site_data <- node_site_data %>%
      dplyr::filter(!is.na(.data[[wealth_var]]) & !is.na(su_size)) %>%
      dplyr::mutate(
        weighted_rank = weighted.rank(.data[[wealth_var]], weights = su_size) /
          nrow(node_site_data)
      ) %>% ## normalize ranks to be between 0 and 1
      dplyr::mutate(higher_class = weighted_rank >= 0.5) %>%
      dplyr::mutate(q1 = weighted_rank < 0.25) %>%
      dplyr::mutate(q2 = weighted_rank >= 0.25 & weighted_rank < 0.5) %>%
      dplyr::mutate(q3 = weighted_rank >= 0.5 & weighted_rank < 0.75) %>%
      dplyr::mutate(q4 = weighted_rank >= 0.75)


    su_node_wealth_data[[site]] <- node_site_data %>%
      rename(su = su_id,
             su_site = site,
             su_size = su_size,
             su_wealth = !!sym(wealth_var),
             su_weighted_rank = weighted_rank,
             su_higher_class = higher_class,
             su_q1 = q1,
             su_q2 = q2,
             su_q3 = q3,
             su_q4 = q4,
             su_num_surveyed = num_surveyed)

    alter_node_wealth_data[[site]] <- node_site_data %>%
      rename(alter = su_id,
             alter_site = site,
             alter_size = su_size,
             alter_wealth = !!sym(wealth_var),
             alter_weighted_rank = weighted_rank,
             alter_higher_class = higher_class,
             alter_q1 = q1,
             alter_q2 = q2,
             alter_q3 = q3,
             alter_q4 = q4,
             alter_num_surveyed = num_surveyed)

  }

  ggplot_list <- list()

  model_list <- list()

  data_list <- list()

  for(i in 1:length(su_nets_expanded)){

    #formation of edgelist
    graph <- su_nets_expanded[[i]]$main_supp
    graph_edge_list <- data.frame(cbind(as_edgelist(graph), weight = edge_attr(graph, "weight"))) %>%
      dplyr::mutate(weight = as.double(weight))
    names(graph_edge_list) <- c("su", "alter", "weight")
    graph_edge_list <- graph_edge_list %>%
      dplyr::filter(su != alter)

    #add a node level effect

    #remove nodes with num_surveyed = NA
    data <- graph_edge_list %>%
      as_tibble() %>%
      dplyr::select(su, alter, weight) %>%
      complete(su = V(graph)$name, alter=V(graph)$name, fill = list(weight = 0)) %>%
      dplyr::filter(su != alter) %>%
      dplyr::mutate(true_weight = weight) %>%
      dplyr::select(su, alter, true_weight) %>%
      left_join(alter_node_wealth_data[[sites[i]]], by = "alter") %>%
      left_join(su_node_wealth_data[[sites[i]]], by = "su") %>%
      dplyr::mutate(wealth_dif = abs(su_wealth - alter_wealth)) %>%
      dplyr::mutate(log_wealth_dif = abs(log(su_wealth) - log(alter_wealth))) %>%
      dplyr::mutate(total_size =  su_size + alter_size) %>%
      dplyr::mutate(total_size_surv = su_num_surveyed + alter_size) %>%
      dplyr::mutate(class_connection = case_when(
        alter_higher_class == su_higher_class & su_higher_class == 1   ~ "higher",
        alter_higher_class == su_higher_class & su_higher_class == 0   ~ "lower",
        alter_higher_class != su_higher_class ~ "cross"
      )) %>%
      dplyr::mutate(class_connection = as.factor(class_connection)) %>% #and now the model matrix specification
      dplyr::mutate(class_connection_higher = alter_higher_class == su_higher_class & su_higher_class == 1) %>%
      dplyr::mutate(class_connection_lower = alter_higher_class == su_higher_class & su_higher_class == 0) %>%
      dplyr::mutate(class_connection_cross = alter_higher_class != su_higher_class ) %>%
      dplyr::mutate(class_connection_same = alter_higher_class == su_higher_class) %>%
      dplyr::mutate(q1_q1 = su_q1 & alter_q1) %>%
      dplyr::mutate(q1_q2 = su_q1 & alter_q2) %>%
      dplyr::mutate(q2_q1 = su_q2 & alter_q1) %>%
      dplyr::mutate(q1_q3 = su_q1 & alter_q3) %>%
      dplyr::mutate(q3_q1 = su_q3 & alter_q1) %>%
      dplyr::mutate(q1_q4 = su_q1 & alter_q4) %>%
      dplyr::mutate(q4_q1 = su_q4 & alter_q1) %>%
      dplyr::mutate(q2_q2 = su_q2 & alter_q2) %>%
      dplyr::mutate(q2_q3 = su_q2 & alter_q3) %>%
      dplyr::mutate(q3_q2 = su_q3 & alter_q2) %>%
      dplyr::mutate(q2_q4 = su_q2 & alter_q4) %>%
      dplyr::mutate(q4_q2 = su_q4 & alter_q2) %>%
      dplyr::mutate(q3_q3 = su_q3 & alter_q3) %>%
      dplyr::mutate(q3_q4 = su_q3 & alter_q4) %>%
      dplyr::mutate(q4_q3 = su_q4 & alter_q3) %>%
      dplyr::mutate(q4_q4 = su_q4 & alter_q4) %>%
      dplyr::filter(!is.na(wealth_dif)) %>%
      dplyr::filter(!(is.na(alter_num_surveyed) | is.na(su_num_surveyed))) %>%
      dplyr::mutate(binary_weight = as.double(true_weight > 0)) %>%
      dplyr::mutate(site = sites[i])

    data_list[[i]] <- data

  }

  master_edge_data <- do.call("rbind", data_list) %>%
    dplyr::mutate(site_num = as.numeric(as.factor(site))) %>%
    arrange(site_num)

  master_node_data <- do.call("rbind", su_node_wealth_data) %>%
    dplyr::select(su, su_site, su_wealth) %>%
    dplyr::mutate(site_num = as.numeric(as.factor(su_site))) %>%
    arrange(site_num)

  su_match <- master_edge_data %>%
    dplyr::select(su) %>%
    distinct() %>%
    dplyr::mutate(alter = su, su_ID = row_number(), alter_ID = row_number())

  master_edge_data <- master_edge_data %>% left_join(y = su_match %>% dplyr::select(su, su_ID), by ="su") %>%
    left_join(y = su_match %>% dplyr::select(alter,alter_ID), by = "alter")


  #directed in terms of quartile
  X_matrix = as.matrix(master_edge_data %>%
                         dplyr::select(q1_q1, q1_q2, q2_q1, q1_q3, q3_q1, q1_q4, q4_q1, q2_q2,
                                q2_q3, q3_q2, q2_q4, q4_q2, q3_q3, q3_q4, q4_q3, q4_q4) %>%
                         mutate_if(is.logical, as.numeric))

  # instead of X_matrix, compute a single class index 1-16 per row
  class_id <- max.col(X_matrix)  # returns column index of the 1 in each row
  # sanity check: every row should have exactly one 1
  stopifnot(all(rowSums(X_matrix) == 1))

  EC_list_1 <- list()
  EC_list_1[[1]] <- c(1,2,4,6)
  EC_list_1[[2]] <- c(2,1,4,6)
  EC_list_1[[3]] <- c(4,1,2,6)
  EC_list_1[[4]] <- c(6,1,2,4)

  EC_list_2 <- list()
  EC_list_2[[1]] <- c(3,8,9,11)
  EC_list_2[[2]] <- c(8,3,9,11)
  EC_list_2[[3]] <- c(9,3,8,11)
  EC_list_2[[4]] <- c(11,3,8,9)

  EC_list_3 <- list()
  EC_list_3[[1]] <- c(5,10,13,14)
  EC_list_3[[2]] <- c(10,5,13,14)
  EC_list_3[[3]] <- c(13,5,10,14)
  EC_list_3[[4]] <- c(14,13,5,10)

  EC_list_4 <- list()
  EC_list_4[[1]] <- c(7,12,15,16)
  EC_list_4[[2]] <- c(12,7,15,16)
  EC_list_4[[3]] <- c(15,7,12,16)
  EC_list_4[[4]] <- c(16,7,12,15)

  EC_list <- list(EC_list_1, EC_list_2, EC_list_3, EC_list_4)

  master_model_list <- list()


  #right now:
  site_X = matrix(1, nrow = 1, ncol = length(unique(master_edge_data$site_num)))
  #just the intercept!
  #but new site level covariates can be added to this matrix as they become available
  #P is the number of level covariates
  P=1


  #RUN ANALYSIS
  for(h in 1:4){

    temp_list <- list()

    for(j in 1:4){

      stan_data <- list(
        N = nrow(master_edge_data),
        L = length(unique(master_edge_data$site_num)),
        P = P,
        y = master_edge_data$true_weight,
        ll = master_edge_data$site_num,
        alter = master_edge_data$alter_ID,
        su = master_edge_data$su_ID,
        class_id = class_id,
        D = dim(X_matrix)[2],
        EC = EC_list[[h]][[j]],
        Z = master_edge_data$su_num_surveyed,
        W = master_node_data$su_wealth,
        N_W = length(master_node_data$su_wealth), #CHANGE
        ll_W = master_node_data$site_num,
        gini = gini[[gini_col]],
        site_X = site_X
      )

      fit <- mod$sample(
        data = stan_data,       # named list of data
        chains = 4,             # number of Markov chains
        parallel_chains = 4,
        threads_per_chain = 2,
        iter_warmup = 1000,     # number of warmup iterations per chain
        iter_sampling = 1000,   # total number of iterations per chain
        adapt_delta = 0.95,
        max_treedepth = 10,
        refresh = 50,
        output_dir = out_folder
      )

      temp_list[[j]] <- fit

      saveRDS(fit, file.path(out_folder, paste0("fit_h", h, "_j", j, "_", wealth_var, ".rds")))

    }

    master_model_list[[h]] <- temp_list
  }

  master_model_list

  alpha_data_frame <- data.frame()
  k <- 1
  for(g in 1:4){
    for(f in 1:4){
      posterior_samples <- extract_cmdstanr(master_model_list[[g]][[f]])
      alpha_data_frame <- rbind(alpha_data_frame,
                                data.frame(paste0("Own = ",g,", Alter = ",f),
                                           posterior_samples$alpha_1, mean(posterior_samples$alpha_1),
                                           unname(quantile(posterior_samples$alpha_1, 0.025)),
                                           unname(quantile(posterior_samples$alpha_1, 0.975))))
      k <- k + 1
    }
  }


  #SAVE ANALYSIS AND VISUALIZATION

  names(alpha_data_frame) <- c("facet","alpha","mean","lower_quantile","upper_quantile")

  alpha_data_frame <- alpha_data_frame %>%
    dplyr::mutate(Effect = ifelse(mean > 0, "positive", "negative")) %>%
    dplyr::mutate(Significant =
             ifelse((lower_quantile > 0 & upper_quantile > 0)| (lower_quantile < 0 & upper_quantile < 0), "yes", "no" ))

  alpha_data_frame

  facet_order <- with(expand.grid(alter = 1:4, own = 4:1),
                    paste0("Own = ", own, ", Alter = ", alter))

  alpha_ggplot <- alpha_data_frame %>%
    dplyr::mutate(facet = factor(facet, levels = facet_order)) %>%
    ggplot(aes(x = alpha)) +
    geom_histogram(aes(fill = Significant)) +
    geom_vline(aes(xintercept = mean, color = Effect)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(vars(facet), ncol = 4) +
    theme_classic() +
    scale_fill_manual(name = "Significant", values = c("grey", "black")) +
    scale_color_manual(name = "Effect", values = c("negative" = "red", "positive" = "blue")) +
    labs(y = "Count", x = TeX("$\\alpha_1$ \\ Posterior")) +


  saveRDS(alpha_data_frame, paste0(path, "/multilevel_model_data_frame_", wealth_var, ".rds"))

  alpha_ggplot

  ggsave(paste0(EndowDropbox, "/Figures/paper-1st-draft/multilevel/multilevel_model_plot_", wealth_var, ".pdf"), alpha_ggplot, width = 10, height = 7)

}

# Diagnostics
fit_files <- list.files(out_folder, pattern = "^fit_h[0-9]+_j[0-9]+_.*\\.rds$", full.names = TRUE)

diagnostics_list <- list()

for (f in fit_files) {

  fname <- basename(f)

  h_val <- str_extract(fname, "h[0-9]+") %>% str_remove("h") %>% as.integer()
  j_val <- str_extract(fname, "j[0-9]+") %>% str_remove("j") %>% as.integer()
  wealth_var_val <- fname %>%
    str_remove("^fit_h[0-9]+_j[0-9]+_") %>%
    str_remove("\\.rds$")

  fit <- readRDS(f)

  diag <- fit$diagnostic_summary()

  sampler_diag <- fit$sampler_diagnostics()
  iters_per_chain <- dim(sampler_diag)[1]
  n_chains <- dim(sampler_diag)[2]
  total_transitions <- iters_per_chain * n_chains

  time_hrs <- fit$time()$total / (60 * 60)

  summ <- fit$summary(variables = c("alpha_0", "alpha_1", "sigma_alpha"))

  diagnostics_list[[fname]] <- data.frame(
    wealth_var = wealth_var_val,
    h = h_val,
    j = j_val,
    time_hrs = round(time_hrs, 2),
    num_divergent_total = sum(diag$num_divergent),
    pct_divergent = round(100 * sum(diag$num_divergent) / total_transitions, 2),
    num_max_treedepth_total = sum(diag$num_max_treedepth),
    pct_max_treedepth = round(100 * sum(diag$num_max_treedepth) / total_transitions, 2),
    ebfmi_min = round(min(diag$ebfmi), 3),
    alpha_1_mean = round(summ$mean[summ$variable == "alpha_1"], 3),
    alpha_1_rhat = round(summ$rhat[summ$variable == "alpha_1"], 3),
    alpha_1_ess_bulk = round(summ$ess_bulk[summ$variable == "alpha_1"], 0),
    alpha_1_ess_tail = round(summ$ess_tail[summ$variable == "alpha_1"], 0),
    sigma_alpha_rhat = round(summ$rhat[summ$variable == "sigma_alpha"], 3),
    stringsAsFactors = FALSE
  )
}

diagnostics_table <- do.call(rbind, diagnostics_list) %>%
  arrange(wealth_var, h, j)

rownames(diagnostics_table) <- NULL

diagnostics_table

write.csv(diagnostics_table, file.path(out_folder, "multilevel_diagnostics.csv"), row.names = FALSE)