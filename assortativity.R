library(cNORM)
library(ggplot2)

path <- file.path(EndowGitHub, "DerivedData")
load(file.path(path,"su_nets_expanded.rdata"))

sysfonts::font_add_google("Roboto Mono", "roboto", regular.wt = 400)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

assorts <- data.frame(
  site = names(su_nets_expanded),
  wealth_assort = NA,
  wealth_rank_assort = NA,
  wealth_pc_assort = NA,
  wealth_pc_rank_assort = NA
)

count <- 1
for (site in names(su_nets_expanded)) {
  graph <- su_nets_expanded[[site]][["sum"]]
  ## drop cases with missing values, as we do for EC measures
  graph <- delete_vertices(graph, V(graph)[is.na(V(graph)$su_wealth)])
  graph <- delete_vertices(graph, V(graph)[is.na(V(graph)$susize)])
  wealths <- V(graph)$su_wealth
  sizes <- V(graph)$susize
  wealth_pcs <- wealths / sizes
  wealth_ranks <- dplyr::percent_rank(wealths)
  wealth_pc_ranks <- dplyr::percent_rank(wealth_pcs)
  assorts$wealth_assort[count] <- igraph::assortativity(
    graph,
    values = wealths,
    normalized = TRUE,
    directed = TRUE
  )
  assorts$wealth_rank_assort[count] <- igraph::assortativity(
    graph,
    values = wealth_ranks,
    normalized = TRUE,
    directed = TRUE
  )
  assorts$wealth_pc_assort[count] <- igraph::assortativity(
    graph,
    values = wealth_pcs,
    normalized = TRUE,
    directed = TRUE
  )
  assorts$wealth_pc_rank_assort[count] <- igraph::assortativity(
    graph,
    values = wealth_pc_ranks,
    normalized = TRUE,
    directed = TRUE
  )

  count <- count + 1
}

dat <-
  read.csv(file.path(path, "DataForLASSO.csv"))

dat$RC_capita.sum = dat$EC_capita.sum / dat$EC_capita_high.sum

dat$RC_capita.rev_sum = dat$EC_capita.rev_sum / dat$EC_capita_high.rev_sum

dat$rel_frank_capita <-
  dat$avg_frank_low_capita.sum / dat$avg_frank_high_capita.sum

dat$rel_frank_capita_rev <-
  dat$avg_frank_low_capita.rev_sum / dat$avg_frank_high_capita.rev_sum

dat$rel_median_fwealth_absolute <-
  dat$median_fwealth_low_absolute.sum / dat$median_fwealth_high_absolute.sum

dat$rel_median_fwealth_absolute_rev <-
  dat$median_fwealth_low_absolute.rev_sum / dat$median_fwealth_high_absolute.rev_sum

dat <- merge(dat, assorts, by.x = "site_name", by.y = "site", all.x = TRUE, all.y = TRUE)

format_pval <- function(p, digits = 4) {
  stopifnot(is.numeric(p), p >= 0, p <= 1)
  if (p < 0.0001) {
    "p < 0.0001"
  } else {
    paste0("p = ", formatC(p, digits = digits, format = "f"))
  }
}

weighted_correlation <-
  cor.test(dat$rel_frank_capita, dat$wealth_pc_rank_assort)

p <- ggplot(dat, aes(x = rel_frank_capita, y = wealth_pc_rank_assort)) +
  #geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Relative Average Alter Wealth",
    y = "Assortativity of Percentile Rank Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "RAAW_vs_AssortWPCRank.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)

weighted_correlation <-
  cor.test(dat$rel_frank_capita, dat$wealth_pc_assort)

p <- ggplot(dat, aes(x = rel_frank_capita, y = wealth_pc_assort)) +
  #geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Relative Average Alter Wealth",
    y = "Assortativity of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()

ggsave(file.path(EndowDropbox, "Figures", "paper-1st-draft", "cross-site", "RAAW_vs_AssortWPC.pdf"),
       plot = p,
       units = "in",
       width = 5, height = 5)


weighted_correlation <-
  cor.test(dat$avg_frank_high_capita.sum, dat$wealth_pc_assort)

p <- ggplot(dat, aes(x = avg_frank_high_capita.sum, y = wealth_pc_assort)) +
  #geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #ggrepel::geom_text_repel(
  #  aes(label = site_name),
  #  size = 3,
  #  max.overlaps = 10
  #) +
  geom_label(
    aes(label = site_name),
    family = "roboto",
    size = 1.5,
    alpha = 0.5,
    linewidth = 0.25,
    label.padding = unit(0.3, "lines"),
    label.r = unit(0.6, "lines")
  ) +
  labs(
    subtitle = paste0(
      "Correlation: ",
      sprintf("%.2f", weighted_correlation$estimate),
      ", ",
      format_pval(weighted_correlation$p.value)
    ),
    x = "Average Alter Wealth of the Rich",
    y = "Assortativity of Wealth per Capita"
  ) +
  guides(size = "none") +
  # xlim(0.25, 1.25) +
  # ylim(0.2, 0.7) +
  theme_classic()