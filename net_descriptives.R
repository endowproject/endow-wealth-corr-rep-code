## Network Descriptive Statistics
##
## Generates summary statistics of social support nominations and networks
##
## Outputs:
##
## eg net


library(dplyr)
library(ggplot2)
library(plm)
library(tidyverse)
library(gridExtra)
library(igraph)
library(patchwork)
library(tidygraph)
library(ggraph)
library(ggforce)

# EndowDerivedData is defined in .Rprofile on Windows; on Mac we fall back to
# the DerivedData subfolder of this repository.
if (!exists("EndowDerivedData")) {
  EndowDerivedData <- file.path(EndowGitHub, "DerivedData")
}

path <- EndowDerivedData
figpath <- file.path(EndowDropbox, "Figures", "DescriptiveStats")
tablepath <- file.path(EndowDropbox, "Tables", "DescriptiveStats")

load(file.path(path,"su_nets_expanded.rdata"))


# ── Network Plots ────────────────────────────────────────────────
plot_net <- function(
  net,
  title,
  layout_style = "fr",
  edge_weight = FALSE,
  node_size_attr = NULL
) {
  if (ecount(net) == 0) edge_weight <- FALSE

  edge_width <- if (edge_weight) aes(width = weight) else aes()
  arr        <- if (is_directed(net)) arrow(length = unit(0.15, "cm")) else NULL
  graph      <- tidygraph::as_tbl_graph(net)

  if (!is.null(node_size_attr)) {
    vals <- igraph::vertex_attr(graph, node_size_attr)
    was_missing <- is.na(vals)
    if (any(was_missing)) {
      vals[was_missing] <- median(vals, na.rm = TRUE)
    }
    graph <- graph %>%
      tidygraph::activate(nodes) %>%
      tidygraph::mutate(
        !!node_size_attr := vals,
        was_missing       = was_missing
      )
  }

  p <- ggraph(graph, layout = layout_style) +
    geom_edge_loop(
      aes(span = 50, direction = 45, strength = 0.5),
      colour      = adjustcolor("black", alpha.f = 0.25),
      show.legend = FALSE
    ) +
    geom_edge_fan(
      edge_width,
      colour      = adjustcolor("black", alpha.f = 0.25),
      arrow       = arr,
      end_cap     = circle(1, "mm"),
      show.legend = FALSE
    ) +
    scale_edge_width_continuous(range = c(0.3, 2))

  p <- if (!is.null(node_size_attr)) {
    p +
      geom_node_point(aes(size = .data[[node_size_attr]], colour = was_missing),
                       show.legend = FALSE) +
      scale_size_continuous(range = c(0.5, 5), guide = "none") +
      scale_colour_manual(values = c(`FALSE` = "royalblue", `TRUE` = "grey"))
  } else {
    p + geom_node_point(size = 1.5, colour = "royalblue")
  }

  p +
    ggtitle(paste(strwrap(title, width = 20), collapse = "\n")) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(size = 9, hjust = 0.5)
    )
}

blank_plot <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = paste(strwrap(label, width = 20), collapse = "\n"),
             colour = "grey50", size = 3) +
    theme_void()
}

pad_to_full <- function(page, n) {
  while (length(page) < n) page <- c(page, list(ggplot() + theme_void()))
  page
}

write_net_pdf <- function(plot_list, path, plots_per_page, grid_rows, grid_cols, width, height) {
  pdf(path, width = width, height = height)
  for (page in split(plot_list, ceiling(seq_along(plot_list) / plots_per_page))) {
    print(wrap_plots(pad_to_full(page, plots_per_page), nrow = grid_rows, ncol = grid_cols))
  }
  dev.off()
}


# ---- Individual agg nets as examples ----
eg_sites <- c("EG", "EX", "SI")
eg_agg_nets_wealth <- lapply(eg_sites, function(site) {
    net <- su_nets_expanded[[site]]$sum
      plot_net(net, site, edge_weight = TRUE, node_size_attr = "su_wealth")
  })

write_net_pdf(
  eg_agg_nets_wealth,
  path           = file.path(EndowDropbox, "Figures", "Networks",
                              "eg_agg_nets_wealth.pdf"),
  plots_per_page = 3,
  grid_rows      = 1,
  grid_cols      = 3,
  width          = 10,
  height         = 5
)