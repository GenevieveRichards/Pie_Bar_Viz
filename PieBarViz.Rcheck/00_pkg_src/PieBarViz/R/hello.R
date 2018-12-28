# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

pieBar <- function(Dataset, Stacked = FALSE) {
  PieChart <-
    dplyr::filter(Dataset, (Dataset[, 2] == max(Dataset[, 2])))
  PieChart <-
    rbind(PieChart, c("Other", (100 - max(
      Dataset$Percentage
    ))))
  PieChart

  g1 <-
    ggplot(data = PieChart, aes(x = "", y = PieChart[, 2], fill = PieChart[, 1])) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) + scale_colour_brewer(palette = "Set3")



  barChart <-
    dplyr::filter(Dataset, Dataset[, 2] != max(Dataset[, 2]))

  if (Stacked) {
    g2 <-
      ggplot (data = barChart, aes (x = "", y = barChart[, 2], fill = barChart[, 1])) +
      geom_bar(stat = "identity") + ggtitle ("Others") +
      scale_fill_manual(
        values = c(
          "Black",
          "Grey",
          "Red",
          "Blue",
          "Green",
          "Brown",
          "Purple",
          "Light Grey",
          "Light Green",
          "Violet",
          "Turquoise",
          "Orange",
          "Yellow"
        )
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 5),
        axis.ticks = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
      ) + coord_cartesian(ylim = c(0, 15))

  } else {g2 <-
    ggplot (data = barChart, aes (x = barChart[, 1], y = barChart[, 2], fill = barChart[, 1])) +
    geom_bar(stat = "identity",
             position = "dodge") + ggtitle ("Others") +
    scale_fill_manual(
      values = c(
        "Black",
        "Grey",
        "Red",
        "Blue",
        "Green",
        "Brown",
        "Purple",
        "Light Grey",
        "Light Green",
        "Violet",
        "Turquoise",
        "Orange",
        "Yellow"
      )
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8),
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1, size = 5),
      axis.ticks = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) + coord_cartesian(ylim = c(0, 15))
  }
  p <-
    plot_grid(
      plot_grid(NULL, g2, rel_widths = c(.4, .6)),
      plot_grid(g1, NULL, rel_widths = c(.8, .2)) ,
      rel_heights = c(.3, .7),
      ncol = 1
    )

  return(p)

}
