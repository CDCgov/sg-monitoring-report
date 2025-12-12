generate_performance_tile_plot <- function(final_table, y, ylab) {

  plot <- ggplot2::ggplot(data = final_table,
                          ggplot2::aes(fill = value, x = indicator, y = stringr::str_to_title(!!dplyr::sym(y)))) +
    geom_tile(color = "white",
              lwd = 0.8,
              linetype = 1) +
    ggplot2::scale_fill_manual(
      values = c(
        "On target" = "#0070c0",
        "Below target" = "darkorange",
        "To Be Determined" = "lightgrey"),
      name = "Indicator Performance",
      na.value = "lightgrey"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1)
    )

  return(plot)
}

