###################################################################
## Default minimal theme for the paper
##################################################################

paper_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n = 9)
  color_background <- palette[1]
  color_grid.major <- palette[3]
  color_axis.text <- palette[6]
  color_axis.title <- palette[7]
  color_title <- palette[9]

  # Begin construction of chart
  theme_bw(base_size = 16) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = color_background, color = color_background)) +
    theme(plot.background = element_rect(fill = color_background, color = color_background)) +
    theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.25)) +

    # Format the grid
    theme(panel.grid.major = element_line(color = color_grid.major, size = 0.25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill = color_background)) +
    theme(legend.text = element_text(size = 7, color = color_axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color_title, size = 14, vjust = 1.25)) +
    theme(plot.subtitle = element_text(size = 10, color = rgb(0.3, 0.3, 0.3))) +
    theme(axis.text.x = element_text(size = 10, color = color_axis.text)) +
    theme(axis.text.y = element_text(size = 10, color = color_axis.text)) +
    theme(axis.title.x = element_text(size = 11, color = color_axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 11, color = color_axis.title, vjust = 1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
