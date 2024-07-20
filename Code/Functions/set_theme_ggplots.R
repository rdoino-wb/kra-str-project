# This is the standard theme we will apply to all graphs
theme_aeqj <- function() {
  theme_minimal(base_family = "LM Roman 10", base_size = 15) +
    theme(
      
      # Text Size
      text = element_text(size = 20, colour = "black"),
      axis.title = element_text(size = 14, colour = "black"), 
      plot.title = element_text(size = 14, colour = "black", face = "italic"), 
      plot.title.position = 'plot',
      
      # Grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      # Axis lines
      axis.line = element_line(color = "black", size = 0.4),
      axis.ticks = element_line(color = "black", size = 0.4),
      
      # Legend
      legend.position = "none",
      
      # Background
      panel.background = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      
      # Margins
      plot.margin = margin(25, 25, 10, 25)
    )
  
}