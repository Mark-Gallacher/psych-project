library(tidyverse)
library(ggtext)

theme_project_light <- function(base_size = 10){
  
  # font <- "TT Arial"   #assign font family up front
  
  theme_minimal(base_size = base_size)  %+replace%
        theme(# Bold, bigger title
              plot.title = element_markdown(face = "bold", size = rel(1.5), hjust = 0, lineheight = 1, vjust = 1),
              # Plain, slightly bigger subtitle that is grey
              plot.subtitle = element_markdown(face = "plain", size = rel(1.1), color = "grey30", hjust = 0, lineheight = 1.2, vjust = 0),
              # Italic, smaller, grey caption that is left-aligned
              plot.caption = element_text(face = "italic", size = rel(0.7), color = "grey60", hjust = 0),
              # Bold legend titles
              legend.title = element_text(face = "bold", size = rel(1.3)),
              
              legend.text = element_text(size = rel(1.2)),
              # legend underneath title
              legend.position = "top",
              # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
              strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
              # Bold axis titles
              axis.title = element_text(face = "bold"),
              # Add some space above the x-axis title and make it left-aligned
              axis.title.x = element_text(size = rel(1.3), margin = margin(t = 10), hjust = 0.5),
              # Add some space to the right of the y-axis title and make it top-aligned
              axis.title.y = element_text(size = rel(1.3), margin = margin(r = 10), hjust = 0.5, angle = 90),
              
              axis.text = element_text(size = rel(1.2)),
              
              plot.background = element_rect(fill = "#FFFFFF", colour = NA, linewidth = 0), 
              
              panel.grid.minor.x = element_blank(),
              
              panel.grid.minor.y = element_blank(),
              
              panel.grid.major.y = element_blank(),
              
              panel.border = element_blank()
      
              )
      
}



