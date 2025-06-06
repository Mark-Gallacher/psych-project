##### Graphs for Equivalence ####
# Plotting Mean and Confidence intervals and their proximity to the equivalence region, 
# or ROPE, and the subsequent interpretation of them
# 
# Following Lakens and Kruschke's recommendations:
#  - overlapping with the ROPE = inconclusive
#  - completely inside the ROPE = equivalence to NULL
#  - completely outside the ROPE = rejection of the NULL 

library(tidyverse)

df <- tibble(
  mean = c(0.02, 0.18, 0.5, 1), 
  y = c(1:length(mean)),
  u_ci = mean + c(.2, .04, .4, .3), 
  l_ci = mean - c(.2, .04, .4, .3) 
)

rope_margin = .25

df |> 
  ggplot(aes(x = mean, y = y))+
  geom_point(size = 4)+
  geom_errorbar(aes(xmin = l_ci, xmax = u_ci), linewidth = 2, width = .15)+
  scale_x_continuous("", breaks = NULL, limits = c(-.5, 1.5))+
  scale_y_continuous("", breaks = NULL)+
  theme_project_light(base_size = 15)+
  geom_segment(aes(x = 0, 
                   xend = 0, 
                   y = 0.6, 
                   yend = 1.1*length(mean)), 
               colour = "grey30", 
               linewidth = 1.5)+
  annotate(geom = "text", 
           x = -0.5, 
           y = df$y, 
           label = rev(c("A", "B", "C", "D")),
           hjust = 0.5, 
           size = 10)+
  annotate(geom = "text", 
           x = c(0,-rope_margin, rope_margin, 0), 
           y = c(4.5, 4.5, 4.5, 0.5), 
           label = c(0,"-m", "+m", "Equivalence Region"),
           hjust = 0.5, 
           size = 10, 
           colour = c("grey30","grey30","grey30", "#000000"), 
           fontface = c('bold', 'italic', 'italic', 'bold'))+
  annotate(geom = "rect", 
           xmin = -rope_margin, 
           xmax = rope_margin, 
           ymin = 0.6, 
           ymax = 1.1*length(df$mean), 
           fill = "grey60", 
           alpha = .3)+
  labs(title = "Using an Equivalence Region to Redefine Significance")

ggsave(filename = here::here("images/ch1", "equivalence.png"), dpi = 360, width = 10, height = 8)


