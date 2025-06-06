library(tidyverse)

source(here::here("my-scripts/R/project_themes.R"))

ggplot(data.frame(x = c(-4, 4)), 
       aes(x = x))+
  stat_function(fun = dt, args = list(df = 30), 
                linewidth = 1.2, colour = "slateblue4")+
  stat_function(fun = dt, args = list(df = 30), 
                xlim = c(qt(p = 0.975, df = 30, lower.tail = T), 4), 
                geom = "area", fill = "slateblue4")+
  scale_y_continuous("")+
  theme_project_light()+
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    )



ggplot(data.frame(x = c(-6, 8)), 
       aes(x = x))+
  stat_function(fun = dnorm, args = list(mean = .5, sd = 2), fill = "cyan4", geom = "area", alpha = .5)+
  stat_function(fun = dnorm, args = list(mean = 4, sd = 1.5), fill = "darkblue", geom = "area", alpha = .5)+
  stat_function(fun = dnorm, args = list(mean = 2, sd = 1.8),  fill = "slateblue2", geom = "area", alpha = .8)+
  scale_y_continuous("")+
  scale_x_continuous("\u03B8")+
  annotate("text", x = -1.2, y = .2, label = "Prior", fontface = "bold", size = 6, colour = "cyan4", alpha = .8)+
  annotate("text", x = 5.6, y = .25, label = "Data", fontface = "bold", size = 6, colour = "darkblue", alpha = .7)+
  annotate("text", x = 1.8, y = .24, label = "Posterior", fontface = "bold", size = 6, colour = "slateblue2")+
  theme_project_light(base_size = 14)+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_text(size = 20)
  )+
  labs(title = "Updating Prior with Data using Bayes' Rule")

ggsave(file = here::here("images/ch1/bayes_update.png"), height = 2880, width = 3600, dpi = 360, units = "px")

