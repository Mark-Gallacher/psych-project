library(tidyverse)
library(doParallel)
library(NetworkToolbox)
library(parallel)
library(pwr)

source(file = here::here("my-scripts/R/project_themes.R"))

cores <- parallel::detectCores() - 2

doParallel::registerDoParallel(cores = cores)


sample_size <- seq(10, 10000, 5)
effect_sizes <-  seq(0.1, .5, 0.1)

long_ss <- rep(sample_size, times = length(effect_sizes))
long_es <- rep(effect_sizes, each = length(sample_size))

adapt_a <- c()

adapt_a <- foreach::foreach(i = 1:length(long_es), .combine = "c")%dopar%{
  
  NetworkToolbox::adapt.a(test = "two.sample",
                          n = long_ss[[i]], 
                          alpha = 0.05, 
                          power = 0.8, 
                          efxize = long_es[[i]])$adapt.a

}

df <- tibble(
  sample_size = long_ss,
  eff_size = long_es,
  alpha = adapt_a
)

df |> 
  ggplot(aes(sample_size, alpha, colour = as.factor(eff_size), group = eff_size))+
  geom_point(alpha = .5, size = 1)+
  geom_line(linewidth = 1, alpha = .5)+
  geom_hline(yintercept = 0, linewidth = 1)+
  geom_hline(yintercept = 0.05, linewidth = 1, colour = "azure4")+
  scale_x_log10("Sample Size")+
  scale_y_continuous("Alpha", limits = c(0, .5))+
  theme_project_light(base_size = 16)+ 
  scale_colour_manual("Predicted Effect Size",
                     values = MetBrewer::met.brewer(name = "Troy", n = 5),
                     guide = guide_legend(title.vjust = .5,
                                          override.aes = list(linewidth = 3,
                                                              size = 0, alpha = 1, shape = 0, fill = NA)))+
  labs(title = "New Alpha Levels from Adaptive Alpha Rule")+
  annotate("text", x = 5000, y = 0.07, label = "\u03B1 = 0.05", vjust = 0, size = 10, colour = "azure4")


ggsave(filename = "adapt_alpha.png", 
       path = here::here("images/ch1"), dpi = 320, width = 10, height = 8)

