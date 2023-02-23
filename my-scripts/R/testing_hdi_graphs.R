library(tidyverse)

hdi_of_icdf <- function(name, width = .95, tol = 1e-8, ... ) {
  
  incredible_mass <-  1.0 - width
  interval_width  <- function(low_tail_prob, name, width, ...) {
    name(width + low_tail_prob, ...) - name(low_tail_prob, ...)
  }
  opt_info <- optimize(interval_width, c(0, incredible_mass), 
                       name = name, width = width, 
                       tol = tol, ...)
  hdi_lower_tail_prob <- opt_info$minimum
  
  return(c(name(hdi_lower_tail_prob, ...),
           name(width + hdi_lower_tail_prob, ...)))
  
}
interval_width  <- function(low_tail_prob, name, width, ...) {
  name(width + low_tail_prob, ...) - name(low_tail_prob, ...)
}
width = .95
tol = 1e-8
incredible_mass <-  1.0 - width

opt_info <- optimize(interval_width,         ## function we want to optimise
                     c(0, incredible_mass),  ## interval we want to look between
                     name = qbeta,           ## name of distribution
                     width = width,          ## Arguments for the first function, and for distribution 
                     tol = tol, 
                     shape1 = 11, 
                     shape2 = 4)




h <- hdi_of_icdf(name = qbeta,
                shape1 = 1,
                shape2 = 1)

h <- hdi_of_icdf(name = qnorm,
                mean = .5,
                sd = .1)

tibble(x = seq(from = 0, to = 1, by = .01)) %>% 

ggplot(aes(x = x))+
  geom_ribbon(aes(ymin = 0, ymax = dnorm(x = x, mean = .5, sd = .1)), 
              fill = "grey75")+
  geom_ribbon(data = . %>% filter(x >= h[1] & x <= h[2]), 
              aes(ymin = 0, ymax = dnorm(x = x, mean = .5, sd = .1)), 
              fill = "grey50")+
  geom_line(data = tibble(x = c(h[1] + .007, h[2] - .007), 
                          y = c(.5, .5)), 
            aes(x = x , y = y), 
            arrow = arrow(length = unit(.2, "cm"), 
                          ends = "both", 
                          type = "closed"), 
            colour = "grey92")+
  annotate("text", x = .5, y = .6, 
           label = "95% HDI", color = "grey92", size = 5)
  
h1 <- hdi_of_icdf(name = qbeta,
                 shape1 = 11,
                 shape2 = 4)

 x = seq(from = 0, to = 1, by = .01) 
tibble(x = seq(from = 0, to = 1, by = .01)) %>% 
  ggplot(aes(x = x))+
  geom_ribbon(aes(ymin = 0, ymax = dbeta(x = x, shape1 = 11, shape2 = 4)), 
              fill = "grey75", alpha = .5)+
  geom_ribbon(data = . %>% filter(x >= h1[1] & x <= h1[2]), 
              aes(ymin = 0, ymax = dbeta(x = x, shape1 = 11, shape2 = 4)), 
              fill = "grey50")+
  geom_line(data = tibble(x = c(h1[1] + .007, h1[2] - .007), 
                          y = c(.5, .5)), 
            aes(x = x , y = y), 
            arrow = arrow(length = unit(.2, "cm"), 
                          ends = "both", 
                          type = "closed"), 
            colour = "grey92")+
  annotate("text", x = .75, y = .6, 
           label = "95% HDI", color = "grey92", size = 5)

  