library(tidyverse)
library(glue)

n  <- c(n = seq(5,5000,5))
a <- c(alpha = c(0.05, 0.01, 0.005, 0.001, 0.0005))





t <- expand_grid(n, a)


t |> mutate(!!glue("p_value", 1,.sep = "_") := map2(
      .x = n, .y = a, 
      .f = ~ t.test(rnorm(.x),rnorm(.x), 
                  alternative = "two.sided", 
                  conf.level = 1 - .y, 
                  var.equal = TRUE
                  )[[3]]
                 )
      )





df <-  map2_dbl(.x = t$n, .y = t$a, 
                .f = ~ t.test(rnorm(.x),rnorm(.x), 
                              alternative = "two.sided", 
                              conf.level = 1 - .y, 
                              var.equal = TRUE)[["p.value"]]
                ) |> as_tibble()



