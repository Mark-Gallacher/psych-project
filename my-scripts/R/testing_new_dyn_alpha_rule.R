library(tidyverse)
library(pwr)
# 
# # Sample Size
# sample_size <- c(5, seq(1, 9.5, 0.5) %o% 10^(1:3), 10^4)
# l_sample <- length(sample_size)
# 
# # Effect Size (Cohen's d) - One default value, and One list
# cohens_d <- 0.5
# many_cohens_d <- seq(0.1, 1, 0.1)
# l_many_cohen <- length(many_cohens_d)
# 
# # Power - One default value, and One list
# power <- 0.9
# many_power <- seq(0.5, 0.90, 0.1)
# l_many_power <- length(many_power)
# 
# # Alpha - One default value, and One list
# alpha <- 0.05
# many_alpha <- c(1 %o% 10^(-1:-6))
# l_many_alpha <- length(many_alpha)
# 
# power_array <- matrix(NA, nrow = l_sample, ncol = l_many_cohen)
# dyn_alpha <- matrix(NA, nrow = l_sample, ncol = l_many_cohen)
# 
# for(n in 1:l_sample){
#   for(d in 1:l_many_cohen){
#     power_array[n, d] <- pwr.t.test(sig.level = alpha, 
#                                     d = many_cohens_d[d], 
#                                     n = sample_size[n], 
#                                     power = NULL, 
#                                     type="paired",
#                                     alternative="two.sided")$power
#     
#     beta_value <- 1 - power_array[n,d]
#     
#     if(beta_value < 1 & beta_value > 0){
#       dyn_alpha[n,d] <- min(alpha, beta_value)
#     } else {
#       dyn_alpha[n,d] = 0
#     }
#   }
# }
# 
# dyn_alpha_df <- tidy_output_with_n(
#   array = dyn_alpha,
#   col_vec = many_cohens_d,
#   row_vec = sample_size, 
#   value_colname = "alpha",
#   name_colname = "effect_size"
# ) 
# 
# d <- dyn_alpha_df|> 
#   rowwise() |> 
#   mutate(test = power.t.test(n = n, sig.level = alpha, power = 1 - alpha, alternative = "two.sided", sd = 1)$delta)
# 
# 
# 
# knitr::kable(x = head(dyn_alpha_df, n = 10))
# 
# 
# 
# d |> 
#   ggplot(aes(x = n, y = test, colour = effect_size))+
#   geom_line(alpha = 0.5, size = 1.5)+
#   geom_point(alpha = 0.5, size = 1.5)+
#   scale_x_log10(name = "Sample Size")+
#   scale_y_continuous(name = "Effect Size")+
#   guides(colour = guide_legend(title = "Power"))+
#   ggtitle("Effect Size Using Dynamic Alpha Levels - min(alpha, beta)")+
#   theme_minimal()



sample_size <- seq(5, 10000, 5)
effect_size <-  0.4
alpha <-  0.05
criteff_vector <- vector()
beta_vector <- vector()
alpha_vector <- vector()

for(n in 1:length(sample_size)){
  # find power
  power <- power.t.test(n = sample_size[n], delta = effect_size, sd = 1, sig.level = alpha, alternative = "two.sided")$power
  # find beta
  beta_vector[n] <-  1 - power

  # compare to alpha, pick smallest
  if (!(beta_vector[n] > 0.0000000001)){
    
    alpha_vector[n] <- 0
    beta_vector[n] <- 0
    
    criteff_vector[n] <- criteff_vector[[n-1]] 
    
  } else{
    alpha_vector[n] <- min(alpha, beta_vector[n])
    
    criteff_vector[[n]] <- power.t.test(n = sample_size[n], power = 0.9, sd = 1, sig.level = alpha_vector[n], alternative = "two.sided")$delta
  }
}

df <- tibble(
  n = sample_size, 
  crit_eff = criteff_vector, 
  alpha = alpha_vector, 
  beta = beta_vector
) 

## Plotting Alpha, Beta and Critical Effect Size
## See how the line flattens 
## Only when we use beta > some very small number, does not flatten when we use condition beta > 0, this is probably a slight bug in the code or an error with rounding

df|> 
  ggplot(aes(x = n, y = crit_eff))+
  geom_point(colour = "blue")+
  geom_point(aes(y = beta), alpha = .2)+
  geom_point(aes(y = alpha), alpha = .2)+
  geom_line(aes(y = beta), size = .7)+
  geom_line(aes(y = alpha), size = .7)+
  # geom_hline(yintercept = 0)+
  geom_hline(yintercept = effect_size/2)+
  scale_x_log10()

















