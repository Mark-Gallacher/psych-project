library(tidyverse)
library(pwr)
library(Rmpfr)

options(digits = 7)

sample_size <- c(c(2:19), seq(20, 10000, 10))
effect_size <-  0.2
effect_sizes <-  c(0.1, 0.2, 0.3, 0.4, .5)
alpha <-  0.05
vector_len <- length(sample_size)*length(effect_sizes)
criteff_vector <- vector(length = vector_len)
beta_vector <- vector(length = vector_len)
alpha_vector <- vector(length = vector_len)
effect_vector <- vector(length = vector_len)
#----------------------------------------------------------
# Method 1 - using Base R and pwr package - to prevent R from rounding numbers to 1 or 0

### Min Alpha - Beta Rule - Stop when power is 1 to stop weird lines in graph
for(d in 1:length(effect_sizes)){
  power <- 0.5 # just to reset power, before it check it in the if statement
  effect_size <- effect_sizes[d]
  alpha <- 0.05

  for(n in 1:length(sample_size)){
    # position in vector
    pos <- length(sample_size) * (d - 1) + n
    
    if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
      # beta_vector[pos] <- beta_vector[pos-1]
      # alpha_vector[pos] <- alpha_vector[pos-1]
      # criteff_vector[[pos]] <- criteff_vector[[pos-1]]
      # effect_vector[pos] <- effect_size
      break
    }else{
    # find power
    power <- pwr.t.test(n = sample_size[n], 
                        d = effect_size,
                        sig.level = alpha, 
                        alternative = "two.sided",
                        type = "two.sample")$power
    # find beta
    beta_vector[pos] <- 1 - power
    
    # alpha <- min(alpha, beta_vector[pos])
    # alpha_vector[pos] <- alpha
    # Without Sequential Alpha, the graphs go beyond the effect / 2 (where they are meant to plateau)
    alpha_vector[pos] <- min(alpha, beta_vector[pos])

    effect_vector[pos] <- effect_size
    
    if(round(power, digits = 16) >= 1 | round(alpha, digits = 16) <= 0){
      # beta_vector[pos] <- beta_vector[pos-1]
      # alpha_vector[pos] <- alpha_vector[pos-1]
      # criteff_vector[[pos]] <- criteff_vector[[pos-1]]
      # effect_vector[pos] <- effect_size
      break
    }else{
      criteff_vector[[pos]] <- pwr.t.test(n = sample_size[n], 
                                        power = 0.8, 
                                        sig.level = alpha_vector[pos], 
                                        alternative = "two.sided", 
                                        type = "two.sample")$d
    
    print(paste("N =",sample_size[n],"Alpha =", alpha, "Power = ", power, "d =", effect_sizes[d]))
    }
  }
  }
  }

df <- tibble(
  n = rep(sample_size, times = length(effect_sizes)), 
  crit_eff = criteff_vector, 
  alpha = alpha_vector, 
  beta = beta_vector, 
  effect = effect_vector, 
  type = "min_a_b"
) |> 
  filter(!is.na(alpha), 
           crit_eff > 0)


vector_len <- length(sample_size)*length(effect_sizes)
criteff_vector <- vector(length = vector_len)
beta_vector <- vector(length = vector_len)
alpha_vector <- vector(length = vector_len)
effect_vector <- vector(length = vector_len)

### Beta / 5 rule, stop when power is 1, to stop weird lines on graph
for(d in 1:length(effect_sizes)){
  
  effect_size <- effect_sizes[d]
  alpha <- 0.05
  for(n in 1:length(sample_size)){
    
    if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
      break
    }else{
      # position in vector
      pos <- length(sample_size) * (d - 1) + n
      
      # find power
      power <- pwr.t.test(n = sample_size[n], 
                          d = effect_size,
                          sig.level = alpha, 
                          alternative = "two.sided", 
                          type = "two.sample")$power
      # find beta
      beta_vector[pos] <-  1 - power

      # Without Sequential Alpha, the graphs go beyond the effect / 2 (where they are meant to plateau)
      alpha_vector[pos] <- beta_vector[pos]/5
      # alpha <- beta_vector[pos]/5
      # alpha_vector[pos] <- alpha®
      
      effect_vector[pos] <- effect_size
      
      if(round(power, digits = 21) >= 1 | round(alpha, digits = 21) <= 0){
        break
      }else{
      criteff_vector[[pos]] <- pwr.t.test(n = sample_size[n], 
                                          power = 0.8, 
                                          sig.level = alpha_vector[pos], 
                                          alternative = "two.sided")$d
      
      print(paste("N =",sample_size[n],"Alpha =", alpha))
    }
  }
  }
}

df_2 <- tibble(
  n = rep(sample_size, times = length(effect_sizes)), 
  crit_eff = criteff_vector, 
  alpha = alpha_vector, 
  beta = beta_vector, 
  effect = effect_vector, 
  type = "beta_5"
) |>  
  filter(!is.na(alpha), 
         crit_eff > 0) |> 
  bind_rows(df)


## Plotting Alpha, Beta and Critical Effect Size
## See how the line flattens 
## Only when we use beta > some very small number, does not flatten when we use condition beta > 0, this is probably a slight bug in the code or an error with rounding

df_2|>
  ggplot(aes(x = n, y = crit_eff, group = effect, colour = effect))+
  # geom_point(alpha = 0.4, size = .5)+
  geom_line(size = 1)+
  geom_hline(aes(colour = effect, yintercept = effect/2))+
  scale_x_log10()+
  scale_y_continuous(limits = c(0, 2))+
  facet_grid(cols = vars(type))

# df_2|>
#   ggplot(aes(x = n, y = beta, group = effect, colour = effect))+
#   geom_point(alpha = 0.4)+
#   geom_line()+
#   scale_x_log10()+
#   facet_grid(cols = vars(type))
# 
# df_2|>
#   ggplot(aes(x = n, y = alpha, group = effect, colour = effect))+
#   geom_point(alpha = 0.4)+
#   geom_line()+
#   scale_x_log10()+
#   facet_grid(cols = vars(type))

#----------------------------------------------------------
# Method 2 - using Rmpfr package to increase precision - to prevent R from rounding numbers to 1 or 0

library(tidyverse)
library(pwr)
library(Rmpfr)
library(parallel)
library(doParallel)

doParallel::registerDoParallel(cores = parallel::detectCores() - 2)

options(digits = 7)

precison <- 200

sample_size <- seq(5, 10000, 5)
effect_sizes <-  seq(0.05, 0.5, 0.05)
# effect_sizes <-  .4

ss_long <- rep(sample_size, times = length(effect_sizes))
es_long <- rep(effect_sizes, each = length(sample_size))

alpha <-  0.05

matrix <- mpfrArray(NA, precBits = precison, dim = c(length(ss_long), 6))
# criteff_vector <- vector(length = vector_len)
# beta_vector <- vector(length = vector_len)
# alpha_vector <- vector(length = vector_len)
# effect_vector <- vector(length = vector_len)
# col 1 - sample size
# col 2 - alpha
# col 3 - beta
# col 4 - power
# col 5 - effect_size (predicted)
# col 6 - critical effect size

## find power for various sample sizes
power <- Rmpfr::mpfr( 
  pwr::pwr.t.test(
    n = ss_long, 
    d = es_long, 
    sig.level = alpha, 
    type = "two.sample", 
    alternative = "two.sided")$power, 
  precBits = precison
)
# some powers are greater than one or equal to one (R is the rounding/ We need beta to be greater than zero), find closest value to one then replace them with it
largest_power <- sort(power[power > 1-1e-14 & power<1], decreasing = T)[1]

power[power>=1] <- largest_power

# find beta 
beta <- 1 - power

# create alpha vector
alpha_vec <- mpfrArray(alpha, dim = length(beta), precBits = precison)
# input beta when beta is smaller than alpha
alpha_vec[beta < alpha] <- beta[beta < alpha] 
matrix[,6] <- foreach::foreach(n = 1:length(ss_long), .combine = c) %dopar% {
  
  Rmpfr::mpfr( 
    pwr::pwr.t.test(
      n = ss_long[[n]], 
      power = 0.95, 
      sig.level = as.numeric(alpha_vec)[[n]], 
      type = "two.sample", 
      alternative = "two.sided")$d, 
    precBits = precison
  )
}

## finalise matrix
# matrix[,1] <- ss_long
# matrix[,2] <- alpha_vec
# matrix[,3] <- beta
# matrix[,4] <- power
# matrix[,5] <- es_long

dyn_df <- tibble(
  n = ss_long, 
  alpha = as.numeric(alpha_vec),
  beta = as.numeric(beta),
  power = as.numeric(power),
  eff_size = as.numeric(es_long),
  crit_eff = as.numeric(matrix[,6])
) # crit effect

dyn_crit1 <- dyn_df |> 
  ggplot(aes(x = n, y = crit_eff, group = eff_size, colour = eff_size))+
  geom_point(alpha = .2)+
  geom_line(alpha = .2)+
  scale_x_log10()

dyn_crit2 <- dyn_df |> 
  filter(!alpha < 1e-10) |> 
  ggplot(aes(x = n, y = crit_eff, group = eff_size, colour = eff_size))+
  geom_point(alpha = .2)+
  geom_line(alpha = .2)+
  scale_x_log10()

ggsave(filename = here::here("images", "dyn_crit1.png"), plot = dyn_crit1, width = 10, height = 8, dpi = 360) 
ggsave(filename = here::here("images", "dyn_crit2.png"), plot = dyn_crit2, width = 10, height = 8, dpi = 360) 

# # beta
# dyn_df |>
#   ggplot(aes(x = n, y = beta, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
# # alpha
# dyn_df |>
#   ggplot(aes(x = n, y = alpha, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
# # # power
# dyn_df |> 
#   ggplot(aes(x = n, y = power, colour = eff_size))+
#   geom_point()+
#   scale_x_log10()
#   scale_x_log10()