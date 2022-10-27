library(tidyverse)

source(file = here::here("my-scripts", "better-t-test-loop.R"))

alphas = c(0.05, 0.01, 0.005, 0.001, 0.0005)
l_a = length(alphas) 

n = seq(5, 5000, length.out = 1000)
l_n = length(n)
p_values <- array(NA, dim = l_n)

repeats = 1000
dat = list()


for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, break_loop = TRUE, alpha = alphas[i])) |> 
    as_tibble(.name_repair = ~ str_c("mean_diff_", 1:repeats)) |> 
    mutate(alpha = alphas[i])
  
}

seq_t_test_many_alphas_mean <- dat |>
  bind_rows() |> 
  mutate(n = as.factor(rep(n*2, times = l_a))) |>
  pivot_longer(cols = c(-n, -alpha),
               values_to = "diff",
               names_to = "set") |> 
  select(-set) |> 
  na.omit() |>
  group_by(n, alpha)  |>
  mutate(mean = mean(abs(diff)), 
         sd = sd (abs(diff)))


write_csv(x= seq_t_test_many_alphas_mean, file = here::here("sim_data/seq_t_test_many_alphas_mean.csv"))


