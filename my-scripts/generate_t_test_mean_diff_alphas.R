library(tidyverse)

source(file = here::here("my-scripts", "t_test_loop_functions.R"))

alphas = c(0.05, 0.01, 0.005, 0.001, 0.0005)
l_a = length(alphas) 

n = seq(5, 5000, length.out = 1000)
l_n = length(n)

repeats = 1000
dat = list()


for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, break_loop = TRUE, alpha = alphas[i])) |> 
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(alpha = as.factor(alphas[i]))
  
}

seq_t_test_many_alphas_mean_break <- dat |>
  bind_rows() |> 
  mutate(n = as.factor(rep(n*2, times = repeats * l_a))) |> 
  na.omit() |> 
  group_by(n, alpha) |> 
  mutate(across(.cols = where(is.numeric), 
                .fns = list(mean = ~ mean(.), sd = ~ sd(.)),
                .names = "{col}_{fn}"))

write_csv(x= seq_t_test_many_alphas_mean_break, file = here::here("sim_data/seq_t_test_many_alphas_mean_break.csv"))

###############################################
dat = list()


for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, break_loop = FALSE, alpha = alphas[i])) |> 
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(alpha = as.factor(alphas[i]))
  
}

seq_t_test_many_alphas_mean_no_break <- dat |>
  bind_rows() |> 
  mutate(n = as.factor(rep(n*2, times = repeats * l_a))) |> 
  na.omit() |> 
  group_by(n, alpha) |> 
  mutate(across(.cols = where(is.numeric), 
                .fns = list(mean = ~ mean(.), sd = ~ sd(.)),
                .names = "{col}_{fn}"))


write_csv(x= seq_t_test_many_alphas_mean_no_break, file = here::here("sim_data/seq_t_test_many_alphas_mean_no_break.csv"))

#############################################
dat = list()

for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, break_loop = FALSE, alpha = alphas[i], mean_diff = 0.5)) |> 
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(alpha = as.factor(alphas[i]))
  
}

seq_t_test_many_alphas_mean_with_diff <- dat |>
  bind_rows() |> 
  mutate(n = as.factor(rep(n*2, times = repeats * l_a))) |> 
  na.omit() |> 
  group_by(n, alpha) |> 
  mutate(across(.cols = where(is.numeric), 
                .fns = list(mean = ~ mean(.), sd = ~ sd(.)),
                .names = "{col}_{fn}"))


write_csv(x= seq_t_test_many_alphas_mean_with_diff, file = here::here("sim_data/seq_t_test_many_alphas_mean_with_diff.csv"))
#########################################

dat = list()

#Using a Rope of size 0.2

for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, 
                                                      alpha = alphas[i],
                                                      break_loop = T, 
                                                      mean_diff = 0, 
                                                      use_rope = T, 
                                                      margin = 0.2)) |>  
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(alpha = as.factor(alphas[i]))
  
}


seq_t_test_many_alphas_rope <- dat |>
  bind_rows() |> 
  mutate(n = as.integer(rep(n*2, times = repeats * l_a))) |> 
  group_by(alpha, n) |> 
  summarise(passed = sum(is.na(p)), 
            failed = (repeats - passed)/repeats, 
            .groups = "drop") |> 
  group_by(alpha) |> 
  summarise(prop = cumsum(failed), 
            .groups = "drop") |> 
  mutate(n = as.integer(rep(n*2, times = l_a)))

write_csv(x= seq_t_test_many_alphas_rope, file = here::here("sim_data/seq_t_test_many_alphas_rope.csv"))
