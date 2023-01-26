library(tidyverse)

source(file = here::here("my-scripts/R", "t_test_seq_testing_functions.R"))

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
  mutate(n = as.integer(rep(n*2, times = repeats * l_a))) |> 
  na.omit() |> 
  group_by(alpha, n) |> 
  summarise(failed = sum(!is.na(p))/repeats, 
            passed = (1 - failed),
            mean_diff = mean(mean_diff, na.rm = TRUE), 
            .groups = "drop")

  
## This might work better?
# seq_t_test_many_alphas_mean_break |> 
#   mutate(n = as.integer(n)) |> 
#   group_by(alpha, n) |> 
#   summarise(failed = cumsum(n()), .groups = "drop") |> 
#   mutate(proport = cumsum(failed)/repeats)

write_csv(x = seq_t_test_many_alphas_mean_break, file = here::here("sim_data/seq_t_test_many_alphas_mean_break.csv"))

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
  group_by(alpha, n) |> 
  summarise(failed = sum(!is.na(p))/repeats, 
            passed = (1 - failed),
            mean_diff = mean(mean_diff, na.rm = TRUE), 
            .groups = "drop")

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
  group_by(alpha, n) |> 
  summarise(failed = sum(!is.na(p))/repeats, 
            passed = (1 - failed),
            mean_diff = mean(mean_diff, na.rm = TRUE), 
            .groups = "drop")

write_csv(x= seq_t_test_many_alphas_mean_with_diff, file = here::here("sim_data/seq_t_test_many_alphas_mean_with_diff.csv"))
#########################################

dat = list()

#Using a Rope of size 0.5

for (i in 1:l_a){
  
  dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, 
                                                      alpha = alphas[i],
                                                      break_loop = T, 
                                                      mean_diff = 0, 
                                                      use_rope = T, 
                                                      margin = .5)) |>  
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(alpha = as.factor(alphas[i]))
  
}


seq_t_test_many_alphas_rope <- dat |>
  bind_rows() |> 
  mutate(n = as.integer(rep(n*2, times = repeats * l_a))) |> 
  group_by(alpha, n) |> 
  summarise(passed = sum(is.na(p))/repeats, 
            failed = (1 - passed),
            mean_diff = mean(mean_diff, na.rm = TRUE),
            .groups = "drop")

write_csv(x= seq_t_test_many_alphas_rope, file = here::here("sim_data/seq_t_test_many_alphas_rope.csv"))
#############################################################################
# dat = list()
# 
# #Using a variety of ROPEs with variety of Alphas
# 
# ropes <- seq(0,0.5, .1)
# rope_dat <- list()
# 
# for (r in 1:length(ropes)){
# 
# for (i in 1:l_a){
#   
#   dat[[i]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, 
#                                                       alpha = alphas[i],
#                                                       break_loop = T, 
#                                                       mean_diff = 0, 
#                                                       use_rope = T, 
#                                                       margin = ropes[r])) |>  
#     t() |> 
#     as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
#     map_dfr(~unlist(.x)) |> 
#     mutate(alpha = as.factor(alphas[i]))
#   
# }
# 
#   rope_dat[[r]] <- dat |>
#   bind_rows() |> 
#   mutate(n = as.integer(rep(n*2, times = repeats * l_a))) |> 
#   group_by(alpha, n) |> 
#   summarise(passed = sum(is.na(p)), 
#             failed = (repeats - passed)/repeats,
#             mean_diff = mean(mean_diff, na.rm = TRUE),
#             .groups = "drop") |> 
#   mutate(rope = ropes[r])
# 
# }
# 
# rope_dat |> 
#   bind_rows()
# 
# write_csv(x = seq_t_test_many_alphas_ropes, file = here::here("sim_data/seq_t_test_many_alphas_rope_1.csv"))

###########################################################
dat = list()

margins = seq(0, 0.5, 0.1)
#Using a Rope of size 1

for (m in 1:length(margins)){
  
  dat[[m]] <- replicate(repeats, expr = get_mean_diff(sample_size = n, 
                                                      alpha = 0.05,
                                                      break_loop = T, 
                                                      mean_diff = 0, 
                                                      use_rope = T, 
                                                      margin = margins[m])) |>  
    t() |> 
    as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
    map_dfr(~unlist(.x)) |> 
    mutate(margin = margins[m])
  
}

seq_t_test_many_alphas_rope_m <- dat |>
  bind_rows() |> 
  mutate(n = as.integer(rep(n*2, times = repeats * length(margins)))) |> 
  group_by(margin, n) |> 
  summarise(passed = sum(is.na(p))/repeats, 
            failed = (1 - passed),
            mean_diff = mean(mean_diff, na.rm = TRUE),
            .groups = "drop")



write_csv(x= seq_t_test_many_alphas_rope_m, file = here::here("sim_data/seq_t_test_many_alphas_rope_m.csv"))
