library(tidyverse)
library(TOSTER)

source(file = here::here("./my-scripts/t_test_loop_functions.R"))

n = seq(5, 5000, length.out = 1000)
l_n = length(n)


n_log <- 10^(seq(1,4,.01))
l_nlog = length(n_log)

repeats = 1000

# Break loop at significant level
p_vals_tost_break <- replicate(repeats, expr = get_pvals(sample_size = n)) |>  
  as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))

# Don't break loop, 1 mil p values
p_vals_tost_no_break <- replicate(repeats, expr = get_pvals(sample_size = n, break_loop = FALSE)) |>
  as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))

# n goes up as a log, so see how sample increase changes shape of curve
p_vals_tost_break_logn <- replicate(repeats, expr = get_pvals(sample_size = n_log)) |>
  as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))

# alpha of 0.005
p_vals_tost_break_small_alpha <- replicate(repeats, expr = get_pvals(sample_size = n, alpha = 0.005)) |>
  as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))

# One Sided t-test
p_vals_tost_break_one_sample <- replicate(repeats, expr = get_pvals_one_sample(sample_size = n)) |>  
  as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))



write_csv(x = p_vals_tost_break, file = here::here("sim_data/p_vals_tost_break.csv"))
write_csv(x = p_vals_tost_no_break, file = here::here("sim_data/p_vals_tost_no_break.csv"))
write_csv(x = p_vals_tost_break_logn, file = here::here("sim_data/p_vals_tost_break_logn.csv"))
write_csv(x = p_vals_tost_break_small_alpha, file = here::here("sim_data//p_vals_tost_break_small_alpha.csv"))
write_csv(x = p_vals_tost_break_one_sample, file = here::here("sim_data//p_vals_tost_break_one_sample.csv"))


