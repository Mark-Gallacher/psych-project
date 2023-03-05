library(tidyverse)
library(doParallel)
library(parallel)

cores <- parallel::detectCores() - 2

doParallel::registerDoParallel(cores = cores)


source(file = here::here("my-scripts/R", "t_test_loop_functions.R"))

#### Sequential t-tests with no ROPE - multiple alphas ####

alphas = c(0.05, 0.01, 0.005, 0.001, 0.0005)
l_a = length(alphas) 

n = seq(5, 5000, length.out = 1000)
l_n = length(n)

repeats = 5000 
dat = list()


dat <- foreach::foreach(i = 1:l_a, .combine = "rbind")%dopar%{
  
  replicate(repeats, 
            expr = simulate_t_tests(sample_size = n, 
                                 break_loop = TRUE, 
                                 alpha = alphas[i])) |> 
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(alpha = alphas[i])
  
}

seq_alphas <- dat |>
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = T)


write_csv(x = seq_alphas, file = here::here("sim_data/ch2_t_tests", "seq_alphas.csv"), num_threads = cores)

#### Simulating t-tests with no ROPE - multiple alphas ####
dat1 = list()


dat1 <- foreach::foreach(i = 1:l_a, .combine = "rbind")%dopar%{
  
  replicate(repeats, 
            expr = simulate_t_tests(sample_size = n, 
                                    break_loop = FALSE, 
                                    alpha = alphas[i])) |> 
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(alpha = alphas[i])
  
}

sim_alphas_raw <- dat1 |>
  mutate(n = as.factor(rep(n*2, times = repeats * l_a))) 

sim_alphas <- dat1 |> 
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = F)

write_csv(x= sim_alphas_raw, file = here::here("sim_data/ch2_t_tests", "sim_alphas_raw.csv.gz"), num_threads = cores)
write_csv(x= sim_alphas, file = here::here("sim_data/ch2_t_tests", "sim_alphas.csv"), num_threads = cores)

#### Simulating t-tests with no ROPE - with an underlying effect - for multiple alphas ####
dat2 = list()

dat2 <- foreach::foreach(i = 1:l_a, .combine = "rbind")%dopar%{
  
  replicate(repeats, 
            expr = simulate_t_tests(sample_size = n, 
                                    break_loop = FALSE, 
                                    alpha = alphas[i], 
                                    mean_diff = 0.5)) |> 
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(alpha = alphas[i])
  
}

sim_diff_alphas_raw <- dat2 |>
  mutate(n = as.factor(rep(n*2, times = repeats * l_a)))

sim_diff_alphas <- dat2 |> 
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = F)

write_csv(x= sim_diff_alphas_raw, file = here::here("sim_data/ch2_t_tests", "sim_diff_alphas_raw.csv.gz"), num_threads = cores)
write_csv(x= sim_diff_alphas, file = here::here("sim_data/ch2_t_tests", "sim_diff_alphas.csv"), num_threads = cores)
#### Simulating t-tests with a ROPE of 0.2 - for multiple alphas ####

dat3 = list()

#Using a Rope of size 0.2

# dat3 <- foreach::foreach(i = 1:l_a, .combine = "rbind")%dopar%{
#   
#   replicate(repeats, 
#             expr = simulate_t_tests(sample_size = n, 
#                                     break_loop = FALSE, 
#                                     alpha = alphas[i], 
#                                     mean_diff = 0, 
#                                     use_rope = T, 
#                                     margin = 0.2)) |> 
#     t() |> 
#     tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci")) |> 
#     purrr::map_dfr(~unlist(.x)) |> 
#     dplyr::mutate(alpha = alphas[i])
#   
# }
# 
# seq_rope_alphas <- dat3 |>
#   summarise_t_test_loop(n = n, repeats = repeats)
# 
# write_csv(x= seq_rope_alphas, file = here::here("sim_data/ch2_t_tests", "seq_rope_alphas.csv"), num_threads = cores)
##### Sequential t-tests with various ROPEs - alpha 0f 0.05 ####
dat4 = list()

margins = seq(0, 0.5, 0.1)

dat4 <- foreach::foreach(m = 1:length(margins), .combine = "rbind")%dopar%{
    
    replicate(repeats, 
              expr = simulate_t_tests(sample_size = n, 
                                      alpha = 0.05,
                                      break_loop = T, 
                                      mean_diff = 0, 
                                      use_rope = T, 
                                      margin = margins[m])) |>  
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(margins = margins[m])
  
}
  

seq_rope_margins_raw <- dat4 |>
  mutate(n = rep(n*2, times = repeats * length(margins)))

seq_rope_margins <- dat4 |>
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = T, margins = T)

write_csv(x = seq_rope_margins, file = here::here("sim_data/ch2_t_tests", "seq_rope_margins.csv"), num_threads = cores)
write_csv(x = seq_rope_margins_raw, file = here::here("sim_data/ch2_t_tests", "seq_rope_margins_raw.csv.gz"), num_threads = cores)


##### Simulating t-tests with various ROPEs - alpha 0f 0.05 ####
dat5 = list()

margins = seq(0, 0.5, 0.1)

dat5 <- foreach::foreach(m = 1:length(margins), .combine = "rbind")%dopar%{
  
  replicate(repeats, 
            expr = simulate_t_tests(sample_size = n, 
                                    alpha = 0.05,
                                    break_loop = F, 
                                    mean_diff = 0, 
                                    use_rope = T, 
                                    margin = margins[m])) |>  
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(margins = margins[m])
  
}

sim_rope_margins_raw <- dat5 |>
  mutate(n = as.factor(rep(n*2, times = repeats * length(margins))))

sim_rope_margins <- dat5 |>
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = F, margins = T)


write_csv(x = sim_rope_margins, file = here::here("sim_data/ch2_t_tests", "sim_rope_margins.csv"), num_threads = cores)
write_csv(x = sim_rope_margins_raw, file = here::here("sim_data/ch2_t_tests", "sim_rope_margins_raw.csv.gz"), num_threads = cores)

##### Simulating t-tests with various small ROPEs - alpha 0f 0.05 ####
dat6 = list()

margins2 = seq(0, 0.25, 0.05)

dat6 <- foreach::foreach(m = 1:length(margins2), .combine = "rbind")%dopar%{
  
  replicate(repeats, 
            expr = simulate_t_tests(sample_size = n, 
                                    alpha = 0.05,
                                    break_loop = F, 
                                    mean_diff = 0, 
                                    use_rope = T, 
                                    margin = margins2[m])) |>  
    t() |> 
    tidyr::as_tibble(.name_repair = ~c("mean_diff", "p", "l_ci", "u_ci", "place")) |> 
    purrr::map_dfr(~unlist(.x)) |> 
    dplyr::mutate(margins = margins2[m])
  
}

sim_rope_small_margins_raw <- dat6 |>
  mutate(n = as.factor(rep(n*2, times = repeats * length(margins2))))

sim_rope_small_margins <- dat6 |>
  summarise_t_test_loop(n = n, repeats = repeats, seq.test = F, margins = T)


write_csv(x = sim_rope_small_margins, file = here::here("sim_data/ch2_t_tests", "sim_rope_small_margins.csv"), num_threads = cores)
write_csv(x = sim_rope_small_margins_raw, file = here::here("sim_data/ch2_t_tests", "sim_rope_small_margins_raw.csv.gz"), num_threads = cores)



