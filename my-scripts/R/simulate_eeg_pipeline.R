library(doParallel)
library(parallel)
library(tidyverse)

source(here::here("my-scripts/R/eeg_rope_pipeline.R"))

doParallel::registerDoParallel(cores = detectCores() - 2)

.true_onset <- 450  ## Effect at 450ms
.stim_on <- 300     ## Stimulus is shown at 300ms
.freq <- 4         ## Defines resolution of stimulation, frequency of electrode (This is around 250 reading a second)
.max_time <- 800    ## End time for Epoch

.Xf <- seq(0, .max_time, .freq) ## start from zero and end at max time, going up in 4ms (freq)
Nf <- length(.Xf)   ## Number of timepoints

# Template one only contains zeros
.temp1 <- vector(mode = "numeric", length = Nf)  

# define magnitude of large effect
.magnit <- 2

# Generate ERP Peak
.erp <- dnorm(seq(-1.5, 1.5, length.out = 200/.freq), 0, 1)
.erp <- .erp - min(.erp)
.erp_small <- .erp / max(.erp)
.erp_large <- .magnit*.erp_small

# Getting length and storing them for later use
.l_erp <- length(.erp)
.l_pre_stim <- ceiling(.true_onset / .freq)

# Template 2 contains the ERP peak
temp1 <- c(rep(0, .l_pre_stim), .erp_small, rep(0, (Nf - .l_erp - .l_pre_stim)))
temp2 <- c(rep(0, .l_pre_stim), .erp_large, rep(0, (Nf - .l_erp - .l_pre_stim)))


sample_size <- c(10, 25, 50, 100, 150, 200) # number of trials
.gsp <- 1 # gamma spectral power 
.outvar <- 1 # noise variance

alpha = 0.05 ## Standard Alpha Level for Significance Testing
static_margins = c(0.3, 0.2, 0.1) # static margins for NUll ROPE

effect_time <- c(.true_onset - .stim_on, .true_onset + 200 - .freq - .stim_on)
time_cond <- expression((time < effect_time[1] | time > effect_time[2]))

## storing values in list, so make functions neater
eeg_pipeline_attr <- list(
  sample_size = sample_size, # Defines Sample Size/ Number of Trials 
  alpha = alpha, 
  num_time_points = Nf, 
  max_time = .max_time, 
  cond1_base = temp1, 
  cond2_base = temp2, 
  gamma_spec_power = .gsp, 
  noise_var = .outvar,   # Influence Noise output - Zero means no Noise
  stim_onset = .stim_on  # Time when Stimulus was "shown"
)

seeds <- 1:1000

##### ROPE ######
## With an underlying effect
large_sim_test_1 <- foreach::foreach(s = 1:length(seeds), .combine = "rbind") %dopar%{

sim_df_1 <- sim_repli_eeg_pipeline(
  pipeline_attr = eeg_pipeline_attr, 
  total_experiments = 2, 
  get_bf = F, 
  seed = s
  )|> 
  dplyr::mutate(set = s) |> 
  dplyr::filter(!is.na(time)) ## weird bug where we get one trial with sample of 0.26, with set number NA 

}

large_sim_df_1 <- large_sim_test_1 |> 
  group_nest(set, .key = "studies")



large_sim_rope_df <- large_sim_df_1 |> 
  map(.x = large_sim_df_1$studies, .f = ~ .x |> 
        generate_rope_df(pipeline_attr = eeg_pipeline_attr, 
                         rope_func = get_eeg_nu_rope, 
                         null_rope = T, 
                         replication = T, 
                         sequential = F, 
                         replicate.effect = F, 
                         static_margins = static_margins)
      ) 

l_sim_rope_df <- bind_rows(large_sim_rope_df, .id = "set")|> 
  filter(experiment == "B", comparison == 13)

l_sim_rope_eval_df <- eval_rope(
          df = large_sim_test_1 |> filter(experiment == "B"), 
          rope_df = l_sim_rope_df, 
          alpha = 0.05, 
          effect_time_vector = effect_time) |> 
  select(!starts_with("sum"), -c(fp, tp, fn, tn))

# ### ROPE df
# write_csv(x = l_sim_rope_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_rope_df.csv"))
# 
# ## df for mean values
# write_csv(x = large_sim_test_1, file = here::here("sim_data/ch4_eeg_sims/large_sim_data_df.csv"))
# 
# ## df for rope eval
# write_csv(x = l_sim_rope_eval_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_rope_eval_df.csv"))

##### Dynamic Alpha #####

large_sim_test_1 <- read_csv(file = here::here("sim_data/ch4_eeg_sims/large_sim_data_df.csv"))
l_sim_rope_df <- read_csv(file = here::here("sim_data/ch4_eeg_sims/large_sim_rope_df.csv"))

large_sim_df_1 <- large_sim_test_1 |> 
  group_nest(set, .key = "studies")

## min alpha beta rule
large_sim_min_df <- large_sim_df_1 |> 
  map(.x = large_sim_df_1$studies, .f = ~ .x |> 
        get_new_alpha(method = "min.ab")
  )

## min alpha beta rule
large_sim_adapt_df <- large_sim_df_1 |> 
  map(.x = large_sim_df_1$studies, .f = ~ .x |> 
        get_new_alpha(method = "adapt.a")
  )

## binding output and making sure set is a numeric for future inner join
l_sim_min_df <- bind_rows(large_sim_min_df, .id = "set") |> 
  mutate(set = as.integer(set))

l_sim_adapt_df <- bind_rows(large_sim_adapt_df, .id = "set") |> 
  mutate(set = as.integer(set))


## compare new alpha to p-value, used to plot significant lines
l_sim_min_eval_df <- large_sim_test_1 |> 
  select(set, everything()) |> 
  inner_join(l_sim_min_df, by = c("set", "n_trial")) |> 
  eval_dyn_alpha() |> 
  select(-c(fp, tp, fn, tn))

l_sim_adapt_eval_df <- large_sim_test_1 |> 
  select(set, everything()) |> 
  inner_join(l_sim_adapt_df, by = c("set", "n_trial")) |> 
  eval_dyn_alpha()|> 
  select(-c(fp, tp, fn, tn))


# ### saving df containing new alphas
# write_csv(x = l_sim_min_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_min_df.csv"))
# write_csv(x = l_sim_adapt_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_adapt_df.csv"))
# 
# 
# ## df for rope eval
# write_csv(x = l_sim_min_eval_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_min_eval_df.csv"))
# write_csv(x = l_sim_adapt_eval_df, file = here::here("sim_data/ch4_eeg_sims/large_sim_adapt_eval_df.csv"))
