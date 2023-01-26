library(doParallel)
library(parallel)

doParallel::registerDoParallel(cores = detectCores() - 2)

seeds <- 1:1000

## With an underlying effect
sim_test_1 <- foreach::foreach(s = 1:length(seeds), .combine = "rbind") %dopar%{

sim_df_1 <- sim_eeg_rope_pipeline(
  sample_size = sample_size, 
  alpha = alpha, 
  num_time_points = Nf, 
  max_time = max_time, 
  cond1_base = temp1, 
  cond2_base = temp2, 
  gamma_spec_power = gsp, 
  noise_var = outvar,      
  stim_onset = stim_on, 
  seed = seeds[s], 
  static_margin = static_margins
) |> 
  dplyr::mutate(set = seeds[s]) |> 
  dplyr::filter(!is.na(time)) ## weird bug where we get one trial with sample of 0.26, with set number NA 

}

## when there is no effect
sim_test_2 <- foreach::foreach(s = 1:length(seeds), .combine = "rbind") %dopar%{
  
  sim_df_2 <- sim_eeg_rope_pipeline(
    sample_size = sample_size, 
    alpha = alpha, 
    num_time_points = Nf, 
    max_time = max_time, 
    cond1_base = temp1, 
    cond2_base = temp1, 
    gamma_spec_power = gsp, 
    noise_var = outvar,      
    stim_onset = stim_on, 
    seed = seeds[s], 
    static_margin = static_margins
  ) |> 
    dplyr::mutate(set = seeds[s]) |> 
    dplyr::filter(!is.na(time))
  
}

write_csv(sim_test_1, here::here("sim_data", "sim_egg_pipeline_eff.csv"))
write_csv(sim_test_2, here::here("sim_data", "sim_egg_pipeline_no_eff.csv"))
