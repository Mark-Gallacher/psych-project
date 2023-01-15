library(tidyverse)


## Step one - Define sample sizes
# 
# sample_size <- c(25, 50, 100, 200, 400, 800)
# Nt <- 50 # number of trials
# gsp <- 0.7 # gamma spectral power
# outvar <- 1 # noise variance

source(here::here("my-scripts", "eeg_sim_functions.R"))

sim_eeg_rope_pipeline <- function(sample_size, alpha, num_time_points, max_time, cond1_base, cond2_base, gamma_spec_power = 1, noise_var = 1, stim_onset = 0){
  

output <- list()
  
for (i in 1:length(sample_size)){
  ## Step two - generate noise using set sample size
  eeg_raw_df <- generate_raw_egg(num_trials = sample_size[i], 
                                 num_time_points = num_time_points, 
                                 max_time = max_time, 
                                 cond1_base = cond1_base, 
                                 cond2_base = cond2_base, 
                                 gamma_spec_power = gamma_spec_power, 
                                 noise_var = outvar, 
                                 stim_onset = stim_onset)
  
  ## Step three - analysis eeg - p values, mean diff, etc
  eeg_df <- eeg_raw_df |> 
    get_stats_from_raw(alpha = alpha)
  
  if(i == 1){  
    # We want to rope to be defined by the current trial but applied to the next trial
    # So the ROPEs will be saved to the following trial
    output[[i]] <- eeg_df
    
    ## Steo four - Construct the two ropes (Null and Alt) - check if overlap?
   rope_df <- get_eeg_rope(df = eeg_df, alpha = alpha, num_time_points = num_time_points, i = i)
    
  }else{  
    
    ## Join dataframes into one
    output[[i]] <- eeg_df |> 
      bind_cols(rope_df)
      
    ## Create new rope_df after saving previous one - this means rope from n=25, will align with data from n=50, meaning graphs should be easier
    rope_df <- get_eeg_rope(df = eeg_df, alpha = alpha, num_time_points = num_time_points)
      
  }
    ## Step five - repeat with larger sample, until all sample sizes are done
}
  
return(bind_rows(output))

}

# 
# sim_df <- sim_eeg_rope_pipeline(sample_size = sample_size, 
#                       alpha = 0.05, 
#                       num_time_points = Nf, 
#                       max_time = max_time, 
#                       cond1_base = temp1, 
#                       cond2_base = temp2, 
#                       gamma_spec_power = gsp)



















