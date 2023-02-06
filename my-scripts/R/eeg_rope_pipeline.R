library(tidyverse)


## Step one - Define sample sizes
# 
# sample_size <- c(25, 50, 100, 200, 400, 800)
# Nt <- 50 # number of trials
# gsp <- 0.7 # gamma spectral power
# outvar <- 1 # noise variance

source(here::here("my-scripts", "R", "eeg_sim_functions.R"))

sim_eeg_rope_pipeline <- function(sample_size, 
                                  alpha, 
                                  num_time_points, 
                                  max_time,
                                  cond1_base, 
                                  cond2_base, 
                                  gamma_spec_power = 1, 
                                  noise_var = 1, 
                                  stim_onset = 0, 
                                  seed = 1, 
                                  static_margins){
  

output <- list()
  
for (i in 1:length(sample_size)){
  ## Step two - generate noise using set sample size
  eeg_raw_df <- generate_raw_egg(num_trials = sample_size[i],        
                                 num_time_points = num_time_points,    
                                 max_time = max_time,           
                                 cond1_base = cond1_base,         
                                 cond2_base = cond2_base,         
                                 gamma_spec_power = gamma_spec_power, 
                                 noise_var = noise_var, 
                                 stim_onset = stim_onset, 
                                 seed = seed
                                 )
  
  ## Step three - analysis eeg - p values, mean diff, etc
  eeg_df <- eeg_raw_df |> 
    get_stats_from_raw(alpha = alpha)
  
  if(i == 1){  

    # We want to rope to be defined by the current trial but applied to the next trial
    # So the ROPEs will be saved to the following trial
    output[[i]] <- eeg_df
    
    ## Step four - Construct the two ropes (Null and Alt) - check if overlap?
   rope_df <- get_eeg_rope(df = eeg_df, 
                           alpha = alpha, 
                           num_time_points = num_time_points, 
                           i = i, 
                           static_margins = static_margins
                           )
    
  }else{  
    
    ## Join dataframes into one
    output[[i]] <- eeg_df |> 
      dplyr::bind_cols(rope_df)
      
    ## Create new rope_df after saving previous one - this means rope from n=25, will align with data from n=50, meaning graphs should be easier
    rope_df <- get_eeg_rope(df = eeg_df, 
                            alpha = alpha, 
                            num_time_points = num_time_points, 
                            static_margins = static_margins
                            )
      
  }
    ## Step five - repeat with larger sample, until all sample sizes are done
}
  
return(dplyr::bind_rows(output))

}

sim_repli_eeg_rope_pipeline <- function(sample_size, 
                                        alpha, 
                                        num_time_points, 
                                        max_time,
                                        cond1_base, 
                                        cond2_base, 
                                        gamma_spec_power = 1, 
                                        noise_var = 1, 
                                        stim_onset = 0, 
                                        seed = 1, 
                                        static_margins, 
                                        total_experiments = 2){
  
  # Creating Letters to ID replications, usually focused on 2 total experiments (original and replication)
  experiments <- LETTERS[1:total_experiments]
  
  output <- vector("list", length = length(experiments))
  
  for (i in 1:length(experiments)) {
    
  output[[i]] <- sim_eeg_rope_pipeline(sample_size = sample_size, 
                        alpha = alpha, 
                        num_time_points = num_time_points, 
                        max_time = max_time,
                        cond1_base = cond1_base, 
                        cond2_base = cond2_base, 
                        gamma_spec_power = gamma_spec_power, 
                        noise_var = noise_var, 
                        stim_onset = stim_onset, 
                        seed = (seed + (i - 1)), # ensures seed is different between replication and original
                        static_margins = static_margins
                        ) |> 
    mutate(experiment = experiments[i]) |> 
    select(experiment, everything())
    
  }
  return(dplyr::bind_rows(output))
}








tidy_rope_names <- function(df){

  df |> 
    ungroup() |> ## to make sure there is no grouped col
    mutate(rope = df$rope |> 
             str_remove("^(nu_)") |> 
             str_remove("_$")
           )
}


## getting number of false positives, false negatives, true positives and true negatives for each rope
## each ROPE is then compared to the total number of fp when not using a ROPE








eval_rope <- function(df, alpha, effect_time_vector){
  
  
  # Checking for Significance
  p_cond <- expression(p < alpha)
  
  # Time outside of an effect
  time_cond <- expression((time < effect_time[1] | time > effect_time[2]))
  
  ## writing it out in full to be more explicit
  fp_cond <- expression(eval(p_cond) & eval(time_cond))
  tp_cond <- expression(eval(p_cond) & !eval(time_cond))
  fn_cond <- expression(!eval(p_cond) & !eval(time_cond))
  tn_cond <- expression(!eval(p_cond) & eval(time_cond))
  
  # Creating df where the max and min of the ROPEs has its own column and 
  # there is one column which contains info of what type of ROPE it is
  rope_df <- df |>  
    pivot_longer(cols= -c(1:10, set),
                 names_pattern = "^(nu_.*).*(...)$", 
                 names_to = c("rope", "name")) |> 
    mutate(rope= if_else(rope=="", "value", rope)) |> 
    filter(!is.na(rope),
           !is.na(time)) |> 
    pivot_wider(id_cols = c(1:10, rope, set), 
                names_from = name, 
                values_from = value, 
                names_repair = "check_unique")
  
  # creating a df to find fpr and fnr, to evaluate different ropes
  eval_rope_df <- rope_df |>  
    ## writing it out in full to be more explicit
    mutate(
      fp = if_else(eval(fp_cond) & (ci_l > max | ci_u < min), 1, 0),
      tp = if_else(eval(tp_cond) & (ci_l > max | ci_u < min), 1, 0),
      fn = if_else(eval(fn_cond) & (ci_l <= max | ci_u >= min), 1, 0),
      tn = if_else(eval(tn_cond) & (ci_l <= max | ci_u >= min), 1, 0)
    ) |> 
    group_by(n_trial, rope) |> 
    summarise(across(.cols = c(fp, tp, fn, tn), .fns = ~ sum(.x))) |> 
    mutate(
      fpr = fp / (fp + tn),
      tpr = tp / (tp + fn),
      fnr = fn / (fn + tp),
      tnr = tn / (tn + fp)
    ) |> 
    filter(!is.na(fp))
  
  # create a df to store all the fp, to use as baseline
  baseline_df <- df |> 
    mutate(
      fp = if_else(eval(fp_cond), 1, 0),
      tn = if_else(eval(tn_cond), 1, 0),
      fn = if_else(eval(fn_cond), 1, 0),
      tp = if_else(eval(tp_cond), 1, 0),
    ) |> 
    select(n_trial, fp, tn, fn, tp) |> 
    filter(!is.na(n_trial)) |> 
    group_by(n_trial) |> 
    summarise(across(.cols = everything(), 
                     .fns = ~ sum(.x, na.rm = T), 
                     .names = "sum_{.col}"),
              .groups = "drop") |> 
    mutate(
      prop_fp = sum_fp/ (sum_fp + sum_tn),
      prop_tp = sum_tp / (sum_tp + sum_fn),
      prop_fn = sum_fn / (sum_fn + sum_tp),
      prop_tn = sum_tn / (sum_tn + sum_fp)
    )
  
  output <- output <- eval_rope_df |> 
    inner_join(baseline_df, by = "n_trial") |> 
    mutate(
      rel_fp = fp/sum_fp,  # relative FP proportion to baseline (% of FP that were not prevented)
      rel_tp = tp/sum_tp  # relative TP proportion to baseline (% of TP that were not prevented)
    ) |>
    tidy_rope_names()
  
  
  return(output)
  
}


## Trying to make second part of condition dynamic
## Min is Always first

# nu <- "^(nu_).*"
# min <- "(_min)$"
# max <- "(_max)$"
# 
# nu_min <- str_c(nu, min)
# nu_max <- str_c(nu, max)
# 
# colnames(sim_test_1)[str_detect(string = colnames(sim_test_1), pattern = nu_min)]
# colnames(sim_test_1)[str_detect(string = colnames(sim_test_1), pattern = nu_max)]

# It might be worth playing around with this later, as dynamically creating columns in tidyverse can be tricky




