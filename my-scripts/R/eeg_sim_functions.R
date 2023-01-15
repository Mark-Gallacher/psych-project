library(tidyverse)


## Function to Generate 1 over F noise
one_over_f <- function(gamma = 1, N = 200, outvar = 1){
  N.2 <- N/2
  sine.waves <- matrix(NA, nrow = N, ncol = N.2)
  steps = 2 * pi * (1:N)/N
  phase <- stats::runif(N.2, 0, 2 * pi)
  for (i in 1:N.2) {
    freq <- i
    weight <- 1/(freq^gamma)
    y <- weight * sin(freq * steps + phase[i])
    sine.waves[, i] <- y
  }
  out <- rowSums(sine.waves)
  # force var = outvar and mean = 0
  out <- out - mean(out);
  out <- out / sd(out);
  out <- out * sqrt(outvar);
  return(out)
}


## Function to generate raw EEG data, for two groups


generate_raw_egg <- function(num_trials,        ## Vector or List containing size of trial(s)
                             num_time_points,    ## Number of Unique time points
                             max_time,           ## Maximum value of time, used to get freq of measurement (max/num_time_point)
                             cond1_base,         ## Template or Base used to add eeg noise to - for condition 1
                             cond2_base,         ## Template or Base used to add eeg noise to - for condition 2
                             gamma_spec_power = 1, 
                             noise_var = 1, 
                             stim_onset = 0){
  
  # create empty list to store df for condtions
  c_dfs <- list()
  
  for (i in 1:length(num_trials)) {
    
    cond1 <- matrix(0, nrow = num_trials[i], ncol = num_time_points)
    cond2 <- matrix(0, nrow = num_trials[i], ncol = num_time_points)
    
    # generate trials given the number of trials
    for(t in 1:num_trials[i]){
      
      # row = trial, col = time point
      cond2[t,] <- cond2_base + one_over_f(gamma = gamma_spec_power, num_time_points, outvar = noise_var)
      cond1[t,] <- cond1_base + one_over_f(gamma = gamma_spec_power, num_time_points, outvar = noise_var)  
      
    }
    
    
    # store as tibble where cols = time, rows = trials
    c1_df <- pivot_raw_eeg(cond1, max_time, max_time/ (num_time_points - 1), stim_onset = stim_onset) |> 
      mutate(n_trial = num_trials[i], 
             cond = "cond1")
    
    c2_df <- pivot_raw_eeg(cond2, max_time, max_time/ (num_time_points - 1), stim_onset = stim_onset)|> 
      mutate(n_trial = num_trials[i],
             cond = "cond2")
    
    ## storing combined dfs to then bind after loop
    c_dfs[[i]] <- bind_rows(c1_df, c2_df)
    
  }
  
  ## Combing tibbles to store all raw data
  eeg_raw_df <- bind_rows(c_dfs)
  
  return(eeg_raw_df)
  
}



## Function to pivot Raw Data created from generate_raw_egg()

pivot_raw_eeg <- function(df, max_time, freq, stim_onset = 0){
  output <- df |> 
    as_tibble(.name_repair = ~ as.character(seq(0, max_time, freq))) |> 
    pivot_longer(cols = everything(), 
                 names_to = "time", 
                 values_to = "value") |> 
    mutate(time = as.integer(time),
           time = time - stim_onset)
  
  return(output)
  
}


## Function to Get descriptive and inferential stats from raw EGG

get_stats_from_raw <- function(df, alpha = 0.05){
  
  
  output <- df |>  
    pivot_wider(names_from = cond, 
                values_from = value, 
                id_cols = c(time, n_trial), 
                values_fn = list) |> 
    unnest(cols = where(is.list)) |> 
    group_by(time, n_trial) |> 
    summarise(
      mn_cond1 = mean(cond1),
      mn_cond2 = mean(cond2),
      mn_diff = mn_cond2 - mn_cond1,
      p = t.test(cond1, cond2)[["p.value"]],
      ci_l = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][1],
      ci_u = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][2],
      .groups = "drop"
    )|>
    group_by(n_trial) |>
    mutate(bon_sig = if_else(p < alpha/length(unique(time)), min(ci_l), NULL),
           raw_sig = if_else(p < alpha, min(ci_l), NULL)) |>  # just so the graph has a line at zero to show significance
    ungroup()
  
  
  return(output)
}


get_eeg_rope <- function(df, alpha = 0.05, num_time_points, i = 0){
  ## Quantile range for pre-stim
  null_quart <- quantile(x =  df |> 
                           filter(time < 0) |> 
                           pull(mn_cond1, mn_cond2) |> 
                           as.vector(), 
                         probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for significant mean difference
  alt_quart <- quantile(x = df |> 
                          filter(p < alpha) |> 
                          pull(mn_diff) |> 
                          as.vector(), 
                        probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for bonferroni significant mean difference
  alt_quart_bonf <- quantile(x = df |> 
                               filter(p < (alpha/ num_time_points)) |> 
                               pull(mn_diff) |> 
                               as.vector(), 
                             probs = c(2*alpha, (1-2*alpha)))
  
  # Store values in tibble to later be combined with eeg_df
  rope_df <- tibble(n_trial_rope = sample_size[i+1], 
                    null_min = null_quart[1], 
                    null_max = null_quart[2], 
                    alt_min = alt_quart[1], 
                    alt_max = alt_quart[2],
                    alt_bonf_min = alt_quart_bonf[1], 
                    alt_bonf_max = alt_quart_bonf[2]
  )
  
  return(rope_df)
  
}






