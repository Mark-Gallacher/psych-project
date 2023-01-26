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
                             stim_onset = 0, 
                             seed = 1){
  
  set.seed(seed)
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
      dplyr::mutate(n_trial = num_trials[i], 
             cond = "cond1")
    
    c2_df <- pivot_raw_eeg(cond2, max_time, max_time/ (num_time_points - 1), stim_onset = stim_onset)|> 
      dplyr::mutate(n_trial = num_trials[i],
             cond = "cond2")
    
    ## storing combined dfs to then bind after loop
    c_dfs[[i]] <- dplyr::bind_rows(c1_df, c2_df)
    
  }
  
  ## Combing tibbles to store all raw data
  eeg_raw_df <- dplyr::bind_rows(c_dfs)
  
  return(eeg_raw_df)
  
}



## Function to pivot Raw Data created from generate_raw_egg()

pivot_raw_eeg <- function(df, max_time, freq, stim_onset = 0){
  output <- df |> 
    tidyr::as_tibble(.name_repair = ~ as.character(seq(0, max_time, freq))) |> 
    tidyr::pivot_longer(cols = dplyr::everything(), 
                 names_to = "time", 
                 values_to = "value") |> 
    dplyr::mutate(time = as.integer(time),
           time = time - stim_onset)
  
  return(output)
  
}


## Function to Get descriptive and inferential stats from raw EGG

get_stats_from_raw <- function(df, alpha = 0.05){
  
  
  output <- df |>  
    tidyr::pivot_wider(names_from = cond, 
                values_from = value, 
                id_cols = c(time, n_trial), 
                values_fn = list) |> 
    tidyr::unnest(cols = where(is.list)) |> 
    dplyr::group_by(time, n_trial) |> 
    dplyr::summarise(
      mn_cond1 = mean(cond1),
      mn_cond2 = mean(cond2),
      mn_diff = mn_cond2 - mn_cond1,
      p = t.test(cond1, cond2)[["p.value"]],
      ci_l = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][1],
      ci_u = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][2],
      .groups = "drop"
    )|>
    dplyr::group_by(n_trial) |>
    dplyr::mutate(bon_sig = dplyr::if_else(p < alpha/length(unique(time)), min(ci_l), NULL),
           raw_sig = dplyr::if_else(p < alpha, min(ci_l), NULL)) |>  # just so the graph has a line at zero to show significance
    dplyr::ungroup()
  
  
  return(output)
}



get_eeg_rope <- function(df, alpha = 0.05, num_time_points, i = 0, static_margins){
  
  #### NULL ROPE
  
  ## Quantile range for pre-stim
  nu_prestim <- quantile(x =  df |> 
                           dplyr::filter(time < 0) |> 
                           dplyr::pull(mn_cond1, mn_cond2) |> 
                           as.vector(), 
                         probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for pre-stim
  nu_control <- quantile(x =  df |> 
                           dplyr::pull(mn_cond2) |> 
                           as.vector(), 
                         probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for non-significant mean difference
  nu_nonsig <- quantile(x = df |> 
                          dplyr::filter(p > alpha) |> 
                          dplyr::pull(mn_diff) |> 
                          as.vector(), 
                        probs = c(2*alpha, (1-2*alpha)))
  
  ## Minimal Effect Observable - Lakens Equivalence testing
  ## using two types of Power - Standard 0.8 and maximum for standard alpha (0.05), so 1- 0.05 = 0.95
  
  d_8 <- pwr::pwr.t.test(n = unique(df$n_trial), sig.level = alpha, type = "two.sample", power = 0.8)$d
  d_95 <- pwr::pwr.t.test(n = unique(df$n_trial), sig.level = alpha, type = "two.sample", power = 0.95)$d
  
  raw_8 <- d_8 * sd(df$mn_diff)
  raw_95 <- d_95 * sd(df$mn_diff)
  
  nu_eff_8 <- c(-raw_8, raw_8)
  nu_eff_95 <- c(-raw_95, raw_95)
  
  ## Static Range - symmetrical
  nu_static_3 <- c(-static_margins[1], static_margins[1])
  nu_static_2 <- c(-static_margins[2], static_margins[2])
  nu_static_1 <- c(-static_margins[3], static_margins[3])
  
  ## Half distancce between max value (absolute largest mean difference) and zero, 
  ## Null ROPE is 0 to the midpoint, Alternative is midpoint to max value
  
  nu_midinter <- c(-abs(max(df$mn_diff)/2), abs(max(df$mn_diff)/2))
  
  # Store values in tibble to later be combined with eeg_df
  nu_colnames <- c("nu_prestim", "nu_control", "nu_nonsig", 
                   "nu_eff_8", "nu_eff_95", "nu_static_3", 
                   "nu_static_2", "nu_static_1", "nu_midinter")
  
  nu_cols <- list(nu_prestim, nu_control, nu_nonsig, nu_eff_8, nu_eff_95, nu_static_3, nu_static_2, nu_static_1, nu_midinter)
  
  null_rope_df <- matrix(unlist(nu_cols), ncol = 2*length(nu_cols)) |> 
    tidyr::as_tibble(.name_repair = ~ paste(rep(nu_colnames, each = 2), c("min", "max"), sep = "_"))
  
  
  #### ALTERNATIVE ROPE
  
  ## Quantile range for significant mean difference
  alt_quart <- quantile(x = df |> 
                          dplyr::filter(p < alpha) |> 
                          dplyr::pull(mn_diff) |> 
                          as.vector(), 
                        probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for bonferroni significant mean difference
  alt_quart_bonf <- quantile(x = df |> 
                               dplyr::filter(p < (alpha/ num_time_points)) |> 
                               dplyr::pull(mn_diff) |> 
                               as.vector(), 
                             probs = c(2*alpha, (1-2*alpha)))
  
  alt_colnames <- c("alt_quart", "alt_quart_bonf")
  alt_cols <- list(alt_quart, alt_quart_bonf)
  
  alt_rope_df <- matrix(unlist(alt_cols), ncol = 2*length(alt_cols)) |> 
    tidyr::as_tibble(.name_repair = ~ paste(rep(alt_colnames, each = 2), c("min", "max"), sep = "_"))
  
  return(dplyr::bind_cols(null_rope_df, alt_rope_df))
  
}
