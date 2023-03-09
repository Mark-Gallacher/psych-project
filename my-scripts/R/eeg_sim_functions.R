library(tidyverse)


## Function to Generate 1 over F noise
one_over_f <- function(gamma = 1, N = 200, outvar = 1, seed = 1){
  set.seed(seed)
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

## Function to convert t values into raw values, used to visualise the critical t
####################################### OLDER VERSION OF CODE #############################
## This is the older version, I might keep this because I think it may be useful
# convert_t_to_raw <- function(df, alpha = 0.05){
#   
#   # number of trials
#   lx <- sqrt(df$n_trial[1])
#   # standard error for each group and combined
#   sdex <- df$sd_cond1[1]/lx
#   sdey <- df$sd_cond3[1]/lx
#   se <- sqrt(sdex^2 + sdey^2)
#   # df - we need this to find the critical t value
#   df <- se^4 / (sdex^4/(lx-1) + sdey^4/(lx-1))
#   # finding critical t value
#   qt(p = alpha/2, df = df, lower.tail = F)
#   # back to raw units
#   raw <- t * se
#   
#   return(raw)
# }
#######################################

convert_t_to_raw <- function(df, alpha = 0.05, t_values, null_rope = F){
  raw <- c()
  # number of trials
  lx <- sqrt(df$n_trial[1])
  # standard error for each group and combined
  sdex <- df$sd_cond1[1]/lx
  if (null_rope){
    sdey <- df$sd_cond2[1]/lx
  }else{
    sdey <- df$sd_cond3[1]/lx
  }
  se <- sqrt(sdex^2 + sdey^2)
  # df - we need this to find the critical t value
  df <- se^4 / (sdex^4/(lx-1) + sdey^4/(lx-1))
  # finding critical t value
  qt(p = alpha/2, df = df, lower.tail = F)
  # back to raw units
  for (i in 1:length(t_values)) {
    raw[[i]] <- t_values[[i]] * se
  }
  return(raw)
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
  
  seed = seed
  # create empty list to store df for condtions
  c_dfs <- list()
  
  for (i in 1:length(num_trials)) {
    
    # empty matrix for all three conditions
    cond1 <- cond2 <- cond3 <- matrix(0, nrow = num_trials[i], ncol = num_time_points)
    
    # generate trials given the number of trials
    for(t in 1:num_trials[i]){
      
      # row = trial, col = time point
      # small effect
      cond1[t,] <- cond1_base + one_over_f(gamma = gamma_spec_power, num_time_points, outvar = noise_var, seed = (seed + t + i) * 5 )
      cond2[t,] <- cond1_base + one_over_f(gamma = gamma_spec_power, num_time_points, outvar = noise_var, seed = (seed + t + i) * 3 )
      # large effect
      cond3[t,] <- cond2_base + one_over_f(gamma = gamma_spec_power, num_time_points, outvar = noise_var, seed = seed + t + i)
      }
    # store as tibble where cols = time, rows = trials
    c1_df <- pivot_raw_eeg(df = cond1, 
                           max_time = max_time, 
                           freq = max_time/ (num_time_points - 1), 
                           stim_onset = stim_onset) |> 
      dplyr::mutate(n_trial = num_trials[i], 
             cond = "cond1")
    c2_df <- pivot_raw_eeg(df = cond2, 
                           max_time = max_time, 
                           freq = max_time/ (num_time_points - 1), 
                           stim_onset = stim_onset)|> 
      dplyr::mutate(n_trial = num_trials[i],
             cond = "cond2")
    c3_df <- pivot_raw_eeg(df = cond3, 
                           max_time = max_time, 
                           freq = max_time/ (num_time_points - 1), 
                           stim_onset = stim_onset)|> 
      dplyr::mutate(n_trial = num_trials[i],
                    cond = "cond3")
    ## storing combined dfs to then bind after loop
    c_dfs[[i]] <- dplyr::bind_rows(c1_df, c2_df, c3_df)
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
    dplyr::mutate(
      time = as.integer(time),
      time = time - stim_onset
      )
  return(output)
}
## Function to Get descriptive and inferential stats from raw EGG

get_stats_from_raw <- function(df, alpha = 0.05){
    # Columns with 12 or 13 in name refers to the groups, 
  # mean_diff_12 = mean diff between group 1 and group 2 
  # ie temp1 and temp2, cond1 and cond2
  # So mean_diff_13 = mean diff between cond1 and cond3
  output <- df |>  
    tidyr::pivot_wider(names_from = cond, 
                values_from = value, 
                id_cols = c(time, n_trial), 
                values_fn = list) |> 
    tidyr::unnest(cols = where(is.list)) |> 
    dplyr::group_by(time, n_trial) |> 
    dplyr::summarise(
      # mean values at each timepoint for each condition
      mn_cond1 = mean(cond1, na.rm = T),
      mn_cond2 = mean(cond2, na.rm = T),
      mn_cond3 = mean(cond3, na.rm = T),
      # sd at each timepoint for each condition
      sd_cond1 = sd(cond1, na.rm = T),
      sd_cond2 = sd(cond2, na.rm = T),
      sd_cond3 = sd(cond3, na.rm = T),
      # mean difference
      mn_diff_12 = mn_cond2 - mn_cond1,
      mn_diff_13 = mn_cond3 - mn_cond1,
      # p-values and t-values
      p_12 = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["p.value"]],
      p_13 = t.test(cond3, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["p.value"]],
      t_12 = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["statistic"]][[1]],
      t_13 = t.test(cond3, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["statistic"]][[1]],
      # 95% confidence intervals, alpha was 0.05
      ci_l_12 = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][1],
      ci_u_12 = t.test(cond2, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][2],
      ci_l_13 = t.test(cond3, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][1],
      ci_u_13 = t.test(cond3, cond1, conf.level = 1 - alpha, alternative = "two.sided")[["conf.int"]][2],
      .groups = "drop"
    )|>
    dplyr::group_by(n_trial) |>
    dplyr::mutate(
      bon_13_sig = dplyr::if_else(p_13 < alpha/length(unique(time)), min(ci_l_13), NA),
      raw_13_sig = dplyr::if_else(p_13 < alpha, min(ci_u_13), NA)
      ) |>  # just so the graph has a line at zero to show significance
    dplyr::ungroup()
  return(output)
}

get_bf_from_raw <- function(df){
  # Columns with 12 or 13 in name refers to the groups, 
  # mean_diff_12 = mean diff between group 1 and group 2 
  # ie temp1 and temp2, cond1 and cond2
  # So mean_diff_13 = mean diff between cond1 and cond3
  output <- df |>  
    tidyr::pivot_wider(names_from = cond, 
                       values_from = value, 
                       id_cols = c(time, n_trial), 
                       values_fn = list) |> 
    tidyr::unnest(cols = where(is.list)) |> 
    dplyr::group_by(time, n_trial) |> 
    dplyr::summarise(
      # mean values at each timepoint for each condition
      mn_cond1 = mean(cond1, na.rm = T),
      mn_cond2 = mean(cond2, na.rm = T),
      mn_cond3 = mean(cond3, na.rm = T),
      # sd at each timepoint for each condition
      sd_cond1 = sd(cond1, na.rm = T),
      sd_cond2 = sd(cond2, na.rm = T),
      sd_cond3 = sd(cond3, na.rm = T),
      # mean difference
      mn_diff_12 = mn_cond2 - mn_cond1,
      mn_diff_13 = mn_cond3 - mn_cond1,
      # bayes
      bf_12 = as.vector(BayesFactor::ttestBF(cond2, cond1, rscale = "medium"))[[1]],
      bf_13 = as.vector(BayesFactor::ttestBF(cond3, cond1, rscale = "medium"))[[1]],
      bfw_12 = as.vector(BayesFactor::ttestBF(cond2, cond1, rscale = "wide"))[[1]],
      bfw_13 = as.vector(BayesFactor::ttestBF(cond3, cond1, rscale = "wide"))[[1]],
      .groups = "drop"
    )|>
    dplyr::ungroup()
  return(output)
}

get_eeg_nu_rope <- function(df, alpha = 0.05, num_time_points, static_margins, replication = F){

  #### NULL ROPE
  if(replication) {
    f_pre <- quote(time < 0, experiment == "A")
    f_nosig <- quote(p_12 > alpha, experiment == "A")
    f <- quote(!eval(time_cond), experiment == "A")
  }
  else{
    f_pre <- quote(time < 0)
    f_nosig <- quote(p_12 > alpha)
    f <- quote(!eval(time_cond))
  } 
  ## T-values
  ## Quantile range for pre-stim
  nu_prestim_ <- quantile(x =  df |>
                           dplyr::filter(eval(f_pre)) |>
                           dplyr::pull(t_12) |>
                           as.vector(),
                         probs = c(2*alpha, (1-2*alpha)))
  
  nu_prestim <- convert_t_to_raw(df, alpha = alpha, t_values = nu_prestim_, null_rope = T)
  ## Quantile range for ROI
  nu_control_ <- quantile(x =  df |>
                           dplyr::filter(eval(f)) |>
                           dplyr::pull(t_12) |>
                           as.vector(),
                         probs = c(2*alpha, (1-2*alpha)))
  
  nu_control <- convert_t_to_raw(df, alpha = alpha, t_values = nu_control_, null_rope = T)
  ## Quantile range for non-significant mean difference
  nu_nonsig_ <- quantile(x = df |>
                          dplyr::filter(eval(f_nosig)) |>
                          dplyr::pull(t_12) |>
                          as.vector(),
                        probs = c(2*alpha, (1-2*alpha)))
  
  nu_nonsig <- convert_t_to_raw(df, alpha = alpha, t_values = nu_nonsig_, null_rope = T)
  ## t value from ROI between 1 and 2
  nu_controlt_ <- t.test(x =  df |>
           dplyr::filter(eval(f)) |>
           dplyr::pull(mn_cond1) |>
           as.vector(),
         y = df |>
           dplyr::filter(eval(f)) |>
           dplyr::pull(mn_cond2) |>
           as.vector(), 
         alternative = "two.sided", var.equal = F)$statistic[[1]]
  
  nu_controlt <- convert_t_to_raw(df, alpha = alpha, t_values = nu_controlt_, null_rope = T)[[1]]
  nu_controlt <- c(-abs(nu_controlt), abs(nu_controlt))
  
  
  ## Static Range - symmetrical
  nu_static_3 <- c(-static_margins[1], static_margins[1])
  nu_static_2 <- c(-static_margins[2], static_margins[2])
  nu_static_1 <- c(-static_margins[3], static_margins[3])
  
  # Store values in tibble to later be combined with df
  nu_colnames <- c(
    "nu_prestim", "nu_control","nu_controlt", "nu_nonsig", "nu_static_3", "nu_static_2", "nu_static_1" 
    #  ,"nu_eff_8", "nu_eff_95", "nu_midinter"
    )
  nu_cols <- list(
    nu_prestim, nu_control, nu_controlt, nu_nonsig, nu_static_3, nu_static_2, nu_static_1
    # nu_eff_8, nu_eff_95, # nu_midinter
    )
    
  null_rope_df <- matrix(unlist(nu_cols), ncol = 2*length(nu_cols)) |> 
    tidyr::as_tibble(.name_repair = ~ paste(rep(nu_colnames, each = 2), c("min", "max"), sep = "_"))
  
  return(null_rope_df)
  
  ######################### OLDER VERSION OF CODE - MAY BE USEFUL ############################
  ## Using Mean Difference of Group 1 and 2
  ## Quantile range for pre-stim
  # nu_prestim <- quantile(x =  df |> 
  #                          dplyr::filter(eval(f_pre)) |> 
  #                          dplyr::pull(mn_diff_13) |> 
  #                          as.vector(), 
  #                        probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for ROI
  # nu_control <- quantile(x =  df |> 
  #                          dplyr::filter(eval(f)) |>
  #                          dplyr::pull(mn_diff_12) |> 
  #                          as.vector(), 
  #                        probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for ROI
  # nu_controlabs <- quantile(x =  df |> 
  #                             dplyr::filter(eval(f)) |>
  #                             dplyr::pull(mn_diff_12) |> 
  #                             as.vector() |> 
  #                             abs(), 
  #                           probs = c(2*alpha, (1-2*alpha)))
  
  ## Quantile range for non-significant mean difference
  # nu_nonsig <- quantile(x = df |> 
  #                         dplyr::filter(eval(f_nosig)) |> 
  #                         dplyr::pull(mn_diff_12) |> 
  #                         as.vector(), 
  #                       probs = c(2*alpha, (1-2*alpha)))
  
  ## Minimal Effect Observable - Lakens Equivalence testing
  ## using two types of Power - Standard 0.8 and maximum for standard alpha (0.05), so 1- 0.05 = 0.95
  
  # d_8 <- pwr::pwr.t.test(n = unique(df$n_trial), sig.level = alpha, type = "two.sample", power = 0.8)$d
  # d_95 <- pwr::pwr.t.test(n = unique(df$n_trial), sig.level = alpha, type = "two.sample", power = 0.95)$d
  # 
  # raw_8 <- d_8 * sd(df$mn_diff)
  # raw_95 <- d_95 * sd(df$mn_diff)
  # 
  # nu_eff_8 <- c(-raw_8, raw_8)
  # nu_eff_95 <- c(-raw_95, raw_95)
  # 
  
  ## Half distancce between max value (absolute largest mean difference) and zero, 
  ## Null ROPE is 0 to the midpoint, Alternative is midpoint to max value
  
  # nu_midinter <- c(-abs(max(df$mn_diff)/2), abs(max(df$mn_diff)/2))
  
}

get_eeg_alter_rope <- function(df, alpha = 0.05, replication = F){
  
  if(replication) {
    f <- quote(!eval(time_cond), experiment == "A")
  }
  else{
    f <- quote(!eval(time_cond))
  }
    t_ <- quantile(x = df |> 
                        filter(eval(f)) |> 
                        pull(t_13) |> 
                        abs(), 
                      probs = c(2*alpha, (1-2*alpha)))
    # convert to raw so we can compare to CI range and plot ROPE on graph
    t_val <- convert_t_to_raw(df, alpha = alpha, t_values = t_, null_rope = F)
    
    # mn_val <- quantile(x = df |> 
    #                      filter(eval(f)) |> 
    #                      pull(mn_diff_13) |> 
    #                      abs(), 
    #                    probs = c(2*alpha, (1-2*alpha)))
  
  
  alt_colnames <- c("alt_t")
  alt_cols <- list(t_val)

  alt_rope_df <- matrix(unlist(alt_cols), ncol = 2*length(alt_cols)) |>
    tidyr::as_tibble(.name_repair = ~ paste(rep(alt_colnames, each = 2), c("min", "max"), sep = "_"))

  return(alt_rope_df)
  
}
  ############################### OLDER VERSION OF CODE ######################
  # 
  # ## Quantile range for significant mean difference
  # alt_quart <- quantile(x = df |> 
  #                         dplyr::filter(p < alpha) |> 
  #                         dplyr::pull(mn_diff) |> 
  #                         as.vector(), 
  #                       probs = c(2*alpha, (1-2*alpha)))
  # 
  # ## Quantile range for bonferroni significant mean difference
  # alt_quart_bonf <- quantile(x = df |> 
  #                              dplyr::filter(p < (alpha/ num_time_points)) |> 
  #                              dplyr::pull(mn_diff) |> 
  #                              as.vector(), 
  #                            probs = c(2*alpha, (1-2*alpha)))
  # 
  # alt_colnames <- c("alt_quart", "alt_quart_bonf")
  # alt_cols <- list(alt_quart, alt_quart_bonf)
  # 
  # alt_rope_df <- matrix(unlist(alt_cols), ncol = 2*length(alt_cols)) |> 
  #   tidyr::as_tibble(.name_repair = ~ paste(rep(alt_colnames, each = 2), c("min", "max"), sep = "_"))
  
  # return(dplyr::bind_cols(null_rope_df, alt_rope_df))


