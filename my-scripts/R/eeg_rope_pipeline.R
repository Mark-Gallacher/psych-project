library(tidyverse)


## Step one - Define sample sizes
# 
# sample_size <- c(25, 50, 100, 200, 400, 800)
# Nt <- 50 # number of trials
# gsp <- 0.7 # gamma spectral power
# outvar <- 1 # noise variance
source(here::here("my-scripts", "R", "eeg_sim_functions.R"))

sim_eeg_pipeline <- function(pipeline_attr,
                             get_bf = F,
                             seed = 1){
  

output <- list()
  
for (i in 1:length(pipeline_attr$sample_size)){
  ## Step two - generate noise using set sample size
  eeg_raw_df <- generate_raw_egg(num_trials = pipeline_attr$sample_size[i],        
                                 num_time_points = pipeline_attr$num_time_points,    
                                 max_time = pipeline_attr$max_time,           
                                 cond1_base = pipeline_attr$cond1_base,         
                                 cond2_base = pipeline_attr$cond2_base,         
                                 gamma_spec_power = pipeline_attr$gamma_spec_power, 
                                 noise_var = pipeline_attr$noise_var, 
                                 stim_onset = pipeline_attr$stim_onset, 
                                 seed = seed
                                 )
  
  ## Step three - analysis eeg - p values, mean diff, etc
  if(get_bf){
    eeg_df <- eeg_raw_df |> 
      get_bf_from_raw()
  }
  else{
    eeg_df <- eeg_raw_df |> 
      get_stats_from_raw(alpha = pipeline_attr$alpha)
  }
  output[[i]] <- eeg_df
}
return(dplyr::bind_rows(output))
}
  
add_rope <- function(df, alpha = 0.05, sample_size, rope_func, sequential = TRUE, replication = F, ...){

  # create empty vector to store rope values
  rope_df <- vector("list", length = length(sample_size))

  # when sequential is true, we apply rope to next "experiment"
  if(sequential){
    for (i in 1:length(sample_size)){ 
      if((i+1) <= length(sample_size)){  
         # We want to rope to be defined by the current trial but applied to the next trial
         # So the ROPEs will be saved to the following trial
         # Construct the two ropes (Null and Alt)
         rope_df[[i+1]] <- df |> 
            dplyr::filter(n_trial == sample_size[i]) |> 
            rope_func(replication = replication, ...)
      }
    }
    # fill first row with NA of length as other objects in list, with same names
    rope_df[[1]] <- array(NA, dim = length(names(rope_df[[2]])))
    names(rope_df[[1]]) <- rope_df[[2]] |> names()
    # bind the cols in list together into one df
    full_rope_df <- dplyr::bind_rows(rope_df) |> 
     dplyr::mutate(n_trial = sample_size)
    }
  ## when sequential is false, rope is applied to own data
  if(!sequential){
    for (i in 1:length(sample_size)){ 
      rope_df[[i]] <- df |> 
        dplyr::filter(n_trial == sample_size[i]) |>  
        rope_func(replication = replication, ...)
    }
    # bind the cols in list together into one df
    full_rope_df <- dplyr::bind_rows(rope_df) |> 
     dplyr::mutate(n_trial = sample_size)
  }
    # add back to main df supplied to function, lining up sample size and time.
  output <- df |> 
      dplyr::inner_join(full_rope_df, by = "n_trial")
 
return(output)
}

sim_repli_eeg_pipeline <- function( pipeline_attr, 
                                    seed = 1, 
                                    total_experiments = 2, 
                                    get_bf = F,
                                    ...){
  
  # Creating Letters to ID replications, usually focused on 2 total experiments (original and replication)
  experiments <- LETTERS[1:total_experiments]
  
  output <- vector("list", length = length(experiments))
  
  for (i in 1:length(experiments)) {
    
    output[[i]] <- sim_eeg_pipeline(
                        pipeline_attr, 
                        seed = (seed + (i - 1)), # ensures seed is different between replication and original
                        get_bf = get_bf
                        ) |> 
    dplyr::mutate(experiment = experiments[i]) |> 
    dplyr::select(experiment, dplyr::everything())
    
  }
  return(dplyr::bind_rows(output))
}

pivot_for_stats <- function(df, replication = F){
  
  if (replication){
    cols_list <- c("experiment", "time", "n_trial")
  }else{
    cols_list <- c("time", "n_trial")
  }
    
    ## storing p, t and mean difference
    df1 <- df |> 
     dplyr::select({{ cols_list }},  dplyr::contains(c("p_", "t_", "diff"))) |> 
     tidyr::pivot_longer(
        cols = dplyr::contains(c("p_", "t_", "diff")), 
        names_to = c(".value", "comparison"), 
        names_pattern = "([[:alpha:]]+)[_.]*(\\d+)"
      )
    
    # storing upper and lower confidence interval
    df2 <- df |> 
     dplyr::select({{ cols_list }}, dplyr::contains("ci_")) |> 
     tidyr::pivot_longer(
        cols = dplyr::contains(c("ci_")), 
        names_to = c(".value", "comparison"), 
        names_pattern = "(.*)_(.*)"
      )
    # combining dataframes into one
    output <- dplyr::inner_join(df1, df2, by = c(cols_list , "comparison"))
    
return(output)
}

pivot_for_mn <- function(df, replication = F){
  if (replication){
    cols_list <- c("experiment", "time", "n_trial")
  }else{
    cols_list <- c("time", "n_trial")
  }
   df1 <- df |>
   dplyr::select({{cols_list}}, dplyr::starts_with(c("mn_cond", "sd_cond"))) |> 
   tidyr::pivot_longer(
      cols = dplyr::starts_with(c("mn_cond", "sd_cond")), 
      names_to = c(".value", "cond"),
      # names_prefix = "mn_", 
      names_sep = "_"
    ) 
 return(df1)
}

pivot_for_rope <- function(df, replication = F, null_rope = T){
  if(null_rope){
    col_str <- "nu_"
    name_pattern <- "nu_([t_]*[[:alpha:]]+[_\\d]*)_(min|max)"
  }else{
    col_str <- "alt_"
    name_pattern <- "alt_([t_]*[[:alpha:]]+)_(min|max)"
  }
  if (replication){
    cols_list <- c("experiment", "time", "n_trial")
  }else{
    cols_list <- c("time", "n_trial")
  }
  ## Use after add_rope to put the ropes in single column and their max and min in two columns. 

  df |> 
   dplyr::select({{ cols_list }}, dplyr::starts_with(col_str)) |> 
   tidyr::pivot_longer(
      cols = dplyr::starts_with(col_str), 
      names_to = c("rope", ".value"), 
      names_pattern = name_pattern
   )
  
}

# after the egg sim pipeline, generate stats, rope and then pivot them separately, to finally combine for the output
generate_rope_df <- function(df, pipeline_attr, rope_func, null_rope, replication = F, sequential = F, replicate.effect = F,...){
  
  if(replication){
    inner_join_cols <- c("experiment", "time", "n_trial")
  }else{
    inner_join_cols <- c("time", "n_trial")
  }
  if(replicate.effect && !null_rope){
    rope_sig <- quote(
      dplyr::if_else((df_2$p < 0.05), 
            if_else((df_2$ci_u < df_2$max), 
                    if_else((df_2$ci_l > df_2$min), 0, NA), NA), NA)
      )
    message("\nAn Alternative ROPE was used to Define Significance")
    
  }else{
    rope_sig <- quote(
      dplyr::if_else(
        (df_2$p < 0.05) & (df_2$ci_u < df_2$min | df_2$ci_l > df_2$max), 0, NA)
    )
    message("\nA Null ROPE was used to Define Significance")
  }
  if(deparse(substitute(rope_func)) == "get_eeg_nu_rope"){
    num_time_points = pipeline_attr$Nf
  }
  # stats for comparisons between the three groups
  df_1 <- df |> 
    pivot_for_stats(replication = replication)
  
  # add null rope, then pivot so ROPEs are in single column
  df_2 <- df |> 
    add_rope(rope_func = rope_func, 
             alpha = pipeline_attr$alpha, 
             sample_size = pipeline_attr$sample_size, 
             sequential = sequential,
             replication = replication,
             ...) |> 
    pivot_for_rope(replication = replication, 
                   null_rope = null_rope) |>    
    dplyr::inner_join(df_1, by = inner_join_cols)
  
  df_3 <- df_2 |> 
    dplyr::mutate(
      sig_rope = eval(rope_sig),
      raw_sig = dplyr::if_else(df_2$p < 0.05, 0, NA),
      bon_sig = dplyr::if_else(df_2$p < (0.05 / length(unique(df$time))), 0, NA),
    )
  
  return(df_3)
}

# sig_rule <- quote(df_test$p < 0.05 && (df_test$ci_u < df_2$max && df_test$ci_l > df_test$min))
# 
# df_test1 <- rep_df_1 |> 
#   pivot_for_stats(replication = T)
# 
# df_test <- rep_df_1 |> 
#   add_rope(alpha = 0.05, sample_size = sample_size, rope_func = get_eeg_alter_rope, sequential = F, replication = T) |> 
#   pivot_for_rope(replication = T, null_rope = F)|>    
#   dplyr::inner_join(df_test1, by = inner_join_cols)
# 
# test <- df_test |> 
#   dplyr::mutate(
#     # sig_rope = dplyr::if_else(all((df_test$p < 0.05) && (df_test$ci_u < df_test$max) && (df_test$ci_l > df_test$min)), 0, NULL),
#     sig_rope = if_else((df_test$p < 0.05), 
#                        if_else((df_test$ci_u < df_test$max), 
#                                if_else((df_test$ci_l > df_test$min), 0, NULL), NULL), NULL
#                        ),
#     lower_ci = if_else((df_test$ci_l > df_test$min), 
#                        if_else((df_test$p < 0.05), 0, NULL), NULL
#                        ),
#     no_rope  = dplyr::if_else(df_test$p < 0.05, 0, NULL)
#   )
# 
# sum(!is.na(test$sig_rope))

tidy_rope_names <- function(df){

  df |> 
   dplyr::ungroup() |> ## to make sure there is no grouped col
   dplyr::mutate(rope = df$rope |> 
             str_remove("^(nu_)") |> 
             str_remove("_$")
           )
}


## getting number of false positives, false negatives, true positives and true negatives for each rope
## each ROPE is then compared to the total number of fp when not using a ROPE


eval_rope <- function(df, rope_df, alpha, effect_time_vector){
  
  
  # Checking for Significance
  p_cond <- expression(p < alpha)
  p13_cond <- expression(p_13 < alpha)
  # bonferroni significance
  p13_bcond <- expression(p_13 < (alpha/length(unique(df$time))))
  
  # Time outside of an effect
  time_cond <- expression((time < effect_time[1] | time > effect_time[2]))
  
  ## writing it out in full to be more explicit
  fp_cond <- expression(eval(p_cond) & eval(time_cond))
  tp_cond <- expression(eval(p_cond) & !eval(time_cond))
  fn_cond <- expression(!eval(p_cond) & !eval(time_cond))
  tn_cond <- expression(!eval(p_cond) & eval(time_cond))

  ## writing it out in full to be more explicit
  fp13_cond <- expression(eval(p13_cond) & eval(time_cond))
  tp13_cond <- expression(eval(p13_cond) & !eval(time_cond))
  fn13_cond <- expression(!eval(p13_cond) & !eval(time_cond))
  tn13_cond <- expression(!eval(p13_cond) & eval(time_cond))
  
  fp13_bcond <- expression(eval(p13_bcond) & eval(time_cond))
  tp13_bcond <- expression(eval(p13_bcond) & !eval(time_cond))
  fn13_bcond <- expression(!eval(p13_bcond) & !eval(time_cond))
  tn13_bcond <- expression(!eval(p13_bcond) & eval(time_cond))
  
  # creating a df to find fpr and fnr, to evaluate different ropes
  eval_rope_df <- rope_df |>  
    ## writing it out in full to be more explicit
   dplyr::mutate(
      fp = dplyr::if_else(eval(fp_cond) & (ci_l > max | ci_u < min), 1, 0),
      tp = dplyr::if_else(eval(tp_cond) & (ci_l > max | ci_u < min), 1, 0),
      fn = dplyr::if_else(eval(fn_cond) & (ci_l <= max | ci_u >= min), 1, 0),
      tn = dplyr::if_else(eval(tn_cond) & (ci_l <= max | ci_u >= min), 1, 0)
    ) |> 
    group_by(experiment, n_trial, rope) |> 
    summarise(across(.cols = c(fp, tp, fn, tn), .fns = ~ sum(.x)), .groups = "drop") |> 
   dplyr::mutate(
      fpr = fp / (fp + tn),
      tpr = tp / (tp + fn),
      fnr = fn / (fn + tp),
      tnr = tn / (tn + fp)
    ) |> 
    filter(!is.na(fp)) ## weird bug ?
  
  # create a df to store all the fp, to use as baseline
  baseline_df <- df |> 
   dplyr::mutate(
      fp = dplyr::if_else(eval(fp13_cond), 1, 0),
      tn = dplyr::if_else(eval(tn13_cond), 1, 0),
      fn = dplyr::if_else(eval(fn13_cond), 1, 0),
      tp = dplyr::if_else(eval(tp13_cond), 1, 0),
      fp_b = dplyr::if_else(eval(fp13_bcond), 1, 0),
      tn_b = dplyr::if_else(eval(tn13_bcond), 1, 0),
      fn_b = dplyr::if_else(eval(fn13_bcond), 1, 0),
      tp_b = dplyr::if_else(eval(tp13_bcond), 1, 0),
    ) |> 
   dplyr::select(n_trial, experiment, fp, tn, fn, tp, fp_b, tn_b, fn_b, tp_b) |> 
    filter(!is.na(n_trial)) |> 
    group_by(n_trial, experiment) |> 
    summarise(across(.cols = dplyr::everything(), 
                     .fns = ~ sum(.x, na.rm = T), 
                     .names = "sum_{.col}"),
              .groups = "drop") |> 
   dplyr::mutate(
      prop_fp = sum_fp / (sum_fp + sum_tn),
      prop_tp = sum_tp / (sum_tp + sum_fn),
      prop_fn = sum_fn / (sum_fn + sum_tp),
      prop_tn = sum_tn / (sum_tn + sum_fp),
      prop_fp_b = sum_fp_b / (sum_fp_b + sum_tn_b),
      prop_tp_b = sum_tp_b / (sum_tp_b + sum_fn_b),
      prop_fn_b = sum_fn_b / (sum_fn_b + sum_tp_b),
      prop_tn_b = sum_tn_b / (sum_tn_b + sum_fp_b)
    )
  
  output <- eval_rope_df |> 
    dplyr::inner_join(baseline_df, by = c("n_trial", "experiment")) |> 
    dplyr::mutate(
      rel_fp = fp/sum_fp,  # relative FP proportion to baseline (% of FP that were not prevented)
      rel_tp = tp/sum_tp   # relative TP proportion to baseline (% of TP that were not prevented)
    ) 
  
  
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




