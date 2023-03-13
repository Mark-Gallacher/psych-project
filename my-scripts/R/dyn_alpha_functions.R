library(tidyverse)
library(pwr)

## convert rep_df into a df with new alpha

get_new_alpha <- function(df, method = c("min.ab", "adapt.a")){

  if (missing(method)) {
    stop("method must be selected")
  }
  else {
    method <- match.arg(method)
  }
    
  df <- df |> 
    filter(!eval(time_cond), experiment == "A") |> 
    group_by(n_trial) |> 
    summarise(
      mean_peak = mean(mn_diff_13), 
      d = abs(mean_peak)/ sqrt(0.5 * sd (sd_cond1) + sd(sd_cond3)), 
      half_d = 0.5 * d,
      .groups = "drop"
    )
  
  if(method == "min.ab"){
    output <- df |> 
    rowwise() |> 
    mutate(
      beta_2 = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = half_d)$power), 
      new_alpha_2 = min(0.05, beta_2), 
      beta = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = d)$power), 
      new_alpha = min(0.05, beta)
    )
  }
  
  if(method == "adapt.a"){
    require(NetworkToolbox)
    
    output <- df |> 
      rowwise() |> 
      mutate(
        new_alpha = NetworkToolbox::adapt.a(test = "two.sample", n = n_trial, alpha = alpha, power = 0.8, efxize = d)$adapt.a,
        new_alpha_2 = NetworkToolbox::adapt.a(test = "two.sample", n = n_trial, alpha = alpha, power = 0.8, efxize = half_d)$adapt.a,
      )
  }

return(output)

  }

## df is the rep_df, containing the original data
## dyn_df is the df from get_new_alphas

get_sig_lines <- function(df, dyn_df){
  
  output <- df |> 
    pivot_for_stats(replication = T) |> 
    inner_join(dyn_df, by =  n_trial ) |> 
    mutate(
      sig_rule1 = if_else(p < new_alpha, 0, NA),
      sig_rule2 = if_else(p < new_alpha_2, 0, NA)
    ) |> 
    filter(experiment == "B") ## to apply only plot on the replication
}


plot_dyn_alpha <- function(plot, dyn_df, base_size = 12, method = c("min.ab", "adapt.a")){
  
  if (missing(method)) {
    stop("method must be selected")
  }
  else {
    method <- match.arg(method)
  }
  if(method == "min.ab"){
    sig_subtitle <- quote(glue::glue("Using Dynamic Alpha Rule - <b> Min (\u03B1, \u03B2) </b>"))
  }
  if(method == "adapt.a"){
    sig_subtitle <- quote("Using Adaptive Alpha Formula")
  }
  
  
plot <- plot + 
  geom_point(data = dyn_df, 
             aes(x = time, y = sig_rule1 - .8, fill = '3' ),
             alpha = line_alpha, inherit.aes = F, size = 1, shape = 22, stroke = NA)+
  geom_point(data = dyn_df, 
             aes(x = time, y = sig_rule2 - .6, fill = '4' ),
             alpha = line_alpha, inherit.aes = F, size = 1, shape = 22, stroke = NA)+
  scale_fill_manual("Significance", values = sig_colours, 
                    labels = c("Raw", "Bonf.", "Cohen's d", "1/2 of Cohen's d"),  
                    guide = guide_legend(override.aes = list(size = 5, colour = NA)))+
  labs(subtitle = eval(sig_subtitle))+
  theme_project_light(base_size = base_size)


return(list(plot))
}

eval_dyn_alpha <- function(df){
  
  # since we used both cohen's d and half cohens d, we have two alpha levels we need to compare
  
  # Checking for Significance
  p_cond <- expression(p_13 < new_alpha)
  p_cond_2 <- expression(p_13 < new_alpha_2)
  p_cond_base <- expression(p_13 < 0.05)
  
  # Time outside of an effect
  time_cond <- expression((time < effect_time[1] | time > effect_time[2]))
  
  ## cond for the cohen's d rule
  fp_cond <- expression(eval(p_cond) & eval(time_cond))
  tp_cond <- expression(eval(p_cond) & !eval(time_cond))
  fn_cond <- expression(!eval(p_cond) & !eval(time_cond))
  tn_cond <- expression(!eval(p_cond) & eval(time_cond))
  
  ## cond for the 1/2 cohen's d rule
  fp_cond_2 <- expression(eval(p_cond_2) & eval(time_cond))
  tp_cond_2 <- expression(eval(p_cond_2) & !eval(time_cond))
  fn_cond_2 <- expression(!eval(p_cond_2) & !eval(time_cond))
  tn_cond_2 <- expression(!eval(p_cond_2) & eval(time_cond))
  
  ## cond for the base rates
  fp_cond_base <- expression(eval(p_cond_base) & eval(time_cond))
  tp_cond_base <- expression(eval(p_cond_base) & !eval(time_cond))
  fn_cond_base <- expression(!eval(p_cond_base) & !eval(time_cond))
  tn_cond_base <- expression(!eval(p_cond_base) & eval(time_cond))
  
  # creating a df to find fpr and fnr, to evaluate different ropes
  eval_rope_df <- df |>  
    ## writing it out in full to be more explicit
    dplyr::mutate(
      ## cohen 'd used
      fp = dplyr::if_else(eval(fp_cond), 1, 0),
      tp = dplyr::if_else(eval(tp_cond), 1, 0),
      fn = dplyr::if_else(eval(fn_cond), 1, 0),
      tn = dplyr::if_else(eval(tn_cond), 1, 0),
      ## half cohen's d
      fp_2 = dplyr::if_else(eval(fp_cond_2), 1, 0),
      tp_2 = dplyr::if_else(eval(tp_cond_2), 1, 0),
      fn_2 = dplyr::if_else(eval(fn_cond_2), 1, 0),
      tn_2 = dplyr::if_else(eval(tn_cond_2), 1, 0),
      ## base rate
      fp_b = dplyr::if_else(eval(fp_cond_base), 1, 0),
      tp_b = dplyr::if_else(eval(tp_cond_base), 1, 0),
      fn_b = dplyr::if_else(eval(fn_cond_base), 1, 0),
      tn_b = dplyr::if_else(eval(tn_cond_base), 1, 0),
    ) |> 
    group_by(experiment, n_trial) |> 
    summarise(across(.cols = c(fp, tp, fn, tn, 
                               fp_2, tp_2, fn_2, tn_2,
                               fp_b, tp_b, fn_b, tn_b
                               ), .fns = ~ sum(.x)), .groups = "drop") |> 
    dplyr::mutate(
      ## for cohen's d
      fpr = fp / (fp + tn),
      tpr = tp / (tp + fn),
      fnr = fn / (fn + tp),
      tnr = tn / (tn + fp),
      ## for half cohen's d
      fpr_2 = fp_2 / (fp_2 + tn_2),
      tpr_2 = tp_2 / (tp_2 + fn_2),
      fnr_2 = fn_2 / (fn_2 + tp_2),
      tnr_2 = tn_2 / (tn_2 + fp_2),
      ## for base
      fpr_b = fp_b / (fp_b + tn_b),
      tpr_b = tp_b / (tp_b + fn_b),
      fnr_b = fn_b / (fn_b + tp_b),
      tnr_b = tn_b / (tn_b + fp_b)
    ) |> 
    filter(!is.na(fp)) ## weird bug ?
  
  return(eval_rope_df)
  
}


plot_eval_dyn_alpha <- function(min_df, adapt_df, rate = "fpr"){
  
  if(rate == "fpr"){
    title = "False Positves - Dynamic Alpha"
  }
  if(rate == "tpr"){
    title = "True Positves - Dynamic Alpha"
  }
  
  eval_min_df <- min_df |> 
    select(experiment, n_trial, contains(rate)) |> 
    pivot_longer(cols = -c(n_trial, experiment), 
                 names_to = "rate", 
                 values_to = "values"
    ) 
  # changing names to ensure they are unique before bind_rows
  eval_min_df$rate <- paste0("min_", eval_min_df$rate)
  
  eval_adapt_df <- adapt_df |> 
    select(experiment, n_trial, contains(rate)) |> 
    pivot_longer(cols = -c(n_trial, experiment), 
                 names_to = "rate", 
                 values_to = "values"
    ) 
  # changing names to ensure they are unique before bind_rows
  eval_adapt_df$rate <- paste0("adapt_", eval_adapt_df$rate)
  
  eval_dyn_df <- bind_rows(eval_min_df, eval_adapt_df)|> 
    filter(!grepl("adapt_fpr_b|adapt_tpr_b", rate), experiment == "A")
  
  p1 <- eval_dyn_df |>
    ggplot(aes(x = n_trial, y = values, group = rate, colour = rate))+
    geom_point(size = 2, shape = 15, show.legend = F)+
    geom_line(linewidth = 1.3)+
    theme_project_light(base_size = base_size)+
    scale_x_continuous("Sample Size", 
                       breaks = c(10, 25, 50, 100, 150, 200), 
                       labels = c("10", "25", "50", "100", "150", "200"))+
    scale_y_log10(stringr::str_to_upper(rate))+
    scale_color_manual(name = "Method", 
                       labels = c("Adapt. (1 d)", "Adapt. (0.5 d)", "Min. (1 d)", "Min. (0.5 d)", "Baseline"),
                       values = MetBrewer::met.brewer(name = "Java"  , type = "discrete", n = 5),
                       guide = guide_legend(override.aes = list(linewidth = 3), nrow = 2))+
    ggtitle(title)
  
  p2 <- eval_dyn_df |> 
    ggplot(aes(x = n_trial, y = values, group = rate, colour = rate))+
    geom_point(size = 2, shape = 15, show.legend = F)+
    geom_line(linewidth = 1.3)+
    geom_hline(yintercept = 0, linewidth = 1)+
    theme_project_light(base_size = base_size)+
    scale_x_continuous("Sample Size", 
                       breaks = c(10, 25, 50, 100, 150, 200), 
                       labels = c("10", "25", "50", "100", "150", "200"))+
    scale_y_continuous(stringr::str_to_upper(rate))+
    scale_color_manual(name = "Method", 
                       labels = c("Adapt. (1 d)", "Adapt. (0.5 d)", "Min. (1 d)", "Min. (0.5 d)", "Baseline"),
                       values = MetBrewer::met.brewer(name = "Java"  , type = "discrete", n = 5),
                       guide = guide_legend(override.aes = list(linewidth = 3), nrow = 2))+
    ggtitle(title)
  
  return(list(p1, p2))  
}


##### Testing, Please Ignore #####
# dyn_rep_1 <- rep_df_1 |> 
#   filter(!eval(time_cond), experiment == "A") |> 
#   group_by(n_trial) |> 
#   summarise(
#     mean_peak = mean(mn_diff_13), 
#     d = abs(mean_peak)/ sqrt(0.5 * sd (sd_cond1) + sd(sd_cond3)), 
#     half_d = 0.5 * d,
#     .groups = "drop"
#   ) |> 
#   rowwise() |> 
#   mutate(
#     beta = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = d)$power), 
#     beta_2 = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = half_d)$power), 
#     new_alpha = min(0.05, beta),
#     new_alpha_2 = min(0.05, beta_2)
#   )
# 
# dyn_rep_stats <- rep_df_1 |> 
#   pivot_for_stats(replication = T) |> 
#   inner_join(dyn_rep_1, by = "n_trial") |> 
#   mutate(
#     sig_rule1 = if_else(p < new_alpha, 0, NA),
#     sig_rule2 = if_else(p < new_alpha_2, 0, NA)
#   )
# 
# ## min difference required to be significant with new alpha rule
# min_diff <- convert_t_to_raw(sim_df_1, alpha = dyn_rep_1$new_alpha, 
#                              t_values = qt(dyn_rep_1$new_alpha, 100, lower.tail = F), null_rope = F) |> 
#   unlist()
# 
# min_diff2 <- convert_t_to_raw(sim_df_1, alpha = dyn_rep_1$new_alpha_2, 
#                               t_values = qt(dyn_rep_1$new_alpha_2, 100, lower.tail = F), null_rope = F) |> 
#   unlist()
# 
# min_diff_df <- tibble(
#   n_trial = unique(sim_df_1$n_trial),
#   min_diff_max = min_diff,
#   min_diff2_max = min_diff2,
#   min_diff_min = -min_diff,
#   min_diff2_min = -min_diff2,
# ) |> 
#   mutate(across(where(is.double), ~ if_else(!(abs(.) == Inf), ., NA)))
# 
# dyn_rep_stats <- inner_join(dyn_rep_stats, min_diff_df, by = "n_trial")
# 
# .rep_dyn2_p1 <- .rep_dyn_p1 + 
#   geom_ribbon(data = dyn_rep_stats, aes(x = time, ymin = min_diff_min, ymax = min_diff_max), inherit.aes = F, alpha = fill_alpha)+
#   geom_ribbon(data = dyn_rep_stats, aes(x = time, ymin = min_diff2_min, ymax = min_diff2_max), inherit.aes = F, alpha = fill_alpha)
# 
# .rep_dyn2_p1
# 



