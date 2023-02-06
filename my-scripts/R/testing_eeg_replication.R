plot_base <- function(df){
  
  p <- df |> 
    ggplot(aes(x = time)) +
    geom_line(aes(y = mn_cond1), colour = line_colours[1], alpha = line_alpha, size = line_size) +
    geom_line(aes(y = mn_cond2), colour = line_colours[2], alpha = line_alpha, size = line_size) +
    geom_line(aes(y = mn_cond3), colour = line_colours[3],alpha = line_alpha, size = line_size) +
    geom_vline(xintercept = 0, colour = grey)+
    geom_hline(yintercept = 0, colour = grey)+
    geom_line(aes(y = raw_13_sig - 0.1), size = line_size, colour = sig_colours[1])+
    geom_line(aes(y = bon_13_sig), size = line_size, colour = sig_colours[2])+
    # geom_ribbon(aes(ymax = ci_u, ymin = ci_l), alpha = fill_alpha, fill = grey)+
    facet_wrap(~n_trial, scale = "free_y")+
    scale_x_continuous("Time (ms)")+
    scale_y_continuous("")+
    labs(title = glue::glue(
      "<b>ERP Mean Values across Time with 
         <span style='color:{grey};'>95% Confidence Intervals</span> for Various Sample Sizes</b><br>"))
  
  
  return(p)
}

df_test <- sim_repli_eeg_rope_pipeline(
        sample_size = sample_size,
        alpha = alpha, 
        num_time_points = Nf, 
        max_time = max_time, 
        cond1_base = temp1, 
        cond2_base = temp2, 
        gamma_spec_power = gsp, 
        noise_var = outvar,       
        stim_onset = stim_on,     
        seed = seed, 
        static_margin = static_margins, 
        total_experiments = 2
        )


df_test |> plot_base()+
  facet_grid(rows = vars(n_trial), cols = vars(experiment))
