df_test <- sim_repli_eeg_pipeline(
        pipeline_attr = eeg_pipeline_attr,
        seed = seed, 
        total_experiments = 2
        )


# df_test |> 
#   plot_base()+
#   facet_grid(rows = vars(n_trial), cols = vars(experiment))
# 


long_df_1 <- df_test |>
  pivot_for_mn(replication = T)

plot_test_base <- function(df){
  
  p <- df |> 
    ggplot(aes(x = time, y = mn, colour = cond)) +
    geom_line(alpha = line_alpha, size = line_size)+
    geom_vline(xintercept = 0, colour = grey)+
    geom_hline(yintercept = 0, colour = grey)+
    # geom_line(aes(y = raw_13_sig - 0.1), size = line_size, colour = sig_colours[1])+
    # geom_line(aes(y = bon_13_sig), size = line_size, colour = sig_colours[2])+
    # geom_ribbon(aes(ymax = ci_u, ymin = ci_l), alpha = fill_alpha, fill = grey)+
    facet_wrap(~n_trial, scale = "free_y")+
    scale_x_continuous("Time (ms)")+
    scale_y_continuous("")+
    # MetBrewer::scale_color_met_c(name = "Java")+
    labs(title = "ERP Mean Values of Three Conditions, at Various Sample Sizes", 
         colour = "Condition")+
    scale_colour_manual(values = MetBrewer::met.brewer("Austria", 3),
                        labels = c("1", "2", "3"))+
    theme(legend.position = "top")
  
  
  return(p)
}

long_df_1 |>
  plot_test_base()+
  facet_grid(vars(n_trial), vars(experiment))






