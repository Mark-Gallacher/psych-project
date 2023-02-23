library(tidyverse)

df_test <- sim_eeg_pipeline(eeg_pipeline_attr, seed = seed)

df_test |> 
  generate_rope_df(rope_func = get_eeg_nu_rope, 
                   eeg_pipeline_attr, 
                   static_margins = static_margins, 
                   replication = F, sequential = F, 
                   null_rope = T)

df_rope <- df_test |> 
  generate_rope_df(rope_func = get_eeg_alter_rope, 
                   eeg_pipeline_attr, 
                   replication = F, 
                   sequential = F, 
                   null_rope = F)


# if(replication){
#   inner_join_cols <- c("experiment", "time", "n_trial")
# }else{
#   inner_join_cols <- c("time", "n_trial")
# }
# 
# # stats for comparisons between the three groups
# df_1 <- df |> 
#   pivot_for_stats(replication = replication)
# 
# # add null rope, then pivot so ROPEs are in single column
# df_2 <- df |> 
#   add_null_rope(alpha = pipeline_attr$alpha, 
#                 sample_size = pipeline_attr$sample_size, 
#                 num_time_points = pipeline_attr$Nf, 
#                 static_margins = static_margins, 
#                 sequential = sequential) |> 
#   pivot_for_rope(replication = replications) |>    
#   dplyr::inner_join(df_1, by = inner_join_cols)
# 
# df_3 <- df_2 |> 
#   dplyr::mutate(
#     sig_rope = dplyr::if_else(
#       df_2$p < 0.05 & (df_2$ci_u < df_2$min | df_2$ci_l > df_2$max), 0, NULL),
#     no_rope = dplyr::if_else(
#       df_2$p < 0.05, 0, NULL)
#   )
# 
# 



# df_test |> 
#   add_rope(rope_func = get_eeg_alter_rope, 
#                 alpha = alpha, 
#                 sample_size = sample_size, 
#                 sequential = F, 
#                 replication = F) |> 
#   dplyr::select(time, n_trial, dplyr::starts_with("alt_")) |> 
#   tidyr::pivot_longer(
#     cols = dplyr::starts_with("alt_"), 
#     names_to = c("rope", ".value"), 
#     names_pattern = "alt_([t_]*[[:alpha:]]+)_(min|max)"
#   )



df_stats |> 
  group_by(n_trial, experiment) |> 
  get_eeg_alter_rope(alpha = 0.05)

df_test |> 
  plot_base()

pt <- df_test |> 
  plot_sim_base(rope_df = df_rope, replication = F)

pt <- plot_rope(pt, df_rope, null_rope = F)






