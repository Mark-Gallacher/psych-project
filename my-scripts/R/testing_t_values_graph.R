df_test <- sim_repli_eeg_pipeline(
  pipeline_attr = eeg_pipeline_attr,
  seed = seed, 
  total_experiments = 2
)


df_stats <- df_test |> 
  pivot_for_stats(replication = T)

df_stats |> 
  filter(!eval(time_cond), comparison == "13", experiment == "A") |> 
  pull(t)


df_stats |> 
  ggplot(aes(x = time, colour = comparison))+
  geom_line(aes(y = t))+
  geom_line(aes(y = diff), linetype = "dashed")+
  facet_grid(rows = vars(n_trial), cols = vars(experiment), scales = "free")

## Combinging the df_stats with the null rope, then adding col to plot where the significant effects are
df_rope <- df_test |> 
  generate_rope_df(pipeline_attr = eeg_pipeline_attr, static_margins = static_margins, replication = F, sequential = F)

rope_test <- df_rope$rope |> unique()

## Graph showing the three condtions and with the line showing significant (relative to rope)
p_test <- df_test |>
  pivot_for_mn(replication = F) |> 
  plot_base()+
  facet_wrap(vars(n_trial)
             # , cols = vars(experiment)
             )+
  geom_point(data = subset(df_rope, rope == rope_test[[4]]), 
             aes(x = time, y = no_rope - 1, fill = comparison), size = 1.5, inherit.aes = F, alpha = .5, stroke = NA, shape = 22, show.legend = F)

# no rope
p_test  


##  Static 1
p_test +
  geom_ribbon(data = subset(df_rope, rope == rope_test[[4]]),
              aes(x = time, ymin = min, ymax = max), inherit.aes = F, alpha = .2)+
  geom_point(data = subset(df_rope, rope == rope_test[[4]]),
             aes(x = time, y = sig_rope - .9, fill = comparison), size = 1.5, inherit.aes = F, alpha = .5, stroke = NA, shape = 23)+
  scale_fill_discrete(name = "Comparison", 
                      labels = c("Cond 1 vs Cond 2", "Cond 1 vs Cond 3"),
                      guide = guide_legend(override.aes = list(size = 5, colour = NA, shape = 22, alpha = 1)))

## Static 2
p_test +
  geom_ribbon(data = subset(df_rope, rope == rope_test[[5]]),
              aes(x = time, ymin = min, ymax = max), inherit.aes = F, alpha = .2)+
  geom_point(data = subset(df_rope, rope == rope_test[[5]]),
             aes(x = time, y = sig_rope - .9, fill = comparison), inherit.aes = F, alpha = .5, shape = 22, stroke=NA)+
  scale_fill_discrete("Comparison", labels = c("Cond 1 vs Cond 2", "Cond 1 vs Cond 3"),
                      guide = guide_legend(override.aes = list(size = 5, colour = NA)))

## graph showing the ci and mean differences, to see the rope interact with Ci intervals
df_rope |> 
  ggplot(aes(x = time, y = diff, colour = comparison))+
  geom_line(size = 1, show.legend = F)+
  facet_grid(rows = vars(n_trial), cols = vars(experiment))+
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = comparison), alpha = .2, show.legend = F, colour = NA)+
  geom_ribbon(data = subset(df_rope, rope == "control"),
              aes(x = time, ymin = min, ymax = max), inherit.aes = F, alpha = .3, fill = "black")+
  geom_point(data = subset(df_rope, rope == "control"),
             aes(x = time, y = sig_rope -1, fill = comparison), inherit.aes = F, alpha = 1, size = 2, shape = 22, stroke=NA)+
  scale_fill_discrete("Comparison", labels = c("Cond 1 vs Cond 2", "Cond 1 vs Cond 3"), 
                      guide = guide_legend(override.aes = list(size = 5, colour = NA)))
