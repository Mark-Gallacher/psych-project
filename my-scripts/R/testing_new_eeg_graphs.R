library(tidyverse)
library(pwr)
## Min alpha beta. 

df <- sim_df_1 |> 
  filter(!eval(time_cond)) |> 
  group_by(n_trial) |> 
  summarise(
    mean_peak = mean(mn_diff_13), 
    d = abs(mean_peak)/ sqrt(0.5 * sd (sd_cond1) + sd(sd_cond3)), 
    half_d = 0.5 * d,
    .groups = "drop"
    ) |> 
  rowwise() |> 
  mutate(
    beta_2 = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = half_d)$power), 
    new_alpha_2 = min(0.05, beta_2), 
    beta = 1 - (pwr.t.test(n = n_trial, sig.level = 0.05, d = d)$power), 
    new_alpha = min(0.05, beta)
    )



df |> 
  select(n_trial, new_alpha) |> 
  inner_join(sim_df_1, by = "n_trial") |> 
  select(1:19) |> 
  group_by(n_trial) |> 
  mutate(
    sig_12 = if_else(p_12 < new_alpha, min(ci_l_12), NULL),
    sig_13 = if_else(p_13 < new_alpha, min(ci_l_13), NULL),
  ) |> 
  ungroup() |> 
  ggplot(aes(x = time)) +
  geom_line(aes(y = mn_cond1), colour = line_colours[1], alpha = line_alpha, size = line_size) +
  geom_line(aes(y = mn_cond2), colour = line_colours[2], alpha = line_alpha, size = line_size) +
  geom_line(aes(y = mn_cond3), colour = line_colours[3],alpha = line_alpha, size = line_size) +
  geom_vline(xintercept = 0, colour = grey)+
  geom_hline(yintercept = 0, colour = grey)+
  geom_line(aes(y = sig_12 - 0.1), size = line_size, colour = sig_colours[1])+
  geom_line(aes(y = sig_13), size = line_size, colour = sig_colours[2])+
  # geom_ribbon(aes(ymax = ci_u, ymin = ci_l), alpha = fill_alpha, fill = grey)+
  facet_wrap(~n_trial, scale = "free_y")+
  scale_x_continuous("Time (ms)")+
  scale_y_continuous("")+
  labs(title = glue::glue(
    "<b>ERP Mean Values across Time with 
         <span style='color:{grey};'>95% Confidence Intervals</span> for Various Sample Sizes</b><br>"))



## plotting t-values

sim_df_1 |> 
  ggplot(aes(time))+
  geom_line(aes(y = t_12), colour = "cyan4")+
  geom_line(aes(y = t_13), colour = "darkred")+
  # alpha = 0.05
  geom_hline(aes(yintercept = qt(0.975, n_trial-2), group = n_trial))+
  # alpha = 0.005
  geom_hline(aes(yintercept = qt(1-1.335e-07, n_trial-2), group = n_trial))+
  facet_wrap(~n_trial)


