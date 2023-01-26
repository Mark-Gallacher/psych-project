
# Checking for Significance
p_cond <- expression(p < alpha)

# Time outside of an effect
time_cond <- expression((time < effect_time[1] | time > effect_time[2]))

## writing it out in full to be more explicit
fp_cond <- expression(eval(p_cond) & eval(time_cond))
tp_cond <- expression(eval(p_cond) & !eval(time_cond))
fn_cond <- expression(!eval(p_cond) & !eval(time_cond))
tn_cond <- expression(!eval(p_cond) & eval(time_cond))

# Creating df where the max and min of the ROPEs has its own column and 
# there is one column which contains info of what type of ROPE it is
rope_df <- sim_test_1 |>  
  pivot_longer(cols= -c(1:10, set),
               names_pattern = "^(nu_.*).*(...)$", 
               names_to = c("rope", "name")) |> 
  mutate(rope= if_else(rope=="", "value", rope)) |> 
  filter(!is.na(rope),
         !is.na(time)) |> 
  pivot_wider(id_cols = c(1:10, rope, set), 
              names_from = name, 
              values_from = value, 
              names_repair = "check_unique")

# creating a df to find fpr and fnr, to evaluate different ropes
eval_rope_df <- rope_df |>  
  ## writing it out in full to be more explicit
  mutate(
    fp = if_else(eval(fp_cond) & (ci_l > max | ci_u < min), 1, 0),
    tp = if_else(eval(tp_cond) & (ci_l > max | ci_u < min), 1, 0),
    fn = if_else(eval(fn_cond) & (ci_l <= max | ci_u >= min), 1, 0),
    tn = if_else(eval(tn_cond) & (ci_l <= max | ci_u >= min), 1, 0)
  ) |> 
  group_by(n_trial, rope) |> 
  summarise(across(.cols = c(fp, tp, fn, tn), .fns = ~ sum(.x))) |> 
  mutate(
    fpr = fp / (fp + tn),
    tpr = tp / (tp + fn),
    fnr = fn / (fn + tp),
    tnr = tn / (tn + fp)
  ) |> 
  filter(!is.na(fp))

# create a df to store all the fp, to use as baseline
baseline_df <- sim_test_1 |> 
  mutate(
    fp = if_else(eval(fp_cond), 1, 0),
    tn = if_else(eval(tn_cond), 1, 0),
    fn = if_else(eval(fn_cond), 1, 0),
    tp = if_else(eval(tp_cond), 1, 0),
  ) |> 
  select(n_trial, fp, tn, fn, tp) |> 
  filter(!is.na(n_trial)) |> 
  group_by(n_trial) |> 
  summarise(across(.cols = everything(), 
                   .fns = ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}"),
    .groups = "drop") |> 
  mutate(
    prop_fp = sum_fp/ (sum_fp + sum_tn),
    prop_tp = sum_tp / (sum_tp + sum_fn),
    prop_fn = sum_fn / (sum_fn + sum_tp),
    prop_tn = sum_tn / (sum_tn + sum_fp)
    )

output <- eval_rope_df |> 
  inner_join(baseline_df, by = "n_trial") |> 
  mutate(
    rel_fp = fp/sum_fp  # relative FP proportion to baseline (% of FP that were not prevented)
         ) |>
  tidy_rope_names()



  
  
  
  