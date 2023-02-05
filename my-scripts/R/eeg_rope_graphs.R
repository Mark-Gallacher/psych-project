library(tidyverse)

## theme for the plots
source(here::here("my-scripts", "R", "project_themes.R"))
# set theme to project_theme
theme_set(theme_project_light())


## default colours

## Red/ Blue Palette - Troy palette from MetBrewer
# troy_palette <- c("#421401", "#6c1d0e", "#8b3a2b", "#c27668", "#7ba0b4", "#44728c", "#235070", "#0a2d46")
# # Tam palette from MetBrewer
# tam_palette <- c("#ffd353", "#ffb242", "#ef8737", "#de4f33", "#bb292c", "#9f2d55", "#62205f", "#341648")
# # Renoir from MetBrewer
# renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#f6b3b0", "#e48171", "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")
# line_colours <- grDevices::colorRampPalette(palette)(8)
# the demo shows 8 colours and the second and second last ones are my favourite

line_colours <- c("#2f357c", "#bf3729", "#235070")
# colours for the lines of significance
sig_colours <-  c("deepskyblue4", "darkblue")
# grey for ci range - fill
grey = "azure4"
fill_colours = c("#00BFC4", "#F8766D")

# variables for the main liines
line_size = 1
line_alpha = .8
# alpha for the ci range fill
fill_alpha = .2



## Graph for FPs on y axis, and different ROPEs = Different Colour
## Four graphs because we can use FWER or % of false positives (so 20% would mean that 80% of false positives were prevented compared to raw p-values)
## Sometimes a log scale is clearer to see difference but arithmetric scale is easier to understand naturally. (4 is just the combinations of these two factors)
fp_graphs <- function(df, effect = T){
  
  
  g1 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, fpr, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = prop_fp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = prop_fp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_continuous("FWER")+
    scale_color_discrete(name = "Method")+
    ggtitle("Minimising False Positves across Difference Sample Sizes")
  
  
  g2 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, fpr, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = prop_fp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = prop_fp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_log10("FWER")+
    scale_color_discrete(name = "Method")+
    ggtitle("Minimising False Positves across Difference Sample Sizes")

  
  g3 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, 100*rel_fp, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = 100*rel_fp/rel_fp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = 100*rel_fp/rel_fp), colour = "black")+ ## I know prop_fp/prop_fp = 1, but I am showing where it comes from
    scale_x_continuous("Sample Size")+
    scale_y_continuous("Percentage of FP captured (%)")+
    scale_color_discrete(name = "Method")+
    ggtitle("Minimising False Positves across Difference Sample Sizes")
  
  g4 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, 100*rel_fp, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = 100*rel_fp/rel_fp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = 100*rel_fp/rel_fp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_log10("Percentage of FP captured (%)")+
    scale_color_discrete(name = "Method")+
    ggtitle("Minimising False Positves across Difference Sample Sizes")
  
  
  
if (effect){
  g1 <- g1 + labs(subtitle = "With an Underlying Effect")
  g2 <- g2 + labs(subtitle = "With an Underlying Effect")
  g3 <- g3 + labs(subtitle = "With an Underlying Effect")
  g4 <- g4 + labs(subtitle = "With an Underlying Effect")
}else{
  g1 <- g1 + labs(subtitle = "With No Underlying Effect")
  g2 <- g2 + labs(subtitle = "With No Underlying Effect")
  g3 <- g3 + labs(subtitle = "With No Underlying Effect")
  g4 <- g4 + labs(subtitle = "With No Underlying Effect")
}
  
  
  return(print(list(g1,g2,g3,g4)))
}

## Same as above but for True positives
tp_graphs <- function(df){
  
  g1 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, tpr, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = prop_tp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = prop_tp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_log10("TPR")+
    scale_color_discrete(name = "Method")+
    ggtitle("Maximising True Positves across Difference Sample Sizes")
  
  g2 <- df |> 
    filter(n_trial != 10) |> 
    ggplot(aes(n_trial, tpr, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = prop_tp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = prop_tp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_continuous("TPR")+
    scale_color_discrete(name = "Method")+
    ggtitle("Maximising True Positves across Difference Sample Sizes")

  g3 <- df |>
    filter(n_trial != 10) |>
    ggplot(aes(n_trial, 100*rel_tp, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = 100*rel_tp/rel_tp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = 100*rel_tp/rel_tp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_log10("Percentage of TP Relative to No-ROPE (%)")+
    scale_color_discrete(name = "Method")+
    ggtitle("Maximising True Positves across Difference Sample Sizes")

  g4 <- df |>
    filter(n_trial != 10) |>
    ggplot(aes(n_trial, 100*rel_tp, colour = as.factor(rope), group = rope))+
    geom_line()+
    geom_point()+
    geom_line(aes(y = 100*rel_tp/rel_tp), colour = "black")+ ## showing baseline values - fp for no ROPE
    geom_point(aes(y = 100*rel_tp/rel_tp), colour = "black")+
    scale_x_continuous("Sample Size")+
    scale_y_continuous("Percentage of TP Relative to No-ROPE (%)")+
    scale_color_discrete(name = "Method")+
    ggtitle("Maximising True Positves across Difference Sample Sizes")
  
  return(print(list(g1,g2, g3, g4)))
  
}


## For the plot where we have EEG data and we want to show what each ROPE looks like
## This is the base function, which we will use to build the rest of the ROPE graphs
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

plot_nu_rope <- function(df){
  
  # getting unique ropes, used to get max and min columns later
  ropes <- colnames(df)[str_detect(string = colnames(df), pattern = "^(nu_)")]|> 
    str_remove_all("^(nu_)") |> 
    str_remove_all("(_min)|(_max)$") |> 
    unique()
  
  plots <- list()
  
  for (i in 1:length(ropes)){
  
    # column names of the two columns for the min and max values
  col <- df |> 
    select(contains(ropes[i])) |> 
    colnames()
    
    # get name of rope for subtitle  
  rope_subtitle <- stringr::str_to_title(ropes[i])
  
  plots[[i]] <- df |> 
    plot_base()+
    geom_ribbon(aes(x = time , 
                    ymin = .data[[col[1]]],
                    ymax = .data[[col[2]]]), 
                alpha = fill_alpha, 
                fill = fill_colours[2], 
                colour = fill_colours[2])+
    labs(subtitle = glue::glue("Lines at Bottom represent significance,
                            <b><span style='color:{sig_colours[1]};'>Raw p-values</b></span> and
                            <b><span style='color:{sig_colours[2]};'>Bonferroni Corrected </b></span><br>
                            Region of Practical Equivalance:
                            <b><span style='color:{fill_colours[2]};'>{rope_subtitle}</b></span>"))+
    theme_project_light()
  }
  
  return(plots)
}

