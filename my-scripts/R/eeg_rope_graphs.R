library(tidyverse)

## theme for the plots
source(here::here("my-scripts", "R", "project_themes.R"))
# set theme to project_theme
base_size = 12
theme_set(theme_project_light(base_size = base_size))


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
# Raw - Bonferroni - Null ROPE - Alternative ROPE (in that order)

sig_colours <-  c("#F8766D","purple3","#2f357c", "darkgreen")

# grey for ci range - fill
grey = "azure4"

fill_colours = c("#00BFC4", "#F8766D")

# variables for the main liines
line_size = 1
line_alpha = .8
# alpha for the ci range fill
fill_alpha = .2


subtitle_for_ropes <- list(
  
  "prestim" = "90% Quantile of Cond 1 and 3 Before Stim. Onset",
  "control" = "90% Quantile of Cond 1 and 2 in ROI", 
  "controlt" = "T-test between Cond 1 and 2 in ROI (0 +/- t-statistic)",
  "nonsig" = "90% Quantile of Non-sig. Values between Cond 1 and 2",
  "t" = "90% Quantile of T-values between Cond 1 and 3 in ROI",
  "static_3" = "Static ROPE at +/- 0.3 raw units",
  "static_2" = "Static ROPE at +/- 0.2 raw units",
  "static_1" = "Static ROPE at +/- 0.1 raw units"
  
)




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
# #### OLD plot_base function - keeping it just incase we hit problem, then will be removed
# plot_base <- function(df){
#   
#   p <- df |> 
#     ggplot(aes(x = time)) +
#     geom_line(aes(y = mn_cond1), colour = line_colours[1], alpha = line_alpha, size = line_size) +
#     geom_line(aes(y = mn_cond2), colour = line_colours[2], alpha = line_alpha, size = line_size) +
#     geom_line(aes(y = mn_cond3), colour = line_colours[3],alpha = line_alpha, size = line_size) +
#     geom_vline(xintercept = 0, colour = grey)+
#     geom_hline(yintercept = 0, colour = grey)+
#     geom_line(aes(y = raw_13_sig - 0.1), size = line_size, colour = sig_colours[1])+
#     geom_line(aes(y = bon_13_sig), size = line_size, colour = sig_colours[2])+
#     # geom_ribbon(aes(ymax = ci_u, ymin = ci_l), alpha = fill_alpha, fill = grey)+
#     facet_wrap(~n_trial, scale = "free_y")+
#     scale_x_continuous("Time (ms)")+
#     scale_y_continuous("")+
#     labs(title = glue::glue(
#       "<b>ERP Mean Values across Time with 
#          <span style='color:{grey};'>95% Confidence Intervals</span> for Various Sample Sizes</b><br>"))
#   
#   
#   return(p)
# }



plot_base <- function(df){
  
  p <- df |> 
    ggplot(aes(x = time, y = mn, colour = cond)) +
    geom_line(alpha = line_alpha, size = line_size)+
    geom_vline(xintercept = 0, colour = grey)+
    geom_hline(yintercept = 0, colour = grey)+
    facet_wrap(~n_trial, scale = "free_y")+
    scale_x_continuous("Time (ms)")+
    scale_y_continuous("")+
    # MetBrewer::scale_color_met_c(name = "Java")+
    labs(title = "ERP Values of Three Conditions, at Various Sample Sizes", 
         colour = "Condition")+
    scale_colour_manual(values = MetBrewer::met.brewer("Austria", 3),
                        labels = c("1", "2", "3"), 
                        guide = guide_legend(override.aes = list(linewidth = 2, shape = NA), order = 1))
  
  return(p)
}

plot_sim_base <- function(df, rope_df = NULL, replication = F){
  
  p <- df |> 
    pivot_for_mn(replication = replication) |> 
    plot_base()
  
  if(!is.null(rope_df)){
  p <- p +
    geom_point(data = rope_df, aes(x = time, y = raw_sig - 1.1, fill = '1'), 
               size = line_size, colour = sig_colours[1], inherit.aes = F, stroke = NA, shape = 22, show.legend = T)+
    geom_point(data = rope_df, aes(x = time, y = bon_sig - 1, fill = '2'), 
               size = line_size, colour = sig_colours[2], inherit.aes = F, stroke = NA, shape = 22, show.legend = T)+
    scale_fill_manual("Significance", 
                      values = sig_colours[1:2], 
                      labels = c("Raw ", "Bonf. Corrected"),
                      guide = guide_legend(override.aes = list(size = 5, colour = NA), order = 2))
  }else{
    message("Argument 'rope_df' was empty - No lines of Significance were plotted")
  }
  return(list(p))
}

plot_rep_base <- function(df, rope_df = NULL, replication = T){
  
  col_labels <- c("Original", "Replication")
  names(col_labels) <- c("A", "B")
  row_labels <- str_c(unique(df$n_trial))
  names(row_labels) <- unique(df$n_trial)
  
  p <- df |>
    pivot_for_mn(replication = replication) |> 
    plot_base()+
    facet_grid(n_trial ~ experiment, 
               labeller = labeller(n_trial = row_labels, experiment = col_labels), 
               scales = "free_y")
  
  if(!is.null(rope_df)){
    ropes <- unique(rope_df$rope)
    p <- p + 
      geom_point(data = subset(rope_df, rope == ropes[[1]]), 
               aes(x = time, y = raw_sig - 1.2, fill = '1'), 
               size = line_size, inherit.aes = F, alpha = line_alpha, stroke = NA, shape = 22, show.legend = T)+
      geom_point(data = subset(rope_df, rope == ropes[[1]]), 
                 aes(x = time, y = bon_sig - 1, fill = '2'), 
                 size = line_size, inherit.aes = F, alpha = line_alpha, stroke = NA, shape = 22, show.legend = T)+
      scale_fill_manual("Significance", 
                        values = sig_colours[1:2], 
                        labels = c("Raw ", "Bonf."),
                        guide = guide_legend(override.aes = list(size = 5, colour = NA), order = 2))
  }else{
    message("Argument 'rope_df' was empty - No lines of Significance were plotted")
  }
  return(list(p))
}

plot_rope <- function(p, df, null_rope = T, change_subtitle = F, replication = F){
  
  # getting unique ropes, used to get max and min columns later
  ropes <- unique(df$rope)
  subs_ropes <- subtitle_for_ropes[ropes]
  plots <- list()
  
  rope_label = c("Raw ", "Bonf.","Null ROPE")
  
  if(null_rope){
    ribbon_fill = "black"
    point_fill = sig_colours[1:3]
    fill_group = '3'
    title <- "Applying a ROPE to Achieve Statistical Consistency"
    }else{
    ribbon_fill = "#28E2E5"
    point_fill = sig_colours
    fill_group = '4'
    rope_label = c(rope_label, "Alternative ROPE")
    title <- "Applying an Alternative ROPE to Replicate ERP Effects"
    gap <- 0.8
    }
  
  if(replication){
    
    if(null_rope == FALSE){
      gap <- 0.6
    }else{
      gap <- 0.8
    }
    
  }else{
    gap <- 0.9
  }
  
  for (g in 1:length(p)) {
    if(change_subtitle){
    null_rope <- p[[g]]$labels$subtitle |> 
      str_c() |> 
      str_remove_all("(<b><span>)|(</b></span>)|(Region of Practical Equivalence: )")
    }
    for (i in 1:length(ropes)){
      # get name of rope for subtitle 
      if(change_subtitle){
        ## Subtitle for Null and Alternative ROPE
        rope_subtitle <- c(stringr::str_c("Null: ", null_rope),stringr::str_c("Alternative: ", subs_ropes[i]))
        full_subtitle <- quote(glue::glue("Region of Practical Equivalence: <b><span>{rope_subtitle[1]}</b></span> and<br>
                                 <b><span>{rope_subtitle[2]}</b></span>"))
        }else{
        ## Subtile for just Null ROPE
          rope_subtitle <- subtitle_for_ropes[ropes][[i]]
          full_subtitle <- quote(glue::glue("Region of Practical Equivalence: <b><span>{rope_subtitle}</b></span>"))
        } 
      
      if(replication == T){

        df_rope <- subset(df, rope == ropes[[i]] & experiment == "B")

      }else{

        df_rope <- subset(df, rope == ropes[[i]])

        }

      # df_rope <- subset(df, rope == ropes[[i]])
      
      plots[[length(ropes) * (g - 1) + i]] <- p[[g]] + 
        geom_ribbon(data = df_rope, 
                    aes(x = time, ymin = min, ymax = max), 
                                  alpha = fill_alpha, 
                                  fill = ribbon_fill, 
                                  colour = NA, 
                                  inherit.aes = F)+
        geom_point(data = df_rope, 
                   aes(x = time, y = sig_rope - gap, fill = fill_group), 
                   inherit.aes = F, 
                   size = line_size, 
                   alpha = line_alpha,
                   shape = 22, 
                   stroke = NA) +
        labs(
          title = glue::glue({title}),
          subtitle = eval(full_subtitle))+
        scale_fill_manual("Significance", 
                          values = point_fill, 
                          labels = rope_label,
                          guide = guide_legend(override.aes = list(size = 5, colour = NA)))+
        theme_project_light(base_size = base_size)
    }
  }
  
  return(plots)
}

