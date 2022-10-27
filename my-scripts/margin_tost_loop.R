sample = c(25,50, 100, 500, 1000)
mn = 100 # mean of both groups set to 100
d = c(0:100)/10 # smaller increments for smoother graph
sd = 10
alpha = 0.05
margin = seq(0.05, 0.5, 0.05)

p_val_loop_dat = list()


# loop to get p values for different margins for different differences at different sample sizes
for(m in 1:length(margin)){
  
  p_val_loop_dat[[m]] <- get_pval_tost_loop(sample_vec = sample, 
                     row_vec = d, 
                     margin = margin[m], 
                     mean = mn , 
                     sd = sd, 
                     alpha = alpha, 
                     eqbound_type = "SMD") |> 
    tidy_tost_loop(diff_vec = d) |> 
    mutate("margin" = as.factor(margin[m]))

}

test <- p_val_loop_dat |> 
  bind_rows()
