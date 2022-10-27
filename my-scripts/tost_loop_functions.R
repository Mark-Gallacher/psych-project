get_pval_tost_loop <- function(sample_vec, 
                               row_vec, 
                               margin, 
                               mean, 
                               sd, 
                               alpha,
                               eqbound_type = "raw"
                               ){

result <- array(NA, dim = c(length(row_vec), 3*length(sample_vec)))

for(n in 1:length(sample_vec)){
  for(c in 1:length(row_vec)){
    output <- suppressMessages(
      expr = TOSTER::tsum_TOST(
      n1 = sample_vec[n], n2 = sample_vec[n],
      m1 = mean, m2 = mean+row_vec[c], 
      sd1 = sd, sd2 = sd, 
      r12 = 0, # no correlation, so we get a simplified cohen's d
      hypothesis = "EQU",
      low_eqbound = -margin,
      high_eqbound = margin,
      alpha = alpha,
      eqbound_type = eqbound_type, 
      bias_correction = FALSE, 
      paired = TRUE)
      )
    
    result[c, 3*n-2] <- output$TOST$p.value[1] # t test
    result[c, 3*n-1] <- output$TOST$p.value[2] # lower 
    result[c, 3*n] <- output$TOST$p.value[3]   # upper
  }
}
# TOSTER::tsum_TOST() produces message at each call, just repeating it once to make it cleaner

if (eqbound_type == "SMD"){
  warning("Caution: Margin Uses Cohen's d, Lakens (2017) Recommends using Raw Values (eqbound_type = 'raw')\n")
}

## Tidying output by renaming columns
result <- result |> 
  as_tibble(.name_repair = ~ str_c(
    rep(c("t_", "lower_", "upper_"), times = length(sample_vec)),
    rep(sample_vec, each = 3)
    )
    )
return(result)
}

tidy_tost_loop <- function(df, diff_vec){
  
 result <-  df |> 
    mutate(diff = diff_vec) |> 
    pivot_longer(cols = -diff,
                 names_to = "t", 
                 values_to = "p") |> 
    mutate(n = str_extract(t, pattern = "[0-9]+"),
           t = str_extract(t, pattern = "[a-z]+")
    )
 
 return(result)
}

##################################################
##################################################
##################################################

