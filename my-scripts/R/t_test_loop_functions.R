
# Might be Better to use get_mean_diff, as the output is more detailed
# We can do more things with the data from it
# 
#  PLEASE NOTE ALL THE SAMPLES INCREMENT IN 5s by default 

increment <-  5


get_pvals <- function(sample_size, break_loop = TRUE, alpha = 0.05){
  library(tidyverse)
  
  len_n <- length(sample_size)
  p_val_vector <- array(NA, dim = len_n)
  var <- "p.value"
  g1_sample <- c()
  g2_sample <- c()
  
  for (i in 1:len_n){
    g1_sample <- c(g1_sample, rnorm(increment, mean = 0))
    g2_sample <- c(g2_sample, rnorm(increment, mean = 0 + mean_diff))
    
    t_test <- t.test(
      g1_sample, 
      g2_sample, 
      alternative = "two.sided", 
      conf.level = 1 - alpha, 
      var.equal = TRUE)
    
    if (p_val_vector[i] < alpha & break_loop){
      break
    }
  }
  return(p_val_vector)
}

get_pvals_one_sample <- function(sample_size, break_loop = TRUE, alpha = 0.05){
  library(tidyverse)
  
  len_n <- length(sample_size)
  p_val_vector <- array(NA, dim = len_n)
  var <- "p.value"
  
  sample <- c()
  
  for (i in 1:len_n){
    sample <- c(sample, rnorm(increment))
    
    p_val_vector[i] <- t.test(
      sample, 
      alternative = "two.sided", 
      conf.level = 1 - alpha, 
      var.equal = TRUE)[[var]]
    
    if (p_val_vector[i] < alpha & break_loop){
      break
    }
  }
  return(p_val_vector)
}

# More complex function can stop at significant results, 
# use a ROPE and define a rope
get_mean_diff <- function(sample_size, 
                          alpha = 0.05,
                          break_loop = TRUE, 
                          mean_diff = 0, 
                          use_rope = FALSE, 
                          margin = 0){
  library(tidyverse)
  
  len_n <- length(sample_size)
  
  mean_diff_vector <- array(NA, dim = len_n)
  p_vector <- array(NA, dim = len_n)
  conf_vector <- array(NA, dim = c(len_n, 2))
 
  p <- "p.value"
  mean <- "estimate"
  ci <- "conf.int"
  
  # empty lists for samples
  g1_sample = c()
  g2_sample = c()
  
  for (i in 1:len_n){
    ## Sample for group 1 and 2
    ## Increment of 5s
    g1_sample <- c(g1_sample, rnorm(increment, mean = 0))
    g2_sample <- c(g2_sample, rnorm(increment, mean = 0 + mean_diff))
    
    t_test <- t.test(
      g1_sample, 
      g2_sample, 
      alternative = "two.sided", 
      conf.level = 1 - alpha, 
      var.equal = TRUE)
  
    if (t_test[[p]] < alpha & break_loop){
      mean_diff <- abs(t_test[[mean]][[1]] - t_test[[mean]][[2]])
      
      if(use_rope){ # check if we are using a rope
        
        if(
          (t_test[[ci]][[1]] > margin) | (t_test[[ci]][[2]] < -margin)
          ){ # check if difference is bigger than rope
          ## If lower ci is above or upper ci is below, then we are completely outside rope
          
        p_vector[i] <- t_test[[p]]
        mean_diff_vector[i] <- mean_diff
        conf_vector[i, 1] <- t_test[[ci]][[1]]
        conf_vector[i, 2] <- t_test[[ci]][[2]]
        break
        }else{
          next 
        }
      }else{
        p_vector[i] <- t_test[[p]]
        mean_diff_vector[i] <- mean_diff
        conf_vector[i, 1] <- t_test[[ci]][[1]]
        conf_vector[i, 2] <- t_test[[ci]][[2]]
        break
        }
    }
    if (!break_loop){
      p_vector[i] <- t_test[[p]]
      mean_diff_vector[i] <- abs(t_test[[mean]][[1]] - t_test[[mean]][[2]])
      conf_vector[i, 1] <- t_test[[ci]][[1]]
      conf_vector[i, 2] <- t_test[[ci]][[2]]
    }
  }
  
 output <- mean_diff_vector |>
    bind_cols(p_vector, conf_vector, .name_repair = ~ c("mean_diff","p", "l_ci", "u_ci")) 
 
  return(output)
}

ttest_fast_loop <- function(sample_size, 
                          alpha = 0.05, 
                          mean_diff = 0){
  
  len_n <- length(sample_size)
  
  p <- "p.value"
  mean <- "estimate"
  ci <- "conf.int"
  
  s1 <- s2 <- c()
  
  output <-  foreach::foreach (s = 1:len_n, .combine = "rbind") %dopar%{
    # generate sample
    s1 = c(s1, rnorm(increment, mean = 0, sd = 1))
    s2 = c(s2, rnorm(increment, mean = 0 + mean_diff, sd = 1))
    
    # t test between two samples
    t_test <- t.test(s1, s2, 
                     alternative = "two.sided", 
                     conf.level = 1 - alpha, 
                     var.equal = FALSE)
    
    # finding the mean diff
    mean_diff <- abs(t_test[[mean]][[1]] - t_test[[mean]][[2]])
    
    # combining output
    c(alpha, sample_size[s], t_test[[p]], mean_diff, t_test[[ci]][[1]], t_test[[ci]][[2]]) 
    
  }
  
  output |> 
    tibble::as_tibble(.name_repair = ~ c("alpha", "n", "p", "mean_diff", "l_ci", "u_ci"))
}


summarise_t_test_loop <- function(df, n, repeats){
  
  result <- df |> 
    mutate(n = n*2) |> 
    pivot_longer(cols = -n, 
                 values_to = "pval", 
                 names_to = "set") |> 
    group_by(n)  |> 
    na.omit() |>
    summarise(count = n(),
              prop = count /repeats)
  
  return(result)
  
}


bf_ttest_fast_loop <- function(sample_size, mean_diff = 0, nullInterval = NULL){
  
  len_n <- length(sample_size)
  
  output <-  foreach::foreach (s = 1:len_n, .combine = "rbind") %dopar%{
    # generate sample
    s1 = c(s1, rnorm(increment, mean = 0, sd = 1))
    s2 = c(s2, rnorm(increment, mean = 0 + mean_diff, sd = 1))
    
    # t test between two samples
    bf <- BayesFactor::ttestBF(s1, s2, nullInterval = nullInterval)
    
    # finding the mean diff
    mean_diff <- mean(s1) - mean(s2)
    
    # combining output
    c(sample_size[s], as.vector(bf)[[1]],  mean_diff) 
    
  }
  
  output |> 
    tibble::as_tibble(.name_repair = ~ c("n", "bf", "mean_diff"))
}
#################################
#################################
#################################
# Practise Code + Examples
# library(tidyverse)
# 
# 
# n  <- c(n = seq(5,5000,5))
# a <- c(alpha = c(0.05, 0.01, 0.005, 0.001, 0.0005))
# repeats <- 10
# 
# t <- expand_grid(n, a)
# pvals_test <- replicate(
#   n = repeats,
#   expr = map2(.x = t$n, .y = t$a,
#               .f = ~ get_pvals(n = .x, 
#                                alpha = .y, 
#                                break_loop = FALSE))
#   ) |> 
#   as_tibble(.name_repair = ~ str_c("p.value_", 1:repeats))
# 
# mean_dff_test <- replicate(
#   n = repeats,
#   expr = map2(.x = t$n, .y = t$a,
#               .f = ~ get_mean_diff(n = .x, 
#                                alpha = .y, 
#                                break_loop = FALSE))
# ) |> 
#   as_tibble(.name_repair = ~ str_c("mean_diff_", 1:repeats))

#################################
#################################
#################################

# Old Function - Slow and Not efficient
# t.test.loop <- function(n, 
# empty_vector, 
# break_loop = TRUE, 
# alpha = 0.05,
# get_pval = TRUE){
#   
#   len_n <- length(n)
#   
#   
#   if (get_pval){
#     
#     for (i in 1:len_n){
#       
#       t <- t.test(rnorm(n[i]), rnorm(n[i]), alternative = "two.sided", conf.level = 1 - alpha, var.equal = TRUE)
#       p_index <- grep(x = names(t), pattern = "p.value")
#       empty_vector[i] <- t[[p_index]]
#       
#       if (empty_vector[i] < alpha & break_loop){
#         break
#       }
#     }
#     
#     assign(x = "p_values", value = empty_vector)
#     
#   }else{
#     
#     for (i in 1:len_n){
#       
#       t <- t.test(rnorm(n[i]), rnorm(n[i]), alternative = "two.sided", conf.level = 1 - alpha, var.equal = TRUE)
#       p_index <- grep(x = names(t), pattern = "p.value")
#       mn_index <- grep(x = names(t), pattern = "estimate")
#       
#       if (t[[p_index]] < alpha){
#         
#         empty_vector[i] <- t[[mn_index]][[1]] - t[[mn_index]][[2]]
#         if (break_loop){
#           break
#         }
#       }
#     }
#     
#     assign(x = "mean_diff", value = empty_vector)
#     
#   }
#   
# }
