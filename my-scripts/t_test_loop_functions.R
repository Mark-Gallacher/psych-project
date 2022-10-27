get_pvals <- function(sample_size, break_loop = TRUE, alpha = 0.05){
  
  len_n <- length(sample_size)
  empty_vector <- array(NA, dim = len_n)
  var <- "p.value"
  
  for (i in 1:len_n){
    empty_vector[i] <- t.test(
      rnorm( sample_size[i] ), 
      rnorm( sample_size[i] ), 
      alternative = "two.sided", 
      conf.level = 1 - alpha, 
      var.equal = TRUE)[[var]]
    
    if (empty_vector[i] < alpha & break_loop){
      break
    }
  }
  assign(x = "p_values_matrix", value = empty_vector, envir = .GlobalEnv)
  
}

get_mean_diff <- function(sample_size, 
                          alpha = 0.05,
                          break_loop = TRUE){
  
  len_n <- length(sample_size)
  empty_vector <- array(NA, dim = len_n)
  p <- "p.value"
  mean <- "estimate"
  
  for (i in 1:len_n){
    t_test <- t.test(
      rnorm( sample_size[i] ), 
      rnorm( sample_size[i] ), 
      alternative = "two.sided", 
      conf.level = 1 - alpha, 
      var.equal = TRUE)
  
    if (t_test[[p]] < alpha & break_loop){
      empty_vector[i] <- abs(t_test[[mean]][[1]] - t_test[[mean]][[2]])
      break
    }else{
      empty_vector[i] <- abs(t_test[[mean]][[1]] - t_test[[mean]][[2]])
    }
  }
  assign(x = "mean_diff_matrix", value = empty_vector, envir = .GlobalEnv)
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
