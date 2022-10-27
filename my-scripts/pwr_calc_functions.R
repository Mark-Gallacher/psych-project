## Functions to perform lots of power calculations, using the pwr package, then the output is formatted to be more informative

# After for-loop power calculations, the output needs to be organised, so there is a column with sample size, 
# and pivoted to make plotting the data easier.
tidy_output_with_n <- function(array,              # contains values that need tidied up
                               value_colname,      # new name for col containing values
                               name_colname,       # new name for col containing names of values
                               col_vec,            # A vector used to iterate for the for-loop - in cols
                               row_vec             # A vector contain different sample sizes - in rows
                               ){
  
  library(tidyverse)
  
  ## Checking colnames are inside quotation marks and of length one
  ## NEXT STEP - Use tidy evaluation, is_quosure and {{}} to remove need for user to use ""
  
  if (is.character(value_colname) & (length(value_colname) == 1)){ 
    value_name <- value_colname
  }else{ 
    stop("value_colname should be a character of length 1, in quotation marks") 
    }
  
  if (is.character(name_colname) & (length(name_colname) == 1)){ 
    name_name <- name_colname
  }else{ 
    stop("name_colname should be a character of length 1, in quotation marks") 
    }
  colnames(array) <- col_vec
  
  output <- array |> 
    as_tibble() |> 
    mutate(n = row_vec) |> 
    pivot_longer(cols = -n, 
                 names_to = {{ name_name }}, 
                 values_to = {{ value_name }})
  
  return(output)
}

## A Basic For-loop to iterate over sample size and Effect size or Power, returns a 2D array containing values. 
## Rows represent sample size while columns are the other variable iterated over
## 
## NEXT STEP - pwr.t.test can handle a vector as well as individual values, 
## might be quicker to use map() and supply a vector.
get_power_or_effect <- function(col_vec,           # A vector containing values representing cohen's d or power
                                row_vec,           # A vector contain values representing different sample sizes
                                alpha = 0.05,      # Declaring the alpha level (pwr.t.test will return error if not between 0 and 1)
                                get_power = TRUE   # Stating if the col_vec represents power or cohen's d
                                ){
  library(tidyverse)
  library(pwr)
  
  len_row <- length(row_vec)
  len_col <- length(col_vec)
  
  # Empty array to contain the values
  output <- array(NA, dim = c(len_row, len_col))
  
  if(get_power == TRUE){
    col_vec_1 = col_vec
    col_vec_2 = NULL
    result = 4 # power is the 4th output from pwr.t.test
  }else{
    col_vec_1 = NULL
    col_vec_2 = col_vec
    result = 2 # cohen's d is the 2nd output from pwr.t.test
  }
  
  for(n in 1:len_row){
    for(i in 1:len_col){
      ## Allows on to be NULL with the other can be iterated over
      d = col_vec_1[i]    
      power = col_vec_2[i]
      
      output[n, i] <- pwr.t.test(sig.level = alpha, 
                                 d = d, 
                                 n = row_vec[n], 
                                 power = power, 
                                 type="paired",
                                 alternative="two.sided")[[result]]
    }
  }
  return(output)  
}


get_pwr_or_eff_dyn_alpha <- function(col_vec,           # A vector containing values representing cohen's d or power
                                     row_vec,           # A vector contain values representing different sample sizes
                                     alpha_vec,         # A matrix of same length as row_vsc x col_vec 
                                     get_power = TRUE   # Stating if the col_vec represents power or cohen's d
){
  library(tidyverse)
  library(pwr)
  
  if (length(alpha_vec) < 2){
    stop("Alpha needs to be a matrix, ensure there are no values less than zero or greater than one")
  }
  
  len_row <- length(row_vec)
  len_col <- length(col_vec)
  
  # Empty array to contain the values
  output <- array(NA, dim = c(len_row, len_col))
  
  if(get_power == TRUE){
    col_vec_1 = col_vec
    col_vec_2 = NULL
    result = 4 # power is the 4th output from pwr.t.test
  }else{
    col_vec_1 = NULL
    col_vec_2 = col_vec
    result = 2 # cohen's d is the 2nd output from pwr.t.test
  }
  
  for(n in 1:len_row){
    for(i in 1:len_col){
      ## Allows on to be NULL with the other can be iterated over
      d = col_vec_1[i]    
      power = col_vec_2[i]
      alpha = as.numeric(alpha_vec[n,i])
      
      output[n, i] <- pwr.t.test(sig.level = alpha, 
                                 d = d, 
                                 n = row_vec[n], 
                                 power = power, 
                                 type="paired",
                                 alternative="two.sided")[[result]]
    }
  }
  return(output)  
}


## To keep the notebook cleaner, this function combines the two other function, as get_power_of_effect() 
## created a 2d array that is supplied to the tidy_output_with_n(), which names the columns, 
## adds a n column and pivots data to a clearer format
##
## Info about the input variables can be found in above functions
get_tidy_power_or_effect <- function(col_vec, 
                                     row_vec, 
                                     value_colname, 
                                     name_colname, 
                                     get_power = TRUE){
  library(tidyverse)
  library(pwr)
  
  output <-  get_power_or_effect(col_vec = col_vec,
                                 row_vec = row_vec,
                                 get_power = get_power) |> 
              tidy_output_with_n(col_vec = col_vec, 
                                 row_vec = row_vec, 
                                 value_colname = value_colname,
                                 name_colname = name_colname)
  
  return(output)
}