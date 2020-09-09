# custom functions ####################################################
# produce formatted mean and standard deviation of vector
mean_sd <- function(x){
  # mean
  m <- mean(x, na.rm = T) %>% round(1)
  
  # standard deviation
  s <- sd(x, na.rm = T) %>% round(1)
  
  # return formatted product
  return(paste0(m, " (", s, ")"))
}

# function that produces sum and percent of binary vector input
sum_n_percent <- function(x){
  s <- sum(x, na.rm = T)
  
  per <- percent(sum(x, na.rm = T)/length(x))
  
  return(paste0(s, " (", per, ")"))
}
#########################################################################