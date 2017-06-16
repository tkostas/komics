# calculate exponential base^x
calc_exp_vec <- function(x, base = 2) {
  y <- base^x
  return(as.numeric(y))
}


#example

 # x <- 4
 # base <- 2
 #
 # calc_exp_vec(x, base)
 #
 # v <- seq(1:5)
 # calc_exp_vec(v)
